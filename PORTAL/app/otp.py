from __future__ import annotations

import hashlib
import hmac
import json
import os
import re
import secrets
import subprocess
import threading
import time
from pathlib import Path
from typing import Any

from fastapi import APIRouter, Header, HTTPException
from pydantic import BaseModel


REQUEST_PATTERN = re.compile(r"^SOL-[0-9]{4}-[0-9]{6}$")
DATA_PATH = Path(os.environ.get("DOCUMENTOS_DATA_PATH", Path(__file__).parents[2] / "APP" / "data")).resolve()
RSCRIPT = os.environ.get("DOCUMENTOS_RSCRIPT", "Rscript")
BRIDGE_PATH = Path(__file__).with_name("otp_bridge.R")
OTP_TTL_SECONDS = 5 * 60
OTP_RESEND_SECONDS = 60
OTP_MAX_ATTEMPTS = 5
OTP_MAX_SENDS_HOUR = 5
SESSION_TTL_SECONDS = 15 * 60
PBKDF2_ITERATIONS = 200_000
LOCK = threading.RLock()

router = APIRouter(prefix="/api")


class OtpRequest(BaseModel):
    solicitud: str


class OtpVerification(BaseModel):
    solicitud: str
    codigo: str


class BridgeError(RuntimeError):
    def __init__(self, code: str):
        super().__init__(code)
        self.code = code


def _request_code(value: str) -> str:
    code = str(value).strip().upper()
    if not REQUEST_PATTERN.fullmatch(code):
        raise HTTPException(status_code=400, detail="La solicitud indicada no es válida.")
    return code


def _access_dir(code: str) -> Path:
    base = (DATA_PATH / "solicitudes").resolve()
    target = (base / code / "acceso").resolve()
    try:
        target.relative_to(base)
    except ValueError as error:
        raise HTTPException(status_code=400, detail="La solicitud indicada no es válida.") from error
    return target


def _atomic_json(path: Path, data: dict[str, Any]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_name(f".{path.name}.{secrets.token_hex(8)}.tmp")
    descriptor = os.open(temporary, os.O_WRONLY | os.O_CREAT | os.O_EXCL, 0o600)
    try:
        with os.fdopen(descriptor, "w", encoding="utf-8") as stream:
            json.dump(data, stream, ensure_ascii=False, separators=(",", ":"))
            stream.flush()
            os.fsync(stream.fileno())
        os.replace(temporary, path)
    finally:
        if temporary.exists():
            temporary.unlink()


def _read_json(path: Path) -> dict[str, Any]:
    if not path.is_file():
        return {}
    try:
        with path.open("r", encoding="utf-8") as stream:
            data = json.load(stream)
    except (OSError, json.JSONDecodeError) as error:
        raise HTTPException(status_code=503, detail="No fue posible validar el acceso.") from error
    if not isinstance(data, dict):
        raise HTTPException(status_code=503, detail="No fue posible validar el acceso.")
    return data


def _audit(code: str, event: str) -> None:
    path = _access_dir(code) / "auditoria.jsonl"
    path.parent.mkdir(parents=True, exist_ok=True)
    descriptor = os.open(path, os.O_WRONLY | os.O_CREAT | os.O_APPEND, 0o600)
    record = json.dumps(
        {"evento": event, "registrado_en": int(time.time())},
        ensure_ascii=False,
        separators=(",", ":"),
    )
    with os.fdopen(descriptor, "a", encoding="utf-8") as stream:
        stream.write(record + "\n")


def _run_bridge(operation: str, *fields: str) -> dict[str, Any] | None:
    values = [operation, *(str(field) for field in fields)]
    if any("\n" in value or "\r" in value for value in values):
        raise BridgeError("INVALID_INPUT")
    try:
        result = subprocess.run(
            [RSCRIPT, str(BRIDGE_PATH)],
            input="\n".join(values) + "\n",
            text=True,
            capture_output=True,
            timeout=30,
            check=False,
            env=os.environ.copy(),
        )
    except (OSError, subprocess.TimeoutExpired) as error:
        raise BridgeError("BRIDGE_UNAVAILABLE") from error

    lines = result.stdout.splitlines()
    if result.returncode != 0:
        code = lines[1] if len(lines) >= 2 and lines[0] == "ERROR" else "BRIDGE_ERROR"
        raise BridgeError(code)
    if not lines:
        raise BridgeError("BRIDGE_ERROR")
    if lines[0] == "OK":
        return None
    if lines[0] == "JSON" and len(lines) >= 2:
        try:
            payload = json.loads(lines[1])
        except json.JSONDecodeError as error:
            raise BridgeError("BRIDGE_ERROR") from error
        if isinstance(payload, dict):
            return payload
    raise BridgeError("BRIDGE_ERROR")


def _bridge_http(error: BridgeError) -> HTTPException:
    if error.code == "REQUEST_NOT_AVAILABLE":
        return HTTPException(status_code=404, detail="La solicitud no está disponible.")
    if error.code == "EMAIL_FAILED":
        return HTTPException(status_code=503, detail="No fue posible enviar el código de verificación.")
    return HTTPException(status_code=503, detail="No fue posible validar la solicitud.")


def _hash_code(code: str, salt: str) -> str:
    return hashlib.pbkdf2_hmac(
        "sha256",
        code.encode("utf-8"),
        bytes.fromhex(salt),
        PBKDF2_ITERATIONS,
    ).hex()


def _otp_state(code: str) -> tuple[Path, dict[str, Any]]:
    path = _access_dir(code) / "otp.json"
    return path, _read_json(path)


def _retry_after(state: dict[str, Any], now: int) -> int:
    resend = int(state.get("reenvio_despues", 0) or 0)
    return max(resend - now, 0)


def _clean_history(state: dict[str, Any], now: int) -> list[int]:
    history = state.get("envios", [])
    if not isinstance(history, list):
        history = []
    return [int(value) for value in history if isinstance(value, (int, float)) and int(value) > now - 3600]


def _session_path(code: str, token: str) -> Path:
    digest = hashlib.sha256(token.encode("utf-8")).hexdigest()
    return _access_dir(code) / "sesiones" / f"{digest}.json"


def _clean_sessions(code: str, now: int) -> None:
    directory = _access_dir(code) / "sesiones"
    if not directory.is_dir():
        return
    for path in directory.glob("*.json"):
        data = _read_json(path)
        if int(data.get("expira_en", 0) or 0) <= now:
            path.unlink(missing_ok=True)


def _new_session(code: str, now: int) -> str:
    token = secrets.token_urlsafe(32)
    _clean_sessions(code, now)
    _atomic_json(
        _session_path(code, token),
        {"solicitud": code, "creada_en": now, "expira_en": now + SESSION_TTL_SECONDS},
    )
    return token


def _require_session(authorization: str | None, code: str) -> None:
    if not authorization or not authorization.startswith("Bearer "):
        raise HTTPException(status_code=401, detail="La sesión no es válida.")
    token = authorization[7:].strip()
    if not token:
        raise HTTPException(status_code=401, detail="La sesión no es válida.")
    path = _session_path(code, token)
    with LOCK:
        session = _read_json(path)
        now = int(time.time())
        if session.get("solicitud") != code or int(session.get("expira_en", 0) or 0) <= now:
            path.unlink(missing_ok=True)
            _audit(code, "SESION_RECHAZADA")
            raise HTTPException(status_code=401, detail="La sesión venció. Solicite un nuevo código.")


@router.post("/acceso/otp/solicitar")
def request_otp(data: OtpRequest) -> dict[str, Any]:
    code = _request_code(data.solicitud)
    now = int(time.time())
    with LOCK:
        path, state = _otp_state(code)
        history = _clean_history(state, now)
        wait = _retry_after(state, now)
        if wait > 0:
            raise HTTPException(
                status_code=429,
                detail="Espere antes de solicitar un nuevo código.",
                headers={"Retry-After": str(wait)},
            )
        if len(history) >= OTP_MAX_SENDS_HOUR:
            wait = max(history[0] + 3600 - now, 1)
            raise HTTPException(
                status_code=429,
                detail="Se alcanzó el límite de códigos enviados. Intente más tarde.",
                headers={"Retry-After": str(wait)},
            )

        otp = f"{secrets.randbelow(1_000_000):06d}"
        try:
            _run_bridge("otp_send", code, otp, str(OTP_TTL_SECONDS // 60))
        except BridgeError as error:
            raise _bridge_http(error) from error

        salt = secrets.token_hex(16)
        history.append(now)
        state = {
            "sal": salt,
            "codigo_hash": _hash_code(otp, salt),
            "emitido_en": now,
            "expira_en": now + OTP_TTL_SECONDS,
            "reenvio_despues": now + OTP_RESEND_SECONDS,
            "intentos_restantes": OTP_MAX_ATTEMPTS,
            "envios": history,
        }
        _atomic_json(path, state)
        _audit(code, "OTP_ENVIADO")

    return {
        "message": "Código enviado al correo registrado.",
        "retry_after_seconds": OTP_RESEND_SECONDS,
    }


@router.post("/acceso/otp/verificar")
def verify_otp(data: OtpVerification) -> dict[str, Any]:
    code = _request_code(data.solicitud)
    otp = str(data.codigo).strip()
    if not re.fullmatch(r"[0-9]{6}", otp):
        raise HTTPException(status_code=400, detail="Ingrese el código de seis dígitos.")

    now = int(time.time())
    with LOCK:
        path, state = _otp_state(code)
        if not state.get("codigo_hash") or not state.get("sal"):
            raise HTTPException(status_code=400, detail="Solicite un nuevo código de verificación.")
        if int(state.get("expira_en", 0) or 0) <= now:
            state.pop("codigo_hash", None)
            state.pop("sal", None)
            _atomic_json(path, state)
            _audit(code, "OTP_VENCIDO")
            raise HTTPException(status_code=400, detail="El código venció. Solicite uno nuevo.")

        attempts = int(state.get("intentos_restantes", 0) or 0)
        if attempts <= 0:
            raise HTTPException(status_code=429, detail="El código fue bloqueado. Solicite uno nuevo.")

        expected = str(state["codigo_hash"])
        received = _hash_code(otp, str(state["sal"]))
        if not hmac.compare_digest(received, expected):
            state["intentos_restantes"] = attempts - 1
            _atomic_json(path, state)
            _audit(code, "OTP_INCORRECTO")
            message = "Código incorrecto."
            if attempts - 1 <= 0:
                message = "El código fue bloqueado. Solicite uno nuevo."
            raise HTTPException(status_code=400, detail=message)

        try:
            _run_bridge("request", code)
        except BridgeError as error:
            raise _bridge_http(error) from error

        state.pop("codigo_hash", None)
        state.pop("sal", None)
        state["verificado_en"] = now
        _atomic_json(path, state)
        token = _new_session(code, now)
        _audit(code, "OTP_VERIFICADO")

    return {"session_token": token, "expires_in_seconds": SESSION_TTL_SECONDS}


@router.get("/solicitudes/{codigo}")
def request_detail(codigo: str, authorization: str | None = Header(default=None)) -> dict[str, Any]:
    code = _request_code(codigo)
    _require_session(authorization, code)
    try:
        payload = _run_bridge("request", code)
    except BridgeError as error:
        raise _bridge_http(error) from error
    if payload is None:
        raise HTTPException(status_code=503, detail="No fue posible consultar la solicitud.")
    return payload
