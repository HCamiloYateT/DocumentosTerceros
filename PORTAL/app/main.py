from pathlib import Path

from fastapi import FastAPI
from fastapi.responses import FileResponse, JSONResponse
from fastapi.staticfiles import StaticFiles


APP_DIR = Path(__file__).resolve().parent
STATIC_DIR = APP_DIR / "static"
app = FastAPI(
    title="Portal de documentos de terceros",
    docs_url=None,
    redoc_url=None,
    openapi_url=None,
)
app.mount("/static", StaticFiles(directory=STATIC_DIR), name="static")


@app.middleware("http")
async def security_headers(request, call_next):
    response = await call_next(request)
    response.headers["Cache-Control"] = "no-store"
    response.headers["X-Content-Type-Options"] = "nosniff"
    response.headers["X-Frame-Options"] = "DENY"
    response.headers["Referrer-Policy"] = "no-referrer"
    response.headers["Permissions-Policy"] = "camera=(), microphone=(), geolocation=()"
    response.headers["Content-Security-Policy"] = (
        "default-src 'self'; img-src 'self' data:; style-src 'self'; "
        "script-src 'self'; connect-src 'self'; frame-ancestors 'none'; base-uri 'self'; form-action 'self'"
    )
    return response


@app.get("/", include_in_schema=False)
def index():
    return FileResponse(STATIC_DIR / "index.html", media_type="text/html")


@app.get("/health", include_in_schema=False)
def health():
    return {"status": "ok", "application": "Portal de documentos de terceros"}


@app.get("/config", include_in_schema=False)
def config():
    return JSONResponse(
        {
            "otp_enabled": True,
            "api_contract": "v1",
            "max_file_size_mb": 15,
            "allowed_file_types": ["application/pdf"],
        }
    )
