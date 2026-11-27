from __future__ import annotations

import os
import tempfile
from datetime import datetime
from pathlib import Path
from typing import Any

from pypdf import PdfReader, PdfWriter


def resolve_template(data_path: Path, relative_path: str) -> Path:
    root = data_path.resolve()
    path = (root / relative_path).resolve()
    try:
        path.relative_to(root)
    except ValueError as error:
        raise ValueError("Ruta de plantilla no válida.") from error
    if not path.is_file():
        raise FileNotFoundError(path)
    return path


def fill_pdf(
    template: Path,
    output: Path,
    form: dict[str, Any],
    answers: dict[str, Any],
    third_party: dict[str, Any],
) -> None:
    fields = {
        str(form["campos_texto"]["nombre"]): str(third_party["nombre_razon_social"]),
        str(form["campos_texto"]["identificacion"]): (
            f'{third_party["tipo_identificacion"]} {third_party["numero_identificacion"]}'
        ),
        str(form["campos_texto"]["fecha"]): datetime.now().astimezone().strftime("%d/%m/%Y"),
    }
    for purpose in form["finalidades"]:
        value = str(answers.get(purpose["codigo"], "")).upper()
        fields[str(purpose["campo_si"])] = "/Yes" if value == "SI" else "/Off"
        fields[str(purpose["campo_no"])] = "/Yes" if value == "NO" else "/Off"

    reader = PdfReader(str(template))
    writer = PdfWriter()
    writer.clone_document_from_reader(reader)
    for page in writer.pages:
        writer.update_page_form_field_values(page, fields, auto_regenerate=False)

    output.parent.mkdir(parents=True, exist_ok=True)
    descriptor, temporary_name = tempfile.mkstemp(prefix=".formulario-", suffix=".pdf", dir=output.parent)
    os.close(descriptor)
    temporary = Path(temporary_name)
    try:
        with temporary.open("wb") as stream:
            writer.write(stream)
        os.replace(temporary, output)
    finally:
        temporary.unlink(missing_ok=True)
