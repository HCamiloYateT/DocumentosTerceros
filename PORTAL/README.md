# Portal público de documentos de terceros

Aplicación web externa a Posit Connect para que cada tercero atienda una solicitud mediante un enlace personal y
una validación OTP enviada al correo registrado.

## Alcance de esta entrega

- página pública adaptable a escritorio y celular;
- validación inicial del código de solicitud;
- interfaz de solicitud y verificación OTP sin bypass de demostración;
- resumen de tercero, solicitud, vigencia y avance;
- carga de RUT y certificado de existencia en PDF;
- acceso reservado para la autorización de tratamiento de datos y firma;
- finalización bloqueada hasta completar todos los requisitos;
- cabeceras HTTP de seguridad y token de sesión almacenado únicamente en `sessionStorage`.

Los endpoints de negocio todavía no están implementados. La interfaz presenta el error real de la API y no concede
acceso cuando el servicio OTP no está disponible.

## Ejecución en OEAPP

El servidor dispone de un entorno global de Python 3.10 con las dependencias necesarias. No se deben crear entornos
virtuales ni instalar paquetes desde este proyecto. `requirements.txt` se conserva únicamente como manifiesto de
dependencias.

Verificación del entorno global:

```bash
/usr/local/bin/python3.10 -c "import fastapi, uvicorn; print(fastapi.__version__, uvicorn.__version__)"
```

Ejecución:

```bash
cd /home/compartido/APP/4_ControlInterno/DocumentosTerceros/PORTAL
DOCUMENTOS_PORTAL_ROOT_PATH="/documentosterceros" /usr/local/bin/python3.10 -m uvicorn app.main:app \
  --host 127.0.0.1 --port 8019
```

## Contrato API v1

Todas las rutas se publican debajo de `/documentosterceros/api`. Salvo la solicitud inicial del OTP, requieren:

```http
Authorization: Bearer <session_token>
```

### Solicitar OTP

`POST /api/acceso/otp/solicitar`

```json
{"solicitud":"SOL-2026-000001"}
```

Respuesta:

```json
{"message":"Código enviado al correo registrado.","retry_after_seconds":60}
```

### Verificar OTP

`POST /api/acceso/otp/verificar`

```json
{"solicitud":"SOL-2026-000001","codigo":"123456"}
```

Respuesta:

```json
{"session_token":"token-opaco-de-sesion","expires_in_seconds":900}
```

El OTP y el token se guardarán como hashes. Se requieren expiración, límite de intentos, espera entre reenvíos y
revocación al finalizar la solicitud.

### Consultar solicitud

`GET /api/solicitudes/{codigo}`

```json
{
  "codigo_solicitud":"SOL-2026-000001",
  "vence_en":"2026-12-03T15:30:00Z",
  "tercero":{
    "tipo_identificacion":"NIT",
    "numero_identificacion":"900123456",
    "nombre_razon_social":"Empresa Ejemplo S.A.S."
  },
  "requisitos":[
    {"codigo":"DOC_RUT","estado":"PENDIENTE"},
    {"codigo":"DOC_CERL","estado":"PENDIENTE"},
    {"codigo":"FORM_HABEAS","estado":"PENDIENTE"}
  ]
}
```

### Adjuntar documento

`POST /api/solicitudes/{codigo}/requisitos/{requisito}/archivo`

Carga `multipart/form-data` con el campo `archivo`. El servidor debe validar sesión, vigencia, requisito, MIME real,
extensión, tamaño, nombre seguro y análisis antivirus antes de escribir el expediente.

### Finalizar solicitud

`POST /api/solicitudes/{codigo}/finalizar`

Solo puede finalizar cuando todos los requisitos obligatorios estén completos. La operación debe ser idempotente y
registrarse en la auditoría de la solicitud.

## Integración pendiente

El portal Python no modificará directamente los RDS. La siguiente fase debe implementar una API interna que sea la
única escritora del expediente o migrar el contrato interoperable a JSON. Nginx expondrá
`/documentosterceros/`; el upstream definitivo solo debe activarse después de validar OTP, cargas, firma, auditoría
y límites de archivos.
