# Persistencia local por solicitud

## 1. Decisión

La primera versión no utilizará MySQL. Los datos administrativos de Shiny se guardarán en RDS locales y cada
solicitud tendrá un expediente privado independiente con todos sus documentos y resultados.

La base anterior de Habeas Data se considera material de prueba y no se migrará. Su código sigue siendo una
referencia para token, OTP, firma, PDF, correo y evidencia.

## 2. Raíz de almacenamiento

La ruta se configurará con `DOCUMENTOS_DATA_PATH`. No debe ubicarse dentro de `www/`, el repositorio Git ni una
ruta expuesta por Shiny, Nginx o Posit Connect.

```text
data/
├── catalogos/
│   ├── tipos_documentos.rds
│   ├── formularios.rds
│   ├── perfiles_solicitud.rds
│   └── reglas_validacion.rds
├── indices/
│   ├── terceros.rds
│   ├── solicitudes.rds
│   └── trabajos.rds
├── bloqueos/
├── temporales/
└── solicitudes/
    └── SOL-2026-000001/
```

## 3. Expediente de una solicitud

```text
SOL-2026-000001/
├── solicitud.rds
├── requisitos.rds
├── manifiesto.rds
├── acceso/
│   ├── enlaces.rds
│   ├── otp.rds
│   └── sesiones.rds
├── documentos/
│   ├── RUT/
│   │   └── v001/
│   │       ├── documento.pdf
│   │       ├── archivo.rds
│   │       ├── extraccion.rds
│   │       ├── campos.rds
│   │       ├── validaciones.rds
│   │       └── evidencias/
│   └── CERL/
│       └── v001/
│           ├── documento.pdf
│           ├── archivo.rds
│           ├── extraccion.rds
│           ├── administradores.rds
│           ├── validaciones.rds
│           └── evidencias/
├── formularios/
│   └── HABEAS_DATA/
│       └── v001/
│           ├── respuestas.rds
│           ├── preliminar.pdf
│           ├── firmado.pdf
│           └── evidencia.rds
├── firmas/
│   └── ACTO-001/
│       ├── acto.rds
│       ├── firma.png
│       └── evidencia.pdf
├── comunicaciones/
│   └── eventos.rds
└── auditoria/
    └── eventos.rds
```

Los nombres de carpeta se generan en el servidor. No se usarán nombres de terceros, identificaciones ni nombres de
archivo aportados por el navegador como parte de una ruta.

## 4. Objetos principales

### `solicitud.rds`

Contendrá número, tercero, contacto, proceso, responsable, estado, fechas, instrucciones, versión del registro y
referencias a requisitos. Debe guardar una instantánea de los datos usados al crear la solicitud.

### `requisitos.rds`

Una fila por documento o formulario: código, tipo, obligatoriedad, necesidad de firma, orden, configuración,
estado técnico, estado de negocio, versión vigente y fechas.

### `manifiesto.rds`

Una fila por artefacto del expediente: ruta relativa, tipo, versión, tamaño, SHA-256, fecha, estado y origen. Permite
verificar integridad y construir el ZIP final sin recorrer archivos no registrados.

### Índices globales

Los índices contienen solamente la información necesaria para búsquedas y tableros. El expediente de la solicitud
es la fuente de detalle. Los índices pueden reconstruirse a partir de las carpetas y no deben contener documentos,
firmas ni texto OCR completo.

## 5. Reglas de escritura

1. Validar identificadores y rutas antes de leer o escribir.
2. Adquirir un bloqueo exclusivo por solicitud o índice.
3. Escribir el RDS en un archivo temporal dentro del mismo sistema de archivos.
4. Calcular el hash y validar que el archivo temporal pueda leerse.
5. Renombrar atómicamente el temporal sobre el destino.
6. Actualizar el manifiesto dentro del mismo bloqueo.
7. Liberar el bloqueo aunque ocurra un error.

No se editarán RDS simultáneamente desde observers distintos. Cada entidad tendrá un único reactive central y un
trigger de recarga.

## 6. Versionamiento

- Un archivo cargado nunca se sobrescribe: se crea `v001`, `v002`, etc.
- Una extracción referencia exactamente la versión del archivo procesado.
- Un resultado tardío no puede marcarse vigente si el documento fue reemplazado.
- Una modificación de formulario crea nueva versión de respuestas y PDF preliminar.
- Un PDF firmado es inmutable; si cambia el formulario se requiere otro acto de firma.
- Los originales, hashes y evidencias no se eliminan al solicitar correcciones.

## 7. Seguridad

- La raíz de datos será privada y accesible solo a las cuentas de servicio autorizadas.
- Tokens, OTP y sesiones se almacenarán como hashes; nunca en texto plano.
- La imagen de firma no se publicará como recurso estático.
- Las descargas se entregarán mediante una función autorizada que valide solicitud, sesión y artefacto.
- Los logs y auditorías evitarán datos personales, tokens, OTP y rutas absolutas.
- Los documentos se validarán por contenido real, tamaño, extensión permitida y antivirus cuando esté disponible.

## 8. Respaldo y recuperación

El respaldo debe incluir conjuntamente `catalogos`, `indices` y `solicitudes`. La recuperación validará hashes del
manifiesto y reconstruirá los índices globales. Los temporales y bloqueos no forman parte del respaldo.

Se requiere definir retención, cifrado del respaldo, periodicidad y responsable antes de producción.

## 9. Decisión pendiente de interoperabilidad

RDS es adecuado para Shiny y los trabajadores R del OCR, pero FastAPI no debe modificar objetos RDS complejos de
forma directa. Antes de implementar el portal público debe elegirse una opción:

1. portal y API en R, compartiendo la misma capa de persistencia;
2. FastAPI conservado como frontend/servicio y una API interna R como única escritora de RDS;
3. metadatos interoperables en JSON y RDS únicamente como índices derivados de Shiny.

No se implementará escritura concurrente de los mismos datos desde R y Python sin un contrato único.
