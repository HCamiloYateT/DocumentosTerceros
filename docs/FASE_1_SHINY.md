# Fase 1 — Aplicación administrativa Shiny

## Alcance implementado

La primera fase crea la estructura ejecutable de la aplicación administrativa siguiendo `PlantillaBs4Dash` y la
organización de carpetas de `CRM_Nacional`.

Incluye:

- tablero inicial con total de solicitudes, enviadas, completas y vencidas;
- creación o reutilización de terceros por tipo y número de identificación;
- creación de solicitudes de vinculación o actualización;
- selección inicial de requisitos RUT, CERL y Habeas Data;
- registro de responsable, creador, fecha de creación, envío y vigencia;
- tabla de seguimiento con estado y porcentaje de completitud;
- expediente privado y versionado por solicitud;
- índices RDS para terceros, solicitudes y requisitos;
- bloqueo global y escritura atómica de RDS;
- manifiesto de artefactos con tamaño y hash SHA-256.

## Archivos obligatorios por requisito

Un requisito documental solo se considera completo cuando la misma versión contiene:

```text
documento.pdf
resumen.xlsx
```

Un requisito de formulario solo se considera completo cuando contiene:

```text
formulario.pdf
```

Las funciones `GuardarDocumentoSolicitud()` y `GuardarFormularioSolicitud()` aplican estas reglas, actualizan el
manifiesto y recalculan la completitud de la solicitud.

## Data frames de control

### `terceros.rds`

Contiene identificación, razón social, contacto, correo, tipo de tercero, ubicación, estado, creador y fechas.

### `solicitudes.rds`

Contiene código de solicitud y tercero, proceso, perfil, responsable, creador, fechas de creación/envío/vencimiento,
estado, total de requisitos, requisitos completos, porcentaje de completitud y última actividad.

### `requisitos.rds`

Contiene una fila por requisito y solicitud: tipo, nombre, obligatoriedad, firma, estado, versión vigente, ruta del
artefacto y fecha de actualización.

## Ejecución

Desde la raíz del repositorio:

```r
shiny::runApp("APP")
```

La ruta de datos puede configurarse fuera del repositorio:

```bash
export DOCUMENTOS_DATA_PATH=/ruta/privada/GestionTerceros
```

Si no se configura, la fase de desarrollo utiliza `APP/data`.

## Pendiente de validación

El entorno de trabajo usado para preparar esta fase no dispone de R. Se realizaron verificaciones estáticas de
estructura, diferencias Git, longitud de líneas y referencias de archivos, pero no se ejecutó la aplicación ni se
validaron las APIs instaladas de los paquetes Racafé. La primera prueba debe ejecutarse en un entorno con las
versiones mínimas declaradas en `global.R`.
