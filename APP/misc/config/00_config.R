# Configuración de persistencia ----
DOCUMENTOS_DATA_PATH <- Sys.getenv("DOCUMENTOS_DATA_PATH", file.path(.ruta_app, "data"))
DOCUMENTOS_DATA_PATH <- normalizePath(DOCUMENTOS_DATA_PATH, winslash = "/", mustWork = FALSE)

.RUTAS_DOCUMENTOS <- list(
  raiz = DOCUMENTOS_DATA_PATH,
  catalogos = file.path(DOCUMENTOS_DATA_PATH, "catalogos"),
  indices = file.path(DOCUMENTOS_DATA_PATH, "indices"),
  solicitudes = file.path(DOCUMENTOS_DATA_PATH, "solicitudes"),
  bloqueos = file.path(DOCUMENTOS_DATA_PATH, "bloqueos"),
  temporales = file.path(DOCUMENTOS_DATA_PATH, "temporales")
)

DOCUMENTOS_VIGENCIA_DIAS <- suppressWarnings(as.integer(Sys.getenv("DOCUMENTOS_VIGENCIA_DIAS", "7")))
if (is.na(DOCUMENTOS_VIGENCIA_DIAS) || DOCUMENTOS_VIGENCIA_DIAS < 1L) DOCUMENTOS_VIGENCIA_DIAS <- 7L
DOCUMENTOS_SCHEMA_VERSION <- 2L

.PRIORIDAD_CANALES <- c(ENLACE = 1L, QR = 2L, CORREO = 3L)

.REQUISITOS_BASE <- tibble::tribble(
  ~CodigoRequisito, ~TipoRequisito, ~NombreRequisito, ~Obligatorio, ~RequiereFirma,
  "DOC_RUT", "DOCUMENTO", "RUT", TRUE, FALSE,
  "DOC_CERL", "DOCUMENTO", "Certificado de existencia y representación legal", TRUE, FALSE,
  "FORM_HABEAS", "FORMULARIO", "Autorización de tratamiento de datos personales", TRUE, TRUE
)

.ESTADOS_SOLICITUD <- c("BORRADOR", "GENERADA", "ENVIADA", "EN_DILIGENCIAMIENTO", "RECIBIDA",
                        "EN_REVISION", "APROBADA", "RECHAZADA", "CORRECCION_SOLICITADA", "VENCIDA",
                        "CANCELADA")
