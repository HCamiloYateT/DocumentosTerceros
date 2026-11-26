# Constantes del módulo Solicitudes ----
.SOL_DOCUMENTOS <- c(
  "RUT" = "DOC_RUT",
  "Certificado de existencia y representación legal" = "DOC_CERL",
  "Autorización de tratamiento de datos" = "FORM_HABEAS"
)
.SOL_TIPOS_DOCUMENTO <- c("NIT", "CC", "CE", "PASAPORTE")
.SOL_COLUMNAS_MASIVAS <- c("Nombre", "TipoDocumento", "NumeroDocumento", "Correo")
.SOL_MAPA_DOCUMENTOS <- c(
  RUT = "DOC_RUT", CERL = "DOC_CERL", HABEAS = "FORM_HABEAS",
  HABEAS_DATA = "FORM_HABEAS", DOC_RUT = "DOC_RUT", DOC_CERL = "DOC_CERL",
  FORM_HABEAS = "FORM_HABEAS"
)
.SOL_MAX_ARCHIVO <- 10 * 1024^2
.SOL_MAX_FILAS <- 100L
