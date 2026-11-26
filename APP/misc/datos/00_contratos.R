# Contratos de datos ----
.TercerosVacio <- function() {
  tibble::tibble(
    CodigoTercero = character(), TipoPersona = character(), TipoIdentificacion = character(),
    NumeroIdentificacion = character(), DigitoVerificacion = character(), NombreRazonSocial = character(),
    NombreContacto = character(), CorreoContacto = character(), Telefono = character(), TipoTercero = character(),
    Pais = character(), Ciudad = character(), Observaciones = character(), Estado = character(),
    CreadoEn = as.POSIXct(character(), tz = "UTC"), CreadoPor = character(),
    ActualizadoEn = as.POSIXct(character(), tz = "UTC"), ActualizadoPor = character()
  )
}

.SolicitudesVacio <- function() {
  tibble::tibble(
    CodigoSolicitud = character(), CodigoTercero = character(), TipoProceso = character(), Perfil = character(),
    CreadoEn = as.POSIXct(character(), tz = "UTC"), CreadoPor = character(), CanalPrincipal = character(),
    EnviadoEn = as.POSIXct(character(), tz = "UTC"), VenceEn = as.POSIXct(character(), tz = "UTC"),
    AbiertoEn = as.POSIXct(character(), tz = "UTC"), TotalAperturas = integer(), Estado = character(),
    TotalRequisitos = integer(), RequisitosCompletos = integer(),
    PorcentajeCompletitud = numeric(), UltimaActividadEn = as.POSIXct(character(), tz = "UTC")
  )
}

.EventosVacio <- function() {
  tibble::tibble(
    CodigoSolicitud = character(), Canal = character(), Destinatario = character(), Asunto = character(),
    Estado = character(), Detalle = character(), RegistradoEn = as.POSIXct(character(), tz = "UTC"),
    RegistradoPor = character()
  )
}

.RequisitosVacio <- function() {
  tibble::tibble(
    CodigoSolicitud = character(), CodigoRequisito = character(), TipoRequisito = character(),
    NombreRequisito = character(), Obligatorio = logical(), RequiereFirma = logical(), Estado = character(),
    VersionVigente = integer(), RutaRelativa = character(), ActualizadoEn = as.POSIXct(character(), tz = "UTC")
  )
}

.ManifiestoVacio <- function() {
  tibble::tibble(
    CodigoSolicitud = character(), CodigoRequisito = character(), TipoArtefacto = character(), Version = integer(),
    RutaRelativa = character(), NombreArchivo = character(), TamanoBytes = numeric(), HashSHA256 = character(),
    Estado = character(), RegistradoEn = as.POSIXct(character(), tz = "UTC"), RegistradoPor = character()
  )
}

.ValidarColumnas <- function(datos, plantilla, nombre) {
  faltantes <- setdiff(names(plantilla), names(datos))
  if (length(faltantes)) stop(nombre, " no contiene las columnas: ", paste(faltantes, collapse = ", "), call. = FALSE)
  invisible(TRUE)
}
