# Comunicaciones de solicitudes ----
ConstruirEnlaceSolicitud <- function(codigo_solicitud) {
  codigo_solicitud <- .ValidarCodigoRuta(codigo_solicitud, "código de solicitud")
  base <- Sys.getenv(
    "DOCUMENTOS_PUBLIC_BASE_URL",
    unset = "https://analitica.racafe.com/DocumentosTerceros/"
  )
  base <- sub("/+$", "", trimws(base))
  if (!grepl("^https://", base, ignore.case = TRUE)) {
    stop("DOCUMENTOS_PUBLIC_BASE_URL debe usar HTTPS.", call. = FALSE)
  }
  paste0(base, "/?solicitud=", utils::URLencode(codigo_solicitud, reserved = TRUE))
}

RegistrarComunicacionSolicitud <- function(codigo_solicitud, destinatario, asunto, estado, usuario,
                                           detalle = "") {
  codigo_solicitud <- .ValidarCodigoRuta(codigo_solicitud, "código de solicitud")
  ruta <- file.path(.RutaSolicitud(codigo_solicitud), "comunicaciones", "eventos.rds")
  .ConBloqueo(paste0("comunicacion_", codigo_solicitud), {
    eventos <- .LeerRDS(ruta, function() tibble::tibble(
      CodigoSolicitud = character(), Canal = character(), Destinatario = character(),
      Asunto = character(), Estado = character(), Detalle = character(),
      RegistradoEn = as.POSIXct(character(), tz = "UTC"), RegistradoPor = character()
    ))
    fila <- tibble::tibble(
      CodigoSolicitud = codigo_solicitud, Canal = "CORREO",
      Destinatario = tolower(trimws(destinatario)), Asunto = asunto,
      Estado = estado, Detalle = detalle,
      RegistradoEn = as.POSIXct(Sys.time(), tz = "UTC"), RegistradoPor = usuario
    )
    .EscribirRDSAtomico(dplyr::bind_rows(eventos, fila), ruta)
  })
  invisible(codigo_solicitud)
}
