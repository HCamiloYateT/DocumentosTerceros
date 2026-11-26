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
                                           detalle = "", canal = "CORREO") {
  codigo_solicitud <- .ValidarCodigoRuta(codigo_solicitud, "código de solicitud")
  canal <- toupper(trimws(as.character(canal)[1L]))
  if (!canal %in% c("CORREO", "ENLACE", "QR", "SISTEMA")) {
    stop("Canal de comunicación no válido.", call. = FALSE)
  }
  ruta <- file.path(.RutaSolicitud(codigo_solicitud), "comunicaciones", "eventos.rds")
  .ConBloqueo(paste0("comunicacion_", codigo_solicitud), {
    eventos <- .LeerRDS(ruta, .EventosVacio)
    fila <- tibble::tibble(
      CodigoSolicitud = codigo_solicitud, Canal = canal,
      Destinatario = tolower(trimws(destinatario)), Asunto = asunto,
      Estado = estado, Detalle = detalle,
      RegistradoEn = as.POSIXct(Sys.time(), tz = "UTC"), RegistradoPor = usuario
    )
    .EscribirRDSAtomico(dplyr::bind_rows(eventos, fila), ruta)
  })
  invisible(codigo_solicitud)
}

ListarEventosSolicitud <- function(codigo_solicitud) {
  codigo_solicitud <- .ValidarCodigoRuta(codigo_solicitud, "código de solicitud")
  ruta <- file.path(.RutaSolicitud(codigo_solicitud), "comunicaciones", "eventos.rds")
  .LeerRDS(ruta, .EventosVacio)
}

ListarCronologiaSolicitud <- function(codigo_solicitud) {
  codigo_solicitud <- .ValidarCodigoRuta(codigo_solicitud, "código de solicitud")
  solicitudes <- ListarSolicitudes()
  solicitud <- solicitudes[match(codigo_solicitud, solicitudes$CodigoSolicitud), ]
  if (!nrow(solicitud) || is.na(solicitud$CodigoSolicitud)) stop("La solicitud no existe.", call. = FALSE)

  creacion <- tibble::tibble(
    Fecha = solicitud$CreadoEn, Evento = "CREADA", Canal = "SISTEMA",
    Detalle = "Solicitud creada.", RegistradoPor = solicitud$CreadoPor
  )
  eventos <- ListarEventosSolicitud(codigo_solicitud) %>%
    dplyr::transmute(Fecha = RegistradoEn, Evento = Estado, Canal, Detalle, RegistradoPor)
  vencida <- solicitud$VenceEn < as.POSIXct(Sys.time(), tz = "UTC") && solicitud$PorcentajeCompletitud < 100
  if (isTRUE(vencida)) {
    eventos <- dplyr::bind_rows(
      eventos,
      tibble::tibble(Fecha = solicitud$VenceEn, Evento = "VENCIDA", Canal = "SISTEMA",
                     Detalle = "Finalizó la vigencia del enlace.", RegistradoPor = "SISTEMA")
    )
  }
  dplyr::arrange(dplyr::bind_rows(creacion, eventos), Fecha)
}
