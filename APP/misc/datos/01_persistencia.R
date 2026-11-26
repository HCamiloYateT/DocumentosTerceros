# Utilidades privadas ----
`%||%` <- function(x, y) {
  if (is.null(x) || !length(x) || all(is.na(x))) y else x
}

.ValidarCodigoRuta <- function(codigo, nombre = "código") {
  codigo <- trimws(as.character(codigo)[1L])
  if (!nzchar(codigo) || !grepl("^[A-Za-z0-9_-]+$", codigo)) {
    stop("El ", nombre, " contiene caracteres no permitidos.", call. = FALSE)
  }
  codigo
}

.RutaSolicitud <- function(codigo_solicitud) {
  codigo_solicitud <- .ValidarCodigoRuta(codigo_solicitud, "código de solicitud")
  file.path(.RUTAS_DOCUMENTOS$solicitudes, codigo_solicitud)
}

.ConBloqueo <- function(nombre, expresion, timeout = 15) {
  nombre <- .ValidarCodigoRuta(nombre, "nombre del bloqueo")
  ruta <- file.path(.RUTAS_DOCUMENTOS$bloqueos, paste0(nombre, ".lock"))
  inicio <- Sys.time()
  adquirido <- FALSE

  repeat {
    adquirido <- dir.create(ruta, showWarnings = FALSE, recursive = FALSE)
    if (adquirido) break
    if (as.numeric(difftime(Sys.time(), inicio, units = "secs")) >= timeout) {
      stop("No fue posible obtener el bloqueo de persistencia: ", nombre, call. = FALSE)
    }
    Sys.sleep(0.1)
  }

  on.exit(if (dir.exists(ruta)) unlink(ruta, recursive = TRUE, force = TRUE), add = TRUE)
  force(expresion)
}

.EscribirRDSAtomico <- function(objeto, ruta) {
  dir.create(dirname(ruta), recursive = TRUE, showWarnings = FALSE)
  temporal <- tempfile(pattern = ".rds-", tmpdir = dirname(ruta), fileext = ".tmp")
  on.exit(if (file.exists(temporal)) unlink(temporal, force = TRUE), add = TRUE)
  saveRDS(objeto, temporal, compress = "xz", version = 3)
  readRDS(temporal)

  if (!file.rename(temporal, ruta)) stop("No fue posible completar la escritura de ", basename(ruta), call. = FALSE)
  invisible(ruta)
}

.LeerRDS <- function(ruta, vacio) {
  if (!file.exists(ruta)) return(vacio())
  readRDS(ruta)
}

.HashArchivo <- function(ruta) {
  if (!requireNamespace("digest", quietly = TRUE)) stop("Se requiere el paquete digest.", call. = FALSE)
  digest::digest(file = ruta, algo = "sha256", serialize = FALSE)
}

.SiguienteCodigo <- function(codigos, prefijo, ancho = 6L) {
  patron <- paste0("^", prefijo, "-([0-9]+)$")
  numeros <- suppressWarnings(as.integer(sub(patron, "\\1", codigos[grepl(patron, codigos)])))
  siguiente <- if (length(numeros) && any(!is.na(numeros))) max(numeros, na.rm = TRUE) + 1L else 1L
  paste0(prefijo, "-", sprintf(paste0("%0", ancho, "d"), siguiente))
}

.SiguienteSolicitud <- function(codigos, fecha = Sys.Date()) {
  prefijo <- paste0("SOL-", format(fecha, "%Y"))
  .SiguienteCodigo(codigos, prefijo)
}

.SiguienteVersion <- function(ruta_requisito) {
  versiones <- list.dirs(ruta_requisito, recursive = FALSE, full.names = FALSE)
  numeros <- suppressWarnings(as.integer(sub("^v", "", versiones[grepl("^v[0-9]{3}$", versiones)])))
  if (length(numeros) && any(!is.na(numeros))) max(numeros, na.rm = TRUE) + 1L else 1L
}

.RutaRelativa <- function(ruta) {
  raiz <- paste0(normalizePath(.RUTAS_DOCUMENTOS$raiz, winslash = "/", mustWork = TRUE), "/")
  absoluta <- normalizePath(ruta, winslash = "/", mustWork = TRUE)
  if (!startsWith(absoluta, raiz)) stop("El archivo no pertenece al almacenamiento autorizado.", call. = FALSE)
  substring(absoluta, nchar(raiz) + 1L)
}

# Inicialización ----
InicializarPersistenciaDocumentos <- function() {
  sapply(.RUTAS_DOCUMENTOS, dir.create, recursive = TRUE, showWarnings = FALSE)

  indices <- list(
    terceros = list(ruta = file.path(.RUTAS_DOCUMENTOS$indices, "terceros.rds"), vacio = .TercerosVacio),
    solicitudes = list(ruta = file.path(.RUTAS_DOCUMENTOS$indices, "solicitudes.rds"), vacio = .SolicitudesVacio),
    requisitos = list(ruta = file.path(.RUTAS_DOCUMENTOS$indices, "requisitos.rds"), vacio = .RequisitosVacio)
  )

  lapply(indices, function(indice) {
    if (!file.exists(indice$ruta)) .EscribirRDSAtomico(indice$vacio(), indice$ruta)
  })
  invisible(.RUTAS_DOCUMENTOS)
}

# Consultas ----
ListarTerceros <- function() {
  datos <- .LeerRDS(file.path(.RUTAS_DOCUMENTOS$indices, "terceros.rds"), .TercerosVacio)
  .ValidarColumnas(datos, .TercerosVacio(), "El índice de terceros")
  datos
}

ListarSolicitudes <- function(actualizar_completitud = FALSE) {
  datos <- .LeerRDS(file.path(.RUTAS_DOCUMENTOS$indices, "solicitudes.rds"), .SolicitudesVacio)
  .ValidarColumnas(datos, .SolicitudesVacio(), "El índice de solicitudes")
  if (actualizar_completitud && nrow(datos)) {
    invisible(lapply(datos$CodigoSolicitud, ActualizarCompletitudSolicitud))
    datos <- .LeerRDS(file.path(.RUTAS_DOCUMENTOS$indices, "solicitudes.rds"), .SolicitudesVacio)
  }
  datos
}

ListarRequisitosSolicitudes <- function(codigo_solicitud = NULL) {
  datos <- .LeerRDS(file.path(.RUTAS_DOCUMENTOS$indices, "requisitos.rds"), .RequisitosVacio)
  .ValidarColumnas(datos, .RequisitosVacio(), "El índice de requisitos")
  if (!is.null(codigo_solicitud)) datos <- dplyr::filter(datos, CodigoSolicitud == codigo_solicitud)
  datos
}

# Terceros y solicitudes ----
CrearTercero <- function(datos, usuario) {
  usuario <- trimws(as.character(usuario)[1L])
  numero <- toupper(gsub("[^A-Za-z0-9]", "", datos$NumeroIdentificacion))
  if (!nzchar(numero)) stop("El número de identificación es obligatorio.", call. = FALSE)

  .ConBloqueo("persistencia", {
    terceros <- ListarTerceros()
    duplicado <- terceros$TipoIdentificacion == datos$TipoIdentificacion &
      toupper(gsub("[^A-Za-z0-9]", "", terceros$NumeroIdentificacion)) == numero
    if (any(duplicado)) return(terceros$CodigoTercero[which(duplicado)[1L]])

    ahora <- as.POSIXct(Sys.time(), tz = "UTC")
    codigo <- .SiguienteCodigo(terceros$CodigoTercero, "TER")
    fila <- tibble::tibble(
      CodigoTercero = codigo, TipoPersona = datos$TipoPersona, TipoIdentificacion = datos$TipoIdentificacion,
      NumeroIdentificacion = numero, DigitoVerificacion = datos$DigitoVerificacion %||% "",
      NombreRazonSocial = trimws(datos$NombreRazonSocial), NombreContacto = trimws(datos$NombreContacto),
      CorreoContacto = tolower(trimws(datos$CorreoContacto)), Telefono = trimws(datos$Telefono %||% ""),
      TipoTercero = datos$TipoTercero, Pais = datos$Pais, Ciudad = trimws(datos$Ciudad %||% ""),
      Observaciones = trimws(datos$Observaciones %||% ""), Estado = "ACTIVO", CreadoEn = ahora,
      CreadoPor = usuario, ActualizadoEn = ahora, ActualizadoPor = usuario
    )
    .EscribirRDSAtomico(dplyr::bind_rows(terceros, fila),
                        file.path(.RUTAS_DOCUMENTOS$indices, "terceros.rds"))
    codigo
  })
}

CrearSolicitud <- function(codigo_tercero, tipo_proceso, responsable, usuario,
                           requisitos = .REQUISITOS_BASE$CodigoRequisito) {
  codigo_tercero <- .ValidarCodigoRuta(codigo_tercero, "código de tercero")
  if (!tipo_proceso %in% c("VINCULACION", "ACTUALIZACION")) stop("Tipo de proceso no válido.", call. = FALSE)

  .ConBloqueo("persistencia", {
    terceros <- ListarTerceros()
    if (!codigo_tercero %in% terceros$CodigoTercero) stop("El tercero no existe.", call. = FALSE)

    solicitudes <- ListarSolicitudes()
    requisitos_indice <- ListarRequisitosSolicitudes()
    seleccion <- dplyr::filter(.REQUISITOS_BASE, CodigoRequisito %in% requisitos)
    if (!nrow(seleccion)) stop("Seleccione al menos un requisito.", call. = FALSE)

    ahora <- as.POSIXct(Sys.time(), tz = "UTC")
    codigo <- .SiguienteSolicitud(solicitudes$CodigoSolicitud)
    ruta <- .RutaSolicitud(codigo)
    if (!dir.create(ruta, recursive = TRUE, showWarnings = FALSE)) stop("No fue posible crear el expediente.")
    completada <- FALSE
    on.exit(if (!completada && dir.exists(ruta)) unlink(ruta, recursive = TRUE, force = TRUE), add = TRUE)

    sapply(c("acceso", "documentos", "formularios", "firmas", "comunicaciones", "auditoria"),
           function(x) dir.create(file.path(ruta, x), recursive = TRUE, showWarnings = FALSE))

    solicitud <- tibble::tibble(
      CodigoSolicitud = codigo, CodigoTercero = codigo_tercero, TipoProceso = tipo_proceso,
      Perfil = "VINCULACION_BASE", Responsable = trimws(responsable), CreadoEn = ahora, CreadoPor = usuario,
      EnviadoEn = as.POSIXct(NA, tz = "UTC"), VenceEn = as.POSIXct(NA, tz = "UTC"), Estado = "GENERADA",
      TotalRequisitos = nrow(seleccion), RequisitosCompletos = 0L, PorcentajeCompletitud = 0,
      UltimaActividadEn = ahora
    )

    req <- dplyr::transmute(
      seleccion, CodigoSolicitud = codigo, CodigoRequisito, TipoRequisito, NombreRequisito, Obligatorio,
      RequiereFirma, Estado = "PENDIENTE", VersionVigente = NA_integer_, RutaRelativa = NA_character_,
      ActualizadoEn = ahora
    )

    .EscribirRDSAtomico(solicitud, file.path(ruta, "solicitud.rds"))
    .EscribirRDSAtomico(req, file.path(ruta, "requisitos.rds"))
    .EscribirRDSAtomico(.ManifiestoVacio(), file.path(ruta, "manifiesto.rds"))
    .EscribirRDSAtomico(dplyr::bind_rows(solicitudes, solicitud),
                        file.path(.RUTAS_DOCUMENTOS$indices, "solicitudes.rds"))
    .EscribirRDSAtomico(dplyr::bind_rows(requisitos_indice, req),
                        file.path(.RUTAS_DOCUMENTOS$indices, "requisitos.rds"))
    completada <- TRUE
    codigo
  })
}

RegistrarEnvioSolicitud <- function(codigo_solicitud, usuario, vigencia_dias = DOCUMENTOS_VIGENCIA_DIAS) {
  codigo_solicitud <- .ValidarCodigoRuta(codigo_solicitud, "código de solicitud")
  .ConBloqueo("persistencia", {
    solicitudes <- ListarSolicitudes()
    i <- match(codigo_solicitud, solicitudes$CodigoSolicitud)
    if (is.na(i)) stop("La solicitud no existe.", call. = FALSE)
    ahora <- as.POSIXct(Sys.time(), tz = "UTC")
    if (is.na(solicitudes$EnviadoEn[i])) {
      solicitudes$EnviadoEn[i] <- ahora
      solicitudes$VenceEn[i] <- ahora + as.difftime(vigencia_dias, units = "days")
      solicitudes$Estado[i] <- "ENVIADA"
    }
    solicitudes$UltimaActividadEn[i] <- ahora
    .EscribirRDSAtomico(solicitudes, file.path(.RUTAS_DOCUMENTOS$indices, "solicitudes.rds"))
    .EscribirRDSAtomico(solicitudes[i, ], file.path(.RutaSolicitud(codigo_solicitud), "solicitud.rds"))
    invisible(codigo_solicitud)
  })
}

# Artefactos del expediente ----
.RegistrarManifiesto <- function(codigo_solicitud, codigo_requisito, tipo, version, ruta, usuario) {
  ruta_solicitud <- .RutaSolicitud(codigo_solicitud)
  manifiesto_ruta <- file.path(ruta_solicitud, "manifiesto.rds")
  manifiesto <- .LeerRDS(manifiesto_ruta, .ManifiestoVacio)
  fila <- tibble::tibble(
    CodigoSolicitud = codigo_solicitud, CodigoRequisito = codigo_requisito, TipoArtefacto = tipo,
    Version = as.integer(version), RutaRelativa = .RutaRelativa(ruta), NombreArchivo = basename(ruta),
    TamanoBytes = as.numeric(file.info(ruta)$size), HashSHA256 = .HashArchivo(ruta), Estado = "VIGENTE",
    RegistradoEn = as.POSIXct(Sys.time(), tz = "UTC"), RegistradoPor = usuario
  )
  .EscribirRDSAtomico(dplyr::bind_rows(manifiesto, fila), manifiesto_ruta)
}

.ActualizarRequisito <- function(codigo_solicitud, codigo_requisito, version, ruta) {
  requisitos <- ListarRequisitosSolicitudes()
  i <- which(requisitos$CodigoSolicitud == codigo_solicitud & requisitos$CodigoRequisito == codigo_requisito)
  if (length(i) != 1L) stop("El requisito no existe o está duplicado.", call. = FALSE)
  requisitos$Estado[i] <- "COMPLETADO"
  requisitos$VersionVigente[i] <- as.integer(version)
  requisitos$RutaRelativa[i] <- .RutaRelativa(ruta)
  requisitos$ActualizadoEn[i] <- as.POSIXct(Sys.time(), tz = "UTC")
  .EscribirRDSAtomico(requisitos, file.path(.RUTAS_DOCUMENTOS$indices, "requisitos.rds"))
  req_solicitud <- dplyr::filter(requisitos, CodigoSolicitud == codigo_solicitud)
  .EscribirRDSAtomico(req_solicitud, file.path(.RutaSolicitud(codigo_solicitud), "requisitos.rds"))
}

GuardarDocumentoSolicitud <- function(codigo_solicitud, codigo_requisito, pdf_origen, resumen, usuario) {
  if (!file.exists(pdf_origen) || tolower(tools::file_ext(pdf_origen)) != "pdf") {
    stop("El documento de origen debe ser un PDF existente.", call. = FALSE)
  }
  if (!is.data.frame(resumen)) stop("El resumen debe ser un data.frame.", call. = FALSE)
  if (!requireNamespace("openxlsx", quietly = TRUE)) stop("Se requiere el paquete openxlsx.", call. = FALSE)

  codigo_solicitud <- .ValidarCodigoRuta(codigo_solicitud, "código de solicitud")
  codigo_requisito <- .ValidarCodigoRuta(codigo_requisito, "código de requisito")
  .ConBloqueo("persistencia", {
    requisitos <- ListarRequisitosSolicitudes(codigo_solicitud)
    req <- dplyr::filter(requisitos, CodigoRequisito == codigo_requisito, TipoRequisito == "DOCUMENTO")
    if (nrow(req) != 1L) stop("El requisito documental no existe.", call. = FALSE)

    ruta_req <- file.path(.RutaSolicitud(codigo_solicitud), "documentos", codigo_requisito)
    version <- .SiguienteVersion(ruta_req)
    ruta_version <- file.path(ruta_req, sprintf("v%03d", version))
    dir.create(ruta_version, recursive = TRUE, showWarnings = FALSE)
    completado <- FALSE
    on.exit({
      if (!completado && dir.exists(ruta_version)) unlink(ruta_version, recursive = TRUE, force = TRUE)
    }, add = TRUE)

    pdf_destino <- file.path(ruta_version, "documento.pdf")
    excel_destino <- file.path(ruta_version, "resumen.xlsx")
    if (!file.copy(pdf_origen, pdf_destino, overwrite = FALSE, copy.mode = FALSE)) {
      stop("No fue posible copiar el documento.", call. = FALSE)
    }
    openxlsx::write.xlsx(resumen, excel_destino, overwrite = FALSE)
    .RegistrarManifiesto(codigo_solicitud, codigo_requisito, "DOCUMENTO_PDF", version, pdf_destino, usuario)
    .RegistrarManifiesto(codigo_solicitud, codigo_requisito, "RESUMEN_XLSX", version, excel_destino, usuario)
    .ActualizarRequisito(codigo_solicitud, codigo_requisito, version, ruta_version)
    completado <- TRUE
    ActualizarCompletitudSolicitud(codigo_solicitud)
    invisible(ruta_version)
  })
}

GuardarFormularioSolicitud <- function(codigo_solicitud, codigo_requisito, pdf_origen, usuario) {
  if (!file.exists(pdf_origen) || tolower(tools::file_ext(pdf_origen)) != "pdf") {
    stop("El formulario debe ser un PDF existente.", call. = FALSE)
  }

  codigo_solicitud <- .ValidarCodigoRuta(codigo_solicitud, "código de solicitud")
  codigo_requisito <- .ValidarCodigoRuta(codigo_requisito, "código de requisito")
  .ConBloqueo("persistencia", {
    requisitos <- ListarRequisitosSolicitudes(codigo_solicitud)
    req <- dplyr::filter(requisitos, CodigoRequisito == codigo_requisito, TipoRequisito == "FORMULARIO")
    if (nrow(req) != 1L) stop("El requisito de formulario no existe.", call. = FALSE)

    ruta_req <- file.path(.RutaSolicitud(codigo_solicitud), "formularios", codigo_requisito)
    version <- .SiguienteVersion(ruta_req)
    ruta_version <- file.path(ruta_req, sprintf("v%03d", version))
    dir.create(ruta_version, recursive = TRUE, showWarnings = FALSE)
    completado <- FALSE
    on.exit({
      if (!completado && dir.exists(ruta_version)) unlink(ruta_version, recursive = TRUE, force = TRUE)
    }, add = TRUE)

    pdf_destino <- file.path(ruta_version, "formulario.pdf")
    if (!file.copy(pdf_origen, pdf_destino, overwrite = FALSE, copy.mode = FALSE)) {
      stop("No fue posible copiar el formulario.", call. = FALSE)
    }
    .RegistrarManifiesto(codigo_solicitud, codigo_requisito, "FORMULARIO_PDF", version, pdf_destino, usuario)
    .ActualizarRequisito(codigo_solicitud, codigo_requisito, version, ruta_version)
    completado <- TRUE
    ActualizarCompletitudSolicitud(codigo_solicitud)
    invisible(ruta_version)
  })
}

# Completitud ----
ActualizarCompletitudSolicitud <- function(codigo_solicitud) {
  codigo_solicitud <- .ValidarCodigoRuta(codigo_solicitud, "código de solicitud")
  requisitos <- ListarRequisitosSolicitudes(codigo_solicitud)
  if (!nrow(requisitos)) return(invisible(NULL))

  completo <- vapply(seq_len(nrow(requisitos)), function(i) {
    ruta <- requisitos$RutaRelativa[i]
    if (is.na(ruta) || !nzchar(ruta)) return(FALSE)
    absoluta <- file.path(.RUTAS_DOCUMENTOS$raiz, ruta)
    if (requisitos$TipoRequisito[i] == "DOCUMENTO") {
      file.exists(file.path(absoluta, "documento.pdf")) && file.exists(file.path(absoluta, "resumen.xlsx"))
    } else {
      file.exists(file.path(absoluta, "formulario.pdf"))
    }
  }, logical(1))

  requisitos$Estado <- ifelse(completo, "COMPLETADO", "PENDIENTE")
  solicitudes <- ListarSolicitudes()
  i <- match(codigo_solicitud, solicitudes$CodigoSolicitud)
  if (is.na(i)) stop("La solicitud no existe.", call. = FALSE)
  obligatorios <- requisitos$Obligatorio
  total <- sum(obligatorios)
  terminados <- sum(completo & obligatorios)
  solicitudes$TotalRequisitos[i] <- total
  solicitudes$RequisitosCompletos[i] <- terminados
  solicitudes$PorcentajeCompletitud[i] <- if (total) round(100 * terminados / total, 1) else 100
  solicitudes$UltimaActividadEn[i] <- as.POSIXct(Sys.time(), tz = "UTC")

  .EscribirRDSAtomico(solicitudes, file.path(.RUTAS_DOCUMENTOS$indices, "solicitudes.rds"))
  .EscribirRDSAtomico(solicitudes[i, ], file.path(.RutaSolicitud(codigo_solicitud), "solicitud.rds"))
  invisible(solicitudes[i, ])
}
