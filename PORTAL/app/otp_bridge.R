# Puente mínimo entre FastAPI y los índices RDS de DocumentosTerceros ----
.bridge_fail <- function(code) {
  cat("ERROR\n", code, "\n", sep = "")
  quit(save = "no", status = 2L, runLast = FALSE)
}

.bridge_read <- function(path) {
  tryCatch(readRDS(path), error = function(e) .bridge_fail("DATA_UNAVAILABLE"))
}

.bridge_request <- function(code, data_path) {
  if (!grepl("^SOL-[0-9]{4}-[0-9]{6}$", code)) .bridge_fail("INVALID_INPUT")
  indices <- file.path(data_path, "indices")
  solicitudes <- .bridge_read(file.path(indices, "solicitudes.rds"))
  terceros <- .bridge_read(file.path(indices, "terceros.rds"))
  requisitos <- .bridge_read(file.path(indices, "requisitos.rds"))

  solicitud <- solicitudes[solicitudes$CodigoSolicitud == code, , drop = FALSE]
  if (nrow(solicitud) != 1L) .bridge_fail("REQUEST_NOT_AVAILABLE")
  vence <- solicitud$VenceEn[[1L]]
  enviado <- solicitud$EnviadoEn[[1L]]
  estado <- as.character(solicitud$Estado[[1L]])
  if (is.na(enviado) || is.na(vence) || vence <= Sys.time() ||
      estado %in% c("VENCIDA", "CANCELADA", "RECHAZADA")) {
    .bridge_fail("REQUEST_NOT_AVAILABLE")
  }

  tercero <- terceros[terceros$CodigoTercero == solicitud$CodigoTercero[[1L]], , drop = FALSE]
  if (nrow(tercero) != 1L) .bridge_fail("REQUEST_NOT_AVAILABLE")
  req <- requisitos[requisitos$CodigoSolicitud == code, , drop = FALSE]
  list(solicitud = solicitud, tercero = tercero, requisitos = req)
}

.bridge_form <- function(code, form_code, data_path) {
  data <- .bridge_request(code, data_path)
  req <- data$requisitos[
    data$requisitos$CodigoRequisito == form_code &
      data$requisitos$TipoRequisito == "FORMULARIO",
    ,
    drop = FALSE
  ]
  if (nrow(req) != 1L) .bridge_fail("FORM_NOT_AVAILABLE")

  catalog_path <- file.path(data_path, "catalogos", "formularios.json")
  if (!file.exists(catalog_path)) .bridge_fail("FORM_NOT_AVAILABLE")
  catalog <- tryCatch(
    jsonlite::fromJSON(catalog_path, simplifyVector = FALSE),
    error = function(e) .bridge_fail("DATA_UNAVAILABLE")
  )
  forms <- catalog$formularios
  if (!is.list(forms)) .bridge_fail("DATA_UNAVAILABLE")
  indexes <- which(vapply(forms, function(x) identical(x$codigo, form_code) && isTRUE(x$activo), logical(1)))
  if (length(indexes) != 1L) .bridge_fail("FORM_NOT_AVAILABLE")

  response_path <- file.path(data_path, "solicitudes", code, "formularios", form_code, "respuesta.json")
  answers <- if (file.exists(response_path)) {
    tryCatch(jsonlite::fromJSON(response_path, simplifyVector = FALSE), error = function(e) NULL)
  } else {
    NULL
  }
  list(formulario = forms[[indexes]], respuestas = answers)
}

.bridge_validate_answers <- function(form, answers) {
  if (!is.list(answers) || is.null(names(answers)) || any(!nzchar(names(answers)))) {
    .bridge_fail("FORM_INVALID")
  }
  fields <- form$formulario$campos
  codes <- vapply(fields, `[[`, character(1), "codigo")
  if (length(setdiff(names(answers), codes))) .bridge_fail("FORM_INVALID")

  for (field in fields) {
    value <- answers[[field$codigo]]
    required <- isTRUE(field$obligatorio)
    if (identical(field$tipo, "CASILLA")) {
      if (required && !isTRUE(value)) .bridge_fail("FORM_INVALID")
      next
    }
    text <- if (is.null(value) || length(value) != 1L) "" else trimws(as.character(value))
    if (required && !nzchar(text)) .bridge_fail("FORM_INVALID")
    if (identical(field$tipo, "SELECCION") && nzchar(text)) {
      options <- trimws(strsplit(field$opciones, "|", fixed = TRUE)[[1L]])
      if (!text %in% options) .bridge_fail("FORM_INVALID")
    }
  }
  invisible(TRUE)
}

.bridge_write_rds <- function(object, path) {
  temporary <- tempfile(pattern = ".rds-", tmpdir = dirname(path), fileext = ".tmp")
  on.exit(if (file.exists(temporary)) unlink(temporary, force = TRUE), add = TRUE)
  saveRDS(object, temporary, compress = "xz", version = 3)
  if (!file.rename(temporary, path)) .bridge_fail("DATA_UNAVAILABLE")
}

.bridge_write_json <- function(object, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(pattern = ".json-", tmpdir = dirname(path), fileext = ".tmp")
  on.exit(if (file.exists(temporary)) unlink(temporary, force = TRUE), add = TRUE)
  writeLines(
    jsonlite::toJSON(object, auto_unbox = TRUE, null = "null", na = "null", pretty = TRUE),
    temporary,
    useBytes = TRUE
  )
  if (!file.rename(temporary, path)) .bridge_fail("DATA_UNAVAILABLE")
}

.bridge_save_form <- function(code, form_code, answers_json, data_path) {
  form <- .bridge_form(code, form_code, data_path)
  answers <- tryCatch(
    jsonlite::fromJSON(answers_json, simplifyVector = FALSE),
    error = function(e) .bridge_fail("FORM_INVALID")
  )
  .bridge_validate_answers(form, answers)

  lock <- file.path(data_path, "bloqueos", "portal_formularios.lock")
  started <- Sys.time()
  repeat {
    if (dir.create(lock, showWarnings = FALSE, recursive = FALSE)) break
    if (as.numeric(difftime(Sys.time(), started, units = "secs")) >= 10) {
      .bridge_fail("DATA_UNAVAILABLE")
    }
    Sys.sleep(0.1)
  }
  on.exit(if (dir.exists(lock)) unlink(lock, recursive = TRUE, force = TRUE), add = TRUE)

  now <- as.POSIXct(Sys.time(), tz = "UTC")
  response <- list(
    formulario = form_code,
    version = as.integer(form$formulario$version),
    respuestas = answers,
    guardado_en = format(now, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
  request_dir <- file.path(data_path, "solicitudes", code)
  response_path <- file.path(request_dir, "formularios", form_code, "respuesta.json")
  .bridge_write_json(response, response_path)

  index_path <- file.path(data_path, "indices", "requisitos.rds")
  requirements <- .bridge_read(index_path)
  selected <- requirements$CodigoSolicitud == code & requirements$CodigoRequisito == form_code
  if (sum(selected) != 1L) .bridge_fail("FORM_NOT_AVAILABLE")
  requirements$Estado[selected] <- "COMPLETADO"
  requirements$VersionVigente[selected] <- as.integer(form$formulario$version)
  requirements$RutaRelativa[selected] <- file.path(
    "solicitudes", code, "formularios", form_code, "respuesta.json"
  )
  requirements$ActualizadoEn[selected] <- now
  .bridge_write_rds(requirements, index_path)

  local_requirements <- requirements[requirements$CodigoSolicitud == code, , drop = FALSE]
  .bridge_write_rds(local_requirements, file.path(request_dir, "requisitos.rds"))

  requests_path <- file.path(data_path, "indices", "solicitudes.rds")
  requests <- .bridge_read(requests_path)
  request_index <- which(requests$CodigoSolicitud == code)
  if (length(request_index) != 1L) .bridge_fail("REQUEST_NOT_AVAILABLE")
  complete <- sum(local_requirements$Estado == "COMPLETADO")
  total <- nrow(local_requirements)
  requests$RequisitosCompletos[request_index] <- complete
  requests$PorcentajeCompletitud[request_index] <- if (total) round(100 * complete / total, 1) else 0
  requests$UltimaActividadEn[request_index] <- now
  if (requests$Estado[request_index] %in% c("GENERADA", "ENVIADA")) {
    requests$Estado[request_index] <- "EN_DILIGENCIAMIENTO"
  }
  .bridge_write_rds(requests, requests_path)
  .bridge_write_rds(requests[request_index, , drop = FALSE], file.path(request_dir, "solicitud.rds"))

  list(message = "Formulario guardado correctamente.")
}

.bridge_main <- function() {
  connection <- file("stdin", open = "r", encoding = "UTF-8")
  on.exit(close(connection), add = TRUE)
  fields <- readLines(connection, warn = FALSE)
  if (!length(fields)) .bridge_fail("INVALID_INPUT")

  data_path <- Sys.getenv("DOCUMENTOS_DATA_PATH", "")
  if (!nzchar(data_path) || !dir.exists(data_path)) .bridge_fail("DATA_UNAVAILABLE")
  data_path <- normalizePath(data_path, winslash = "/", mustWork = TRUE)
  operation <- fields[[1L]]

  if (identical(operation, "otp_send")) {
    if (length(fields) != 4L) .bridge_fail("INVALID_INPUT")
    code <- trimws(fields[[2L]])
    otp <- trimws(fields[[3L]])
    minutes <- suppressWarnings(as.integer(fields[[4L]]))
    if (!grepl("^[0-9]{6}$", otp) || is.na(minutes) || minutes < 1L) {
      .bridge_fail("INVALID_INPUT")
    }
    data <- .bridge_request(code, data_path)
    email <- tolower(trimws(as.character(data$tercero$CorreoContacto[[1L]])))
    name <- trimws(as.character(data$tercero$NombreContacto[[1L]]))
    if (is.na(email) || !grepl("^[^@[:space:]]+@[^@[:space:]]+\\.[^@[:space:]]+$", email)) {
      .bridge_fail("EMAIL_FAILED")
    }
    if (is.na(name) || !nzchar(name)) {
      name <- trimws(as.character(data$tercero$NombreRazonSocial[[1L]]))
    }
    if (is.na(name)) name <- ""
    sent <- tryCatch({
      suppressMessages(racafeMail::racafeMailOTP(
        para = email, codigo = otp, nombre = name, minutos = minutes
      ))
      TRUE
    }, error = function(e) FALSE)
    if (!sent) .bridge_fail("EMAIL_FAILED")
    cat("OK\n")
    return(invisible(NULL))
  }

  if (identical(operation, "request")) {
    if (length(fields) != 2L) .bridge_fail("INVALID_INPUT")
    code <- trimws(fields[[2L]])
    data <- .bridge_request(code, data_path)
    req <- lapply(seq_len(nrow(data$requisitos)), function(i) {
      list(
        codigo = as.character(data$requisitos$CodigoRequisito[[i]]),
        tipo = as.character(data$requisitos$TipoRequisito[[i]]),
        nombre = as.character(data$requisitos$NombreRequisito[[i]]),
        estado = as.character(data$requisitos$Estado[[i]])
      )
    })
    payload <- list(
      codigo_solicitud = code,
      vence_en = format(
        as.POSIXct(data$solicitud$VenceEn[[1L]], tz = "UTC"),
        "%Y-%m-%dT%H:%M:%SZ",
        tz = "UTC"
      ),
      tercero = list(
        tipo_identificacion = as.character(data$tercero$TipoIdentificacion[[1L]]),
        numero_identificacion = as.character(data$tercero$NumeroIdentificacion[[1L]]),
        nombre_razon_social = as.character(data$tercero$NombreRazonSocial[[1L]])
      ),
      requisitos = unname(req)
    )
    cat(
      "JSON\n",
      jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null", na = "null"),
      "\n",
      sep = ""
    )
    return(invisible(NULL))
  }

  if (identical(operation, "form")) {
    if (length(fields) != 3L) .bridge_fail("INVALID_INPUT")
    payload <- .bridge_form(trimws(fields[[2L]]), trimws(fields[[3L]]), data_path)
    cat(
      "JSON\n",
      jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null", na = "null"),
      "\n",
      sep = ""
    )
    return(invisible(NULL))
  }

  if (identical(operation, "form_save")) {
    if (length(fields) != 4L) .bridge_fail("INVALID_INPUT")
    payload <- .bridge_save_form(trimws(fields[[2L]]), trimws(fields[[3L]]), fields[[4L]], data_path)
    cat(
      "JSON\n",
      jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null", na = "null"),
      "\n",
      sep = ""
    )
    return(invisible(NULL))
  }

  .bridge_fail("INVALID_INPUT")
}

status <- tryCatch({
  .bridge_main()
  0L
}, error = function(e) {
  cat("ERROR\nBRIDGE_ERROR\n")
  1L
})
quit(save = "no", status = status, runLast = FALSE)
