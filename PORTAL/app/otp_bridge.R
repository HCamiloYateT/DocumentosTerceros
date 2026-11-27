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
