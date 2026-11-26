# Funciones del módulo Solicitudes ----
.sol_qr_data_uri <- function(enlace) {
  archivo <- tempfile(fileext = ".png")
  on.exit({
    if (grDevices::dev.cur() > 1L) grDevices::dev.off()
    unlink(archivo, force = TRUE)
  }, add = FALSE)
  grDevices::png(archivo, width = 700, height = 700, bg = "white")
  graphics::plot(qrcode::qr_code(enlace))
  grDevices::dev.off()
  base64enc::dataURI(file = archivo, mime = "image/png")
}

.sol_texto <- function(x) {
  if (is.null(x) || !length(x) || is.na(x[1L])) return("")
  trimws(as.character(x[1L]))
}

.sol_datos_tercero <- function(datos) {
  list(
    TipoPersona = if (identical(datos$tipo_documento, "NIT")) "JURIDICA" else "NATURAL",
    TipoIdentificacion = datos$tipo_documento,
    NumeroIdentificacion = datos$numero_documento,
    DigitoVerificacion = "",
    NombreRazonSocial = datos$nombre,
    NombreContacto = datos$nombre,
    CorreoContacto = datos$correo,
    Telefono = datos$celular %||% "",
    TipoTercero = "PROVEEDOR",
    Pais = "CO",
    Ciudad = "",
    Observaciones = ""
  )
}

.sol_formatear_vencimiento <- function(fecha) {
  format(fecha, "%d/%m/%Y %I:%M %p", tz = "America/Bogota")
}

.sol_nombres_documentos <- function(codigos) {
  nombres <- names(.SOL_DOCUMENTOS)[match(codigos, unname(.SOL_DOCUMENTOS))]
  unique(nombres[!is.na(nombres)])
}

.sol_datos_correo <- function(datos, enlace, vencimiento) {
  list(
    asunto = "Racafé | Solicitud de documentación para creación de tercero",
    preencabezado = "Documentación para creación de tercero",
    categoria = "Gestión de terceros",
    bloques = list(
      list(tipo = "saludo", nombre = datos$nombre),
      list(
        tipo = "parrafo",
        texto = paste(
          "Racafé & Cía. S.C.A. se encuentra realizando el proceso de creación o actualización",
          "de su registro como tercero en nuestros sistemas."
        )
      ),
      list(
        tipo = "parrafo",
        texto = "Para continuar con el proceso, diligencie o adjunte los siguientes documentos:"
      ),
      list(
        tipo = "lista", titulo = "Documentos requeridos:",
        items = .sol_nombres_documentos(datos$documentos)
      ),
      list(
        tipo = "parrafo",
        texto = "Ingrese a la solicitud mediante el siguiente enlace:"
      ),
      list(tipo = "boton", etiqueta = "Abrir solicitud de documentación", enlace = enlace),
      list(tipo = "destacado", texto = paste("Vigencia del enlace:", vencimiento, "(hora de Colombia).")),
      list(
        tipo = "parrafo",
        texto = paste(
          "El enlace es personal y no debe compartirse. Podrá solicitarse un código de verificación",
          "enviado al correo registrado."
        )
      ),
      list(tipo = "parrafo", texto = "Cordialmente,")
    )
  )
}

.sol_render_correo <- function(datos, enlace, vencimiento) {
  racafeMailRender("personalizado", .sol_datos_correo(datos, enlace, vencimiento),
                   config = racafeMailConfig())
}

.sol_notificar_copia <- function(session, resultado, tipo = c("qr", "enlace")) {
  tipo <- match.arg(tipo)
  textos <- switch(
    tipo,
    qr = c("Código QR copiado", "El código QR fue copiado al portapapeles."),
    enlace = c("Enlace copiado", "El enlace fue copiado al portapapeles.")
  )
  if (isTRUE(resultado$success)) {
    racafeNotify(session, textos[1L], textos[2L], type = "success")
  } else {
    racafeNotify(session, "No fue posible copiar", "Revise los permisos del portapapeles.", type = "error")
  }
  invisible(NULL)
}

.sol_crear <- function(datos, usuario) {
  tercero <- CrearTercero(.sol_datos_tercero(datos), usuario)
  codigo <- CrearSolicitud(tercero, "VINCULACION", usuario, usuario, datos$documentos)
  enlace <- ConstruirEnlaceSolicitud(codigo)
  vencimiento <- .sol_formatear_vencimiento(
    as.POSIXct(Sys.time(), tz = "UTC") + as.difftime(datos$vigencia_dias, units = "days")
  )
  list(
    codigo = codigo, datos = datos, correo_destino = datos$correo, enlace = enlace,
    vencimiento = vencimiento, correo = .sol_render_correo(datos, enlace, vencimiento),
    qr = .sol_qr_data_uri(enlace), estado_correo = "PENDIENTE",
    detalle_correo = "El correo aún no se ha enviado."
  )
}

.sol_enviar <- function(resultado, usuario) {
  RegistrarEnvioSolicitud(resultado$codigo, usuario, resultado$datos$vigencia_dias)
  solicitudes <- ListarSolicitudes()
  solicitud <- solicitudes[match(resultado$codigo, solicitudes$CodigoSolicitud), ]
  vencimiento <- .sol_formatear_vencimiento(solicitud$VenceEn)
  correo <- .sol_render_correo(resultado$datos, resultado$enlace, vencimiento)
  config <- racafeMailConfig()
  error_envio <- NULL
  tryCatch(
    racafeMailEnviar(
      para = resultado$correo_destino, plantilla = "personalizado",
      datos = .sol_datos_correo(resultado$datos, resultado$enlace, vencimiento),
      config = config
    ),
    error = function(e) error_envio <<- conditionMessage(e)
  )
  estado <- if (is.null(error_envio)) "ENVIADO" else "ERROR"
  detalle <- error_envio %||% "Correo enviado correctamente."
  RegistrarComunicacionSolicitud(resultado$codigo, resultado$correo_destino, correo$asunto,
                                 estado, usuario, detalle = detalle)
  resultado$vencimiento <- vencimiento
  resultado$correo <- correo
  resultado$estado_correo <- estado
  resultado$detalle_correo <- detalle
  resultado
}

.sol_datos_individuales <- function(input) {
  list(
    nombre = .sol_texto(input$nombre),
    tipo_documento = .sol_texto(input$tipo_documento),
    numero_documento = .sol_texto(input$numero_documento),
    correo = tolower(.sol_texto(input$correo)),
    celular = .sol_texto(input$celular),
    documentos = input$documentos,
    vigencia_dias = suppressWarnings(as.integer(input$vigencia_dias))
  )
}

.sol_validar <- function(datos) {
  c(
    "El nombre es obligatorio" = !nzchar(datos$nombre),
    "El tipo de documento no es válido" = !datos$tipo_documento %in% .SOL_TIPOS_DOCUMENTO,
    "El número de documento es obligatorio" = !nzchar(datos$numero_documento),
    "El correo no es válido" = !grepl("^[^@[:space:]]+@[^@[:space:]]+\\.[^@[:space:]]+$", datos$correo),
    "Seleccione al menos un documento" = !length(datos$documentos),
    "La vigencia debe estar entre 1 y 30 días" = is.na(datos$vigencia_dias) ||
      datos$vigencia_dias < 1L || datos$vigencia_dias > 30L
  )
}

.sol_leer_masivas <- function(archivo) {
  validacion <- racafeValidateInputFile(archivo, accept = ".xlsx", max_size = .SOL_MAX_ARCHIVO)
  if (!validacion$valid) stop(paste(validacion$errors, collapse = " "), call. = FALSE)
  datos <- openxlsx::read.xlsx(archivo$datapath, sheet = 1, check.names = FALSE)
  faltantes <- setdiff(.SOL_COLUMNAS_MASIVAS, names(datos))
  if (length(faltantes)) stop("Faltan columnas: ", paste(faltantes, collapse = ", "), call. = FALSE)
  if (!nrow(datos) || nrow(datos) > .SOL_MAX_FILAS) {
    stop("El archivo debe contener entre 1 y 100 filas.", call. = FALSE)
  }
  datos
}

.sol_fila_masiva <- function(datos, i) {
  valor_documentos <- as.character(datos$Documentos[i] %||% "")
  tokens <- toupper(trimws(unlist(strsplit(valor_documentos, "[|,;]"))))
  documentos <- unname(.SOL_MAPA_DOCUMENTOS[tokens[tokens %in% names(.SOL_MAPA_DOCUMENTOS)]])
  if (!length(documentos)) documentos <- unname(.SOL_DOCUMENTOS)
  vigencia <- suppressWarnings(as.integer(datos$VigenciaDias[i] %||% DOCUMENTOS_VIGENCIA_DIAS))
  if (is.na(vigencia)) vigencia <- DOCUMENTOS_VIGENCIA_DIAS
  list(
    nombre = .sol_texto(datos$Nombre[i]),
    tipo_documento = toupper(.sol_texto(datos$TipoDocumento[i])),
    numero_documento = .sol_texto(datos$NumeroDocumento[i]),
    correo = tolower(.sol_texto(datos$Correo[i])),
    celular = if ("Celular" %in% names(datos)) .sol_texto(datos$Celular[i]) else "",
    documentos = unique(documentos), vigencia_dias = vigencia
  )
}

.sol_mostrar_errores <- function(condiciones) {
  sapply(names(condiciones)[condiciones], function(mensaje) {
    racafeNotify(getDefaultReactiveDomain(), mensaje, type = "error")
  })
  invisible(NULL)
}
