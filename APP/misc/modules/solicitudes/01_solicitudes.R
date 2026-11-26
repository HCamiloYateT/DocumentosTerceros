# Módulo de solicitudes ----
.DOCUMENTOS_SOLICITUD <- c(
  "RUT" = "DOC_RUT", "Certificado de existencia y representación legal" = "DOC_CERL",
  "Autorización de tratamiento de datos" = "FORM_HABEAS"
)

.CrearQRDataURI <- function(enlace) {
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

.DatosTerceroSolicitud <- function(datos) {
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

.CrearEnviarSolicitud <- function(datos, usuario) {
  tercero <- CrearTercero(.DatosTerceroSolicitud(datos), usuario)
  codigo <- CrearSolicitud(tercero, "VINCULACION", usuario, usuario, datos$documentos)
  RegistrarEnvioSolicitud(codigo, usuario, datos$vigencia_dias)

  solicitudes <- ListarSolicitudes()
  solicitud <- solicitudes[match(codigo, solicitudes$CodigoSolicitud), ]
  enlace <- ConstruirEnlaceSolicitud(codigo)
  vencimiento <- format(solicitud$VenceEn, "%d/%m/%Y %I:%M %p", tz = "America/Bogota")
  config <- racafeMail::racafeMailConfig()
  correo <- racafeMail::racafeMailRender(
    "invitacion",
    list(nombre = datos$nombre, enlace = enlace, vencimiento = vencimiento),
    config = config
  )

  error_envio <- NULL
  tryCatch(
    racafeMail::racafeMailInvitacion(
      para = datos$correo, nombre = datos$nombre, enlace = enlace,
      vencimiento = vencimiento, config = config
    ),
    error = function(e) error_envio <<- conditionMessage(e)
  )
  estado <- if (is.null(error_envio)) "ENVIADO" else "ERROR"
  RegistrarComunicacionSolicitud(
    codigo, datos$correo, correo$asunto, estado, usuario,
    detalle = error_envio %||% "Envío SMTP confirmado por racafeMail."
  )

  list(
    codigo = codigo, nombre = datos$nombre, correo_destino = datos$correo,
    enlace = enlace, vencimiento = vencimiento, correo = correo,
    qr = .CrearQRDataURI(enlace), estado_correo = estado,
    detalle_correo = error_envio %||% "Correo enviado correctamente."
  )
}

.SolicitudIndividualUI <- function(ns) {
  bs4Dash::bs4Row(
    bs4Dash::bs4Column(
      width = 6,
      racafeForms::racafePanel(
        "Datos del tercero",
        racafeForms::racafeForm(
          id = "solicitud_individual", ns = ns, variant = "plain",
          racafeForms::racafeInputText(
            "nombre", "Nombre completo o razón social", required = TRUE, ns = ns
          ),
          racafeForms::racafeFormRow(
            columns = 2, responsive = "auto",
            racafeForms::racafeInputSelect(
              "tipo_documento", "Tipo de documento",
              choices = c("NIT", "CC", "CE", "PASAPORTE"),
              selected = "NIT", empty = FALSE, ns = ns
            ),
            racafeForms::racafeInputText(
              "numero_documento", "Número de documento", required = TRUE, ns = ns
            )
          ),
          racafeForms::racafeFormRow(
            columns = 2, responsive = "auto",
            racafeForms::racafeInputText(
              "correo", "Correo electrónico", type = "email", required = TRUE, ns = ns
            ),
            racafeForms::racafeInputText("celular", "Celular", type = "tel", ns = ns)
          ),
          racafeForms::racafeInputSelect(
            "documentos", "Documentos a incluir", choices = .DOCUMENTOS_SOLICITUD,
            selected = unname(.DOCUMENTOS_SOLICITUD), multiple = TRUE,
            empty = FALSE, gender = "m", ns = ns
          ),
          racafeForms::racafeInputNumeric(
            "vigencia_dias", "Vigencia del enlace en días", value = DOCUMENTOS_VIGENCIA_DIAS,
            format = "integer", min = 1, max = 30, ns = ns
          ),
          racafeForms::racafeFormFooter(
            racafeForms::racafeButton(
              "crear_enviar", "Crear y enviar solicitud", icon = "paper-plane", ns = ns
            )
          )
        )
      )
    ),
    bs4Dash::bs4Column(
      width = 6,
      racafeForms::racafePanel(
        "Compartir con el tercero", shiny::uiOutput(ns("compartir_individual"))
      )
    )
  )
}

.SolicitudMasivaUI <- function(ns) {
  bs4Dash::bs4Row(
    bs4Dash::bs4Column(
      width = 6,
      racafeForms::racafePanel(
        "Archivo de solicitudes",
        racafeForms::racafeInputFile(
          "archivo_masivo", "Archivo Excel", accept = ".xlsx",
          max_size = 10 * 1024^2, help = "Máximo 100 solicitudes por archivo.", ns = ns
        ),
        shiny::p(
          "Columnas: Nombre, TipoDocumento, NumeroDocumento, Correo, Celular, ",
          "Documentos y VigenciaDias. Separe varios documentos con |."
        ),
        racafeForms::racafeFormFooter(
          racafeForms::racafeButtonDownload(
            "plantilla_masiva", "Descargar plantilla", icon = "file-excel", ns = ns
          ),
          racafeForms::racafeButton(
            "procesar_masivo", "Crear y enviar solicitudes", icon = "paper-plane", ns = ns
          )
        )
      )
    ),
    bs4Dash::bs4Column(
      width = 6,
      racafeForms::racafePanel(
        "Resultado del envío", shiny::uiOutput(ns("resultado_masivo"))
      )
    )
  )
}

SolicitudesUI <- function(id) {
  ns <- shiny::NS(id)
  racafeForms::racafeTabs(
    shiny::tabPanel("Creación individual", .SolicitudIndividualUI(ns), value = "individual"),
    shiny::tabPanel("Creación masiva", .SolicitudMasivaUI(ns), value = "masiva"),
    id = "modo_solicitud", selected = "individual", ns = ns
  )
}

.DatosIndividuales <- function(input) {
  list(
    nombre = trimws(input$nombre %||% ""),
    tipo_documento = input$tipo_documento,
    numero_documento = trimws(input$numero_documento %||% ""),
    correo = tolower(trimws(input$correo %||% "")),
    celular = trimws(input$celular %||% ""),
    documentos = input$documentos,
    vigencia_dias = as.integer(input$vigencia_dias)
  )
}

.TextoSolicitud <- function(x) {
  if (is.null(x) || !length(x) || is.na(x[1L])) return("")
  trimws(as.character(x[1L]))
}

.ValidarSolicitud <- function(datos) {
  c(
    "El nombre es obligatorio" = !nzchar(datos$nombre),
    "El tipo de documento no es válido" = !datos$tipo_documento %in% c("NIT", "CC", "CE", "PASAPORTE"),
    "El número de documento es obligatorio" = !nzchar(datos$numero_documento),
    "El correo no es válido" = !grepl("^[^@[:space:]]+@[^@[:space:]]+\\.[^@[:space:]]+$", datos$correo),
    "Seleccione al menos un documento" = !length(datos$documentos),
    "La vigencia debe estar entre 1 y 30 días" = is.na(datos$vigencia_dias) ||
      datos$vigencia_dias < 1L || datos$vigencia_dias > 30L
  )
}

.CompartirSolicitudUI <- function(resultado, ns) {
  if (is.null(resultado)) {
    return(shiny::tagList(
      shiny::p("No se han creado solicitudes en esta sesión."),
      shiny::p("Cree una solicitud para obtener el enlace, el código QR y la vista previa del correo.")
    ))
  }
  variante <- if (identical(resultado$estado_correo, "ENVIADO")) "success" else "danger"
  shiny::tagList(
    racafeForms::racafeStatusBadge(resultado$estado_correo, variant = variante),
    shiny::p(shiny::strong(resultado$codigo), " · ", resultado$correo_destino),
    shiny::p("Vigente hasta ", resultado$vencimiento, " (hora de Colombia)."),
    shiny::p(resultado$detalle_correo),
    shiny::tags$img(
      src = resultado$qr, alt = paste("QR", resultado$codigo), width = "180", height = "180"
    ),
    racafeForms::racafeActionBar(
      right = shiny::tagList(
        racafeForms::racafeCopyButton(
          "copiar_enlace", "Copiar enlace", text = resultado$enlace, ns = ns
        ),
        racafeForms::racafeCopyButton(
          "copiar_qr", "Copiar QR", image = resultado$qr, ns = ns
        ),
        racafeForms::racafeButton("ampliar_qr", "Ampliar QR", icon = "expand", ns = ns),
        racafeForms::racafeButton(
          "ver_correo", "Vista previa del correo", icon = "envelope",
          variant = "secondary", ns = ns
        )
      )
    ),
    shiny::small("El enlace es provisional mientras se habilita el formulario público.")
  )
}

.LeerSolicitudesMasivas <- function(archivo) {
  validacion <- racafeForms::racafeValidateInputFile(
    archivo, accept = ".xlsx", max_size = 10 * 1024^2
  )
  if (!validacion$valid) stop(paste(validacion$errors, collapse = " "), call. = FALSE)
  datos <- openxlsx::read.xlsx(archivo$datapath, sheet = 1, check.names = FALSE)
  requeridas <- c("Nombre", "TipoDocumento", "NumeroDocumento", "Correo")
  faltantes <- setdiff(requeridas, names(datos))
  if (length(faltantes)) stop("Faltan columnas: ", paste(faltantes, collapse = ", "), call. = FALSE)
  if (!nrow(datos) || nrow(datos) > 100L) stop("El archivo debe contener entre 1 y 100 filas.", call. = FALSE)
  datos
}

.FilaSolicitudMasiva <- function(datos, i) {
  valor_documentos <- as.character(datos$Documentos[i] %||% "")
  tokens <- toupper(trimws(unlist(strsplit(valor_documentos, "[|,;]"))))
  mapa <- c(
    RUT = "DOC_RUT", CERL = "DOC_CERL", HABEAS = "FORM_HABEAS",
    HABEAS_DATA = "FORM_HABEAS", DOC_RUT = "DOC_RUT", DOC_CERL = "DOC_CERL",
    FORM_HABEAS = "FORM_HABEAS"
  )
  documentos <- unname(mapa[tokens[tokens %in% names(mapa)]])
  if (!length(documentos)) documentos <- unname(.DOCUMENTOS_SOLICITUD)
  vigencia <- suppressWarnings(as.integer(datos$VigenciaDias[i] %||% DOCUMENTOS_VIGENCIA_DIAS))
  if (is.na(vigencia)) vigencia <- DOCUMENTOS_VIGENCIA_DIAS
  list(
    nombre = .TextoSolicitud(datos$Nombre[i]),
    tipo_documento = toupper(.TextoSolicitud(datos$TipoDocumento[i])),
    numero_documento = .TextoSolicitud(datos$NumeroDocumento[i]),
    correo = tolower(.TextoSolicitud(datos$Correo[i])),
    celular = if ("Celular" %in% names(datos)) .TextoSolicitud(datos$Celular[i]) else "",
    documentos = unique(documentos), vigencia_dias = vigencia
  )
}

SolicitudesServer <- function(id, usuario, recarga) {
  shiny::moduleServer(id, function(input, output, session) {
    pendiente_individual <- shiny::reactiveVal(NULL)
    resultado_individual <- shiny::reactiveVal(NULL)
    pendiente_masivo <- shiny::reactiveVal(NULL)
    resultado_masivo <- shiny::reactiveVal(NULL)

    output$compartir_individual <- shiny::renderUI({
      .CompartirSolicitudUI(resultado_individual(), session$ns)
    })

    shiny::observeEvent(input$crear_enviar, {
      datos <- .DatosIndividuales(input)
      cond <- .ValidarSolicitud(datos)
      if (any(cond)) {
        sapply(names(cond)[cond], shiny::showNotification, duration = 4, type = "error")
        return(invisible(NULL))
      }
      pendiente_individual(datos)
      racafeForms::racafeConfirm(
        session, "confirmar_individual", "Crear y enviar solicitud",
        paste("Se enviará el correo a", datos$correo),
        confirm_label = "Crear y enviar", ns = session$ns
      )
    })

    shiny::observeEvent(input$confirmar_individual, {
      if (!isTRUE(input$confirmar_individual) || is.null(pendiente_individual())) {
        return(invisible(NULL))
      }
      datos <- pendiente_individual()
      pendiente_individual(NULL)
      tryCatch({
        resultado <- .CrearEnviarSolicitud(datos, usuario())
        resultado_individual(resultado)
        recarga(recarga() + 1L)
        tipo <- if (resultado$estado_correo == "ENVIADO") "success" else "warning"
        racafeForms::racafeNotify(
          session, "Solicitud creada", resultado$detalle_correo, type = tipo
        )
      }, error = function(e) {
        shiny::showNotification(
          paste("No fue posible crear la solicitud:", conditionMessage(e)),
          duration = 8, type = "error"
        )
      })
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$ampliar_qr, {
      resultado <- resultado_individual()
      shiny::req(resultado)
      shiny::showModal(racafeForms::racafeModal(
        shiny::tags$div(
          align = "center",
          shiny::tags$img(
            src = resultado$qr, alt = paste("QR", resultado$codigo),
            width = "500", height = "500"
          )
        ),
        title = paste("Código QR", resultado$codigo), size = "subventana3",
        footer = racafeForms::racafeModalFooter(
          shiny::modalButton("Cerrar"),
          racafeForms::racafeCopyButton(
            "copiar_qr_modal", "Copiar QR", image = resultado$qr, ns = session$ns
          )
        )
      ))
    })

    shiny::observeEvent(input$ver_correo, {
      resultado <- resultado_individual()
      shiny::req(resultado)
      shiny::showModal(racafeForms::racafeModal(
        shiny::tags$iframe(
          srcdoc = resultado$correo$html, title = "Vista previa del correo",
          width = "100%", height = "600", sandbox = ""
        ),
        title = resultado$correo$asunto, size = "ventana"
      ))
    })

    output$plantilla_masiva <- shiny::downloadHandler(
      filename = function() "plantilla_solicitudes_masivas.xlsx",
      content = function(file) {
        plantilla <- data.frame(
          Nombre = "Empresa Ejemplo SAS", TipoDocumento = "NIT",
          NumeroDocumento = "900123456", Correo = "correo@empresa.com",
          Celular = "3001234567", Documentos = "RUT|CERL|HABEAS", VigenciaDias = 7
        )
        openxlsx::write.xlsx(plantilla, file, overwrite = TRUE)
      }
    )

    shiny::observeEvent(input$procesar_masivo, {
      tryCatch({
        datos <- .LeerSolicitudesMasivas(input$archivo_masivo)
        pendiente_masivo(datos)
        racafeForms::racafeConfirm(
          session, "confirmar_masivo", "Crear solicitudes masivas",
          paste("Se crearán y enviarán", nrow(datos), "solicitudes."),
          confirm_label = "Crear y enviar", ns = session$ns
        )
      }, error = function(e) {
        shiny::showNotification(conditionMessage(e), duration = 7, type = "error")
      })
    })

    shiny::observeEvent(input$confirmar_masivo, {
      if (!isTRUE(input$confirmar_masivo) || is.null(pendiente_masivo())) return(invisible(NULL))
      datos <- pendiente_masivo()
      pendiente_masivo(NULL)
      resultados <- lapply(seq_len(nrow(datos)), function(i) {
        fila <- .FilaSolicitudMasiva(datos, i)
        cond <- .ValidarSolicitud(fila)
        if (any(cond)) {
          return(list(fila = i, estado = "ERROR", detalle = paste(names(cond)[cond], collapse = "; ")))
        }
        tryCatch({
          resultado <- .CrearEnviarSolicitud(fila, usuario())
          list(
            fila = i, estado = resultado$estado_correo, detalle = resultado$detalle_correo,
            resultado = resultado
          )
        }, error = function(e) {
          list(fila = i, estado = "ERROR", detalle = conditionMessage(e))
        })
      })
      resultado_masivo(resultados)
      recarga(recarga() + 1L)
    }, ignoreInit = TRUE)

    output$resultado_masivo <- shiny::renderUI({
      resultados <- resultado_masivo()
      if (is.null(resultados)) {
        return(shiny::p("Cargue el archivo para crear y enviar las solicitudes."))
      }
      estados <- vapply(resultados, `[[`, character(1), "estado")
      errores <- resultados[estados == "ERROR"]
      shiny::tagList(
        racafeForms::racafeInfoGrid(
          racafeForms::racafeInfoItem("Procesadas", length(resultados)),
          racafeForms::racafeInfoItem("Correos enviados", sum(estados == "ENVIADO")),
          racafeForms::racafeInfoItem("Con error", sum(estados == "ERROR"))
        ),
        if (length(errores)) shiny::tags$ul(lapply(errores, function(x) {
          shiny::tags$li(paste("Fila", x$fila, "·", x$detalle))
        }))
      )
    })
  })
}
