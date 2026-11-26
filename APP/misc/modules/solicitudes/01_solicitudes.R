# Módulo de solicitudes ----
.sol_individual_ui <- function(ns) {
  tagList(
    tags$div(class = "documentos-cajas-row"),
    fluidRow(
      column(
        width = 6,
        racafePanel(
          "Datos del tercero",
          racafeForm(
            id = "solicitud_individual", ns = ns, variant = "plain",
            racafeInputText("nombre", "Nombre completo o razón social", required = TRUE, ns = ns),
            racafeFormRow(
              columns = 2, responsive = "auto",
              racafeInputSelect("tipo_documento", "Tipo de documento", choices = .SOL_TIPOS_DOCUMENTO,
                                selected = "NIT", empty = FALSE, ns = ns),
              racafeInputText("numero_documento", "Número de documento", required = TRUE, ns = ns)
            ),
            racafeFormRow(
              columns = 2, responsive = "auto",
              racafeInputText("correo", "Correo electrónico", type = "email", required = TRUE, ns = ns),
              racafeInputText("celular", "Celular", type = "tel", ns = ns)
            ),
            racafeInputSelect("documentos", "Documentos a incluir", choices = .SOL_DOCUMENTOS,
                              selected = unname(.SOL_DOCUMENTOS), multiple = TRUE, empty = FALSE,
                              gender = "m", ns = ns),
            racafeInputNumeric("vigencia_dias", "Vigencia del enlace en días", value = DOCUMENTOS_VIGENCIA_DIAS,
                               format = "integer", min = 1, max = 30, ns = ns),
            racafeFormFooter(racafeButton("crear_solicitud", "Crear solicitud", icon = "folder-plus", ns = ns))
          )
        )
      ),
      column(width = 6, racafePanel("Compartir con el tercero", uiOutput(ns("compartir_individual"))))
    )
  )
}

.sol_masiva_ui <- function(ns) {
  tagList(
    tags$div(class = "documentos-cajas-row"),
    fluidRow(
      column(
        width = 6,
        racafePanel(
          "Archivo de solicitudes",
          racafeInputFile("archivo_masivo", "Archivo Excel", accept = ".xlsx",
                          max_size = .SOL_MAX_ARCHIVO, help = "Máximo 100 solicitudes por archivo.", ns = ns),
          p("Columnas: Nombre, TipoDocumento, NumeroDocumento, Correo, Celular, Documentos y VigenciaDias. ",
            "Separe varios documentos con |."),
          racafeFormFooter(
            racafeButtonDownload("plantilla_masiva", "Descargar plantilla", icon = "file-excel", ns = ns),
            racafeButton("procesar_masivo", "Crear y enviar solicitudes", icon = "paper-plane", ns = ns)
          )
        )
      ),
      column(width = 6, racafePanel("Resultado del envío", uiOutput(ns("resultado_masivo"))))
    )
  )
}

SolicitudesUI <- function(id) {
  ns <- NS(id)
  bs4Dash::tabsetPanel(
    tabPanel("Creación individual", .sol_individual_ui(ns), value = "individual"),
    tabPanel("Creación masiva", .sol_masiva_ui(ns), value = "masiva"),
    id = ns("modo_solicitud"), selected = "individual", type = "tabs"
  )
}

.sol_compartir_ui <- function(resultado, ns) {
  if (is.null(resultado)) {
    return(tagList(
      p("No se han creado solicitudes en esta sesión."),
      p("Cree una solicitud para generar el enlace, el código QR y la vista previa del correo.")
    ))
  }
  variante <- switch(resultado$estado_correo, ENVIADO = "success", ERROR = "danger", "warning")
  enviar <- if (!identical(resultado$estado_correo, "ENVIADO")) {
    racafeButton("enviar_correo", "Enviar correo", icon = "paper-plane", ns = ns)
  }
  racafeStack(
    racafeInfoGrid(
      racafeInfoItem("Solicitud", resultado$codigo),
      racafeInfoItem("Vigencia", paste(resultado$vencimiento, "(hora de Colombia)"))
    ),
    racafeDetailSection(
      "Código QR",
      tags$img(src = resultado$qr, alt = paste("QR", resultado$codigo), width = "180", height = "180"),
      racafeActionBar(
        left = tagList(
          racafeCopyButton("copiar_qr", "Copiar QR", image = resultado$qr, notify = TRUE, ns = ns),
          racafeButton("ampliar_qr", "Ampliar QR", icon = "expand", variant = "secondary", ns = ns)
        )
      )
    ),
    racafeDetailSection(
      "Enlace para compartir",
      racafeInputReadOnly("enlace_solicitud", "Enlace", value = resultado$enlace,
                          copyable = TRUE, multiline = FALSE, ns = ns)
    ),
    racafeDetailSection(
      "Correo",
      racafeStatusBadge(resultado$estado_correo, variant = variante),
      p(resultado$correo_destino),
      p(resultado$detalle_correo),
      racafeActionBar(
        right = tagList(
          racafeButton("ver_correo", "Previsualizar correo", icon = "envelope",
                       variant = "secondary", ns = ns),
          enviar
        )
      )
    ),
    spacing = "compact"
  )
}

SolicitudesServer <- function(id, usuario, recarga) {
  moduleServer(id, function(input, output, session) {
    pendiente_individual <- reactiveVal(NULL)
    solicitud_activa <- reactiveVal(NULL)
    pendiente_masivo <- reactiveVal(NULL)
    resultado_masivo <- reactiveVal(NULL)
    enviando_correo <- reactiveVal(FALSE)
    loader_correo <- racafeShiny::racafeLoader(session, type = "enviar", text = "Enviando correo…")

    output$compartir_individual <- renderUI(.sol_compartir_ui(solicitud_activa(), session$ns))

    observeEvent(input$crear_solicitud, {
      datos <- .sol_datos_individuales(input)
      cond <- .sol_validar(datos)
      if (any(cond)) return(.sol_mostrar_errores(cond))
      pendiente_individual(datos)
      racafeConfirm(session, "confirmar_creacion", "Crear solicitud",
                    paste("Se creará la solicitud para", datos$nombre),
                    confirm_label = "Crear", ns = session$ns)
    })

    observeEvent(input$confirmar_creacion, {
      if (!isTRUE(input$confirmar_creacion) || is.null(pendiente_individual())) return(invisible(NULL))
      datos <- pendiente_individual()
      pendiente_individual(NULL)
      tryCatch({
        solicitud_activa(.sol_crear(datos, usuario()))
        recarga(recarga() + 1L)
        racafeNotify(session, "Solicitud creada", "Ya puede compartirla desde el panel derecho.",
                     type = "success")
      }, error = function(e) {
        racafeNotify(session, "No fue posible crear la solicitud", conditionMessage(e), type = "error")
      })
    }, ignoreInit = TRUE)

    observeEvent(input$enviar_correo, {
      resultado <- solicitud_activa()
      req(resultado)
      racafeConfirm(session, "confirmar_envio", "Enviar correo",
                    paste("Se enviará el correo a", resultado$correo_destino),
                    confirm_label = "Enviar", ns = session$ns)
    })

    observeEvent(input$confirmar_envio, {
      if (!isTRUE(input$confirmar_envio) || is.null(solicitud_activa()) || enviando_correo()) {
        return(invisible(NULL))
      }
      enviando_correo(TRUE)
      loader_correo$show()
      session$onFlushed(function() {
        tryCatch({
          resultado <- .sol_enviar(solicitud_activa(), usuario())
          solicitud_activa(resultado)
          recarga(recarga() + 1L)
          tipo <- if (identical(resultado$estado_correo, "ENVIADO")) "success" else "warning"
          racafeNotify(session, "Resultado del envío", resultado$detalle_correo, type = tipo)
        }, error = function(e) {
          racafeNotify(session, "No fue posible enviar el correo", conditionMessage(e), type = "error")
        }, finally = {
          enviando_correo(FALSE)
          loader_correo$hide()
        })
      }, once = TRUE)
    }, ignoreInit = TRUE)

    observeEvent(input$ampliar_qr, {
      resultado <- solicitud_activa()
      req(resultado)
      showModal(racafeModal(
        tags$div(align = "center", tags$img(src = resultado$qr, alt = paste("QR", resultado$codigo),
                                           width = "500", height = "500")),
        title = paste("Código QR", resultado$codigo), size = "subventana3",
        footer = racafeModalFooter(
          modalButton("Cerrar"),
          racafeCopyButton("copiar_qr_modal", "Copiar QR", image = resultado$qr,
                           notify = TRUE, ns = session$ns)
        )
      ))
    })

    observeEvent(input$ver_correo, {
      resultado <- solicitud_activa()
      req(resultado)
      showModal(racafeModal(
        tags$iframe(srcdoc = resultado$correo$html, title = "Vista previa del correo",
                    width = "100%", height = "600", sandbox = ""),
        title = resultado$correo$asunto, size = "ventana"
      ))
    })

    output$plantilla_masiva <- downloadHandler(
      filename = function() "plantilla_solicitudes_masivas.xlsx",
      content = function(file) {
        plantilla <- data.frame(
          Nombre = "Empresa Ejemplo SAS", TipoDocumento = "NIT", NumeroDocumento = "900123456",
          Correo = "correo@empresa.com", Celular = "3001234567",
          Documentos = "RUT|CERL|HABEAS", VigenciaDias = 7
        )
        openxlsx::write.xlsx(plantilla, file, overwrite = TRUE)
      }
    )

    observeEvent(input$procesar_masivo, {
      tryCatch({
        datos <- .sol_leer_masivas(input$archivo_masivo)
        pendiente_masivo(datos)
        racafeConfirm(session, "confirmar_masivo", "Crear solicitudes masivas",
                      paste("Se crearán y enviarán", nrow(datos), "solicitudes."),
                      confirm_label = "Crear y enviar", ns = session$ns)
      }, error = function(e) {
        racafeNotify(session, "No fue posible leer el archivo", conditionMessage(e), type = "error")
      })
    })

    observeEvent(input$confirmar_masivo, {
      if (!isTRUE(input$confirmar_masivo) || is.null(pendiente_masivo())) return(invisible(NULL))
      datos <- pendiente_masivo()
      pendiente_masivo(NULL)
      resultados <- lapply(seq_len(nrow(datos)), function(i) {
        fila <- .sol_fila_masiva(datos, i)
        cond <- .sol_validar(fila)
        if (any(cond)) {
          return(list(fila = i, estado = "ERROR", detalle = paste(names(cond)[cond], collapse = "; ")))
        }
        tryCatch({
          resultado <- .sol_enviar(.sol_crear(fila, usuario()), usuario())
          list(fila = i, estado = resultado$estado_correo, detalle = resultado$detalle_correo)
        }, error = function(e) {
          list(fila = i, estado = "ERROR", detalle = conditionMessage(e))
        })
      })
      resultado_masivo(resultados)
      recarga(recarga() + 1L)
    }, ignoreInit = TRUE)

    output$resultado_masivo <- renderUI({
      resultados <- resultado_masivo()
      if (is.null(resultados)) return(p("Cargue el archivo para crear y enviar las solicitudes."))
      estados <- vapply(resultados, `[[`, character(1), "estado")
      errores <- resultados[estados == "ERROR"]
      tagList(
        racafeInfoGrid(
          racafeInfoItem("Procesadas", length(resultados)),
          racafeInfoItem("Correos enviados", sum(estados == "ENVIADO")),
          racafeInfoItem("Con error", sum(estados == "ERROR"))
        ),
        if (length(errores)) tags$ul(lapply(errores, function(x) {
          tags$li(paste("Fila", x$fila, "·", x$detalle))
        }))
      )
    })
  })
}
