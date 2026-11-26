# Módulo de inicio ----
InicioUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(
      class = "documentos-cajas-row",
      fluidRow(
        column(2, cajaValor_ui(ns("creadas"), icono = "folder-plus", texto = "Creadas",
                               mostrar_boton = FALSE, compact = TRUE)),
        column(2, cajaValor_ui(ns("enviadas"), icono = "paper-plane", texto = "Enviadas",
                               mostrar_boton = FALSE, compact = TRUE)),
        column(2, cajaValor_ui(ns("pendientes"), icono = "hourglass-half", texto = "Pendientes",
                               mostrar_boton = FALSE, compact = TRUE)),
        column(2, cajaValor_ui(ns("abiertas"), icono = "link", texto = "Abiertas",
                               mostrar_boton = FALSE, compact = TRUE)),
        column(2, cajaValor_ui(ns("completas"), icono = "circle-check", texto = "Completas",
                               mostrar_boton = FALSE, compact = TRUE)),
        column(2, cajaValor_ui(ns("vencidas"), icono = "clock", texto = "Vencidas",
                               mostrar_boton = FALSE, compact = TRUE))
      )
    ),
    racafePanel("Estado de solicitudes", TablaVancouverUI(ns("tabla"), descargable = TRUE))
  )
}

InicioServer <- function(id, recarga, usuario) {
  moduleServer(id, function(input, output, session) {
    solicitud_modal <- reactiveVal(NULL)
    enviando_correo <- reactiveVal(FALSE)
    loader_correo <- racafeShiny::racafeLoader(session, type = "enviar", text = "Enviando correo…")

    solicitudes <- reactive({
      recarga()
      ListarSolicitudes()
    })

    datos_tabla <- reactive(.inicio_datos_tabla(solicitudes()))

    cajaValor_server("creadas", reactive({
      .inicio_indicador(rep(TRUE, nrow(solicitudes())))
    }), "numero", formatear = .inicio_formatear_cantidad)
    cajaValor_server("enviadas", reactive({
      .inicio_indicador(!is.na(solicitudes()$EnviadoEn))
    }), "numero", formatear = .inicio_formatear_indicador)
    cajaValor_server("pendientes", reactive({
      .inicio_indicador(!solicitudes()$CanalPrincipal %in% c("CORREO", "QR"))
    }), "numero", formatear = .inicio_formatear_indicador)
    cajaValor_server("abiertas", reactive({
      .inicio_indicador(solicitudes()$TotalAperturas > 0)
    }), "numero", formatear = .inicio_formatear_indicador)
    cajaValor_server("completas", reactive({
      .inicio_indicador(solicitudes()$PorcentajeCompletitud >= 100)
    }), "numero", formatear = .inicio_formatear_indicador)
    cajaValor_server("vencidas", reactive({
      datos <- solicitudes()
      .inicio_indicador(!is.na(datos$VenceEn) & datos$VenceEn < as.POSIXct(Sys.time(), tz = "UTC") &
                          datos$PorcentajeCompletitud < 100)
    }), "numero", formatear = .inicio_formatear_indicador)

    TablaVancouver(
      "tabla", data = datos_tabla, page_size = 10, nombre_archivo = "estado_solicitudes",
      id_col = "CodigoSolicitud", modo_seleccion = "fila",
      col_labels = c(CreadoPor = "Creada por", CreadoEn = "Creada", CanalPrincipal = "Canal",
                     EnviadoEn = "Enviada", AbiertoEn = "Primera apertura", TotalAperturas = "Aperturas",
                     VenceEn = "Vence"),
      col_specs = list(
        PorcentajeCompletitud = list(name = "Completitud", format = list(digits = 1, suffix = "%"))
      ),
      modal_titulo_fn = function(seleccion) paste("Cronología", seleccion$id),
      modal_pre_fn = function(seleccion) solicitud_modal(seleccion$id),
      modal_contenido_fn = function(seleccion) .inicio_cronologia_ui(seleccion, session$ns),
      modal_size = "ventana"
    )

    registrar_copia <- function(resultado_copia, tipo) {
      .sol_notificar_copia(session, resultado_copia, tipo)
      if (!isTRUE(resultado_copia$success) || is.null(solicitud_modal())) return(invisible(NULL))
      tryCatch({
        resultado <- .inicio_resultado_compartir(solicitud_modal())
        .sol_registrar_comparticion(resultado, usuario(), tipo)
        recarga(recarga() + 1L)
      }, error = function(e) {
        racafeNotify(session, "Copia realizada sin registro", conditionMessage(e), type = "warning")
      })
      invisible(NULL)
    }

    observeEvent(input$inicio_copiar_enlace_copy_result, {
      registrar_copia(input$inicio_copiar_enlace_copy_result, "enlace")
    }, ignoreInit = TRUE)

    observeEvent(input$inicio_copiar_qr_copy_result, {
      registrar_copia(input$inicio_copiar_qr_copy_result, "qr")
    }, ignoreInit = TRUE)

    observeEvent(input$inicio_enviar_correo, {
      resultado <- .inicio_resultado_compartir(solicitud_modal())
      racafeConfirm(session, "inicio_confirmar_envio", "Enviar correo",
                    paste("Se enviará el correo a", resultado$correo_destino),
                    confirm_label = "Enviar", ns = session$ns)
    })

    observeEvent(input$inicio_confirmar_envio, {
      if (!isTRUE(input$inicio_confirmar_envio) || is.null(solicitud_modal()) || enviando_correo()) {
        return(invisible(NULL))
      }
      enviando_correo(TRUE)
      loader_correo$show()
      tryCatch({
        resultado <- .sol_enviar(.inicio_resultado_compartir(solicitud_modal()), usuario())
        recarga(recarga() + 1L)
        tipo <- if (identical(resultado$estado_correo, "ENVIADO")) "success" else "warning"
        racafeNotify(session, "Resultado del envío", resultado$detalle_correo, type = tipo)
      }, error = function(e) {
        racafeNotify(session, "No fue posible enviar el correo", conditionMessage(e), type = "error")
      }, finally = {
        enviando_correo(FALSE)
        loader_correo$hide()
      })
    }, ignoreInit = TRUE)
  })
}
