# Módulo de inicio ----
InicioUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(
      class = "documentos-cajas-row",
      fluidRow(
        column(2, cajaValor_ui(ns("creadas"), icono = "folder-plus", texto = "Creadas",
                               mostrar_boton = TRUE, label_boton = "Filtrar", icono_boton = "filter",
                               compact = TRUE)),
        column(2, cajaValor_ui(ns("enviadas"), icono = "paper-plane", texto = "Enviadas",
                               mostrar_boton = TRUE, label_boton = "Filtrar", icono_boton = "filter",
                               compact = TRUE)),
        column(2, cajaValor_ui(ns("pendientes"), icono = "hourglass-half", texto = "Pendientes",
                               mostrar_boton = TRUE, label_boton = "Filtrar", icono_boton = "filter",
                               compact = TRUE)),
        column(2, cajaValor_ui(ns("abiertas"), icono = "link", texto = "Abiertas",
                               mostrar_boton = TRUE, label_boton = "Filtrar", icono_boton = "filter",
                               compact = TRUE)),
        column(2, cajaValor_ui(ns("completas"), icono = "circle-check", texto = "Completas",
                               mostrar_boton = TRUE, label_boton = "Filtrar", icono_boton = "filter",
                               compact = TRUE)),
        column(2, cajaValor_ui(ns("vencidas"), icono = "clock", texto = "Vencidas",
                               mostrar_boton = TRUE, label_boton = "Filtrar", icono_boton = "filter",
                               compact = TRUE))
      )
    ),
    racafePanel("Estado de solicitudes", TablaVancouverUI(ns("tabla"), descargable = TRUE))
  )
}

InicioServer <- function(id, recarga, usuario, grupo) {
  moduleServer(id, function(input, output, session) {
    solicitud_modal <- reactiveVal(NULL)
    enviando_correo <- reactiveVal(FALSE)
    filtro <- reactiveVal("CREADAS")
    loader_correo <- racafeShiny::racafeLoader(session, type = "enviar", text = "Enviando correo…")

    solicitudes <- reactive({
      recarga()
      datos <- ListarSolicitudes()
      if (any(grupo() %in% c("ANALÍTICA", "ANALITICA"))) return(datos)
      dplyr::filter(datos, CreadoPor == usuario())
    })

    datos_tabla <- reactive({
      .inicio_datos_tabla(.inicio_filtrar_solicitudes(solicitudes(), filtro()))
    })

    output$detalle_solicitud <- renderUI({
      codigo <- solicitud_modal()
      req(codigo)
      datos <- solicitudes()
      fila <- match(codigo, datos$CodigoSolicitud)
      req(!is.na(fila))
      .inicio_cronologia_ui(list(data = datos[fila, , drop = FALSE]), session$ns)
    })

    caja_creadas <- cajaValor_server("creadas", reactive({
      .inicio_indicador(rep(TRUE, nrow(solicitudes())))
    }), "numero", formatear = .inicio_formatear_cantidad)
    caja_enviadas <- cajaValor_server("enviadas", reactive({
      .inicio_indicador(!is.na(solicitudes()$EnviadoEn))
    }), "numero", formatear = .inicio_formatear_indicador)
    caja_pendientes <- cajaValor_server("pendientes", reactive({
      .inicio_indicador(!solicitudes()$CanalPrincipal %in% c("CORREO", "QR"))
    }), "numero", formatear = .inicio_formatear_indicador)
    caja_abiertas <- cajaValor_server("abiertas", reactive({
      .inicio_indicador(solicitudes()$TotalAperturas > 0)
    }), "numero", formatear = .inicio_formatear_indicador)
    caja_completas <- cajaValor_server("completas", reactive({
      .inicio_indicador(solicitudes()$PorcentajeCompletitud >= 100)
    }), "numero", formatear = .inicio_formatear_indicador)
    caja_vencidas <- cajaValor_server("vencidas", reactive({
      datos <- solicitudes()
      .inicio_indicador(!is.na(datos$VenceEn) & datos$VenceEn < as.POSIXct(Sys.time(), tz = "UTC") &
                          datos$PorcentajeCompletitud < 100)
    }), "numero", formatear = .inicio_formatear_indicador)

    observeEvent(caja_creadas$click_detalle(), filtro("CREADAS"), ignoreInit = TRUE)
    observeEvent(caja_enviadas$click_detalle(), filtro("ENVIADAS"), ignoreInit = TRUE)
    observeEvent(caja_pendientes$click_detalle(), filtro("PENDIENTES"), ignoreInit = TRUE)
    observeEvent(caja_abiertas$click_detalle(), filtro("ABIERTAS"), ignoreInit = TRUE)
    observeEvent(caja_completas$click_detalle(), filtro("COMPLETAS"), ignoreInit = TRUE)
    observeEvent(caja_vencidas$click_detalle(), filtro("VENCIDAS"), ignoreInit = TRUE)

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
      modal_contenido_fn = function(seleccion) uiOutput(session$ns("detalle_solicitud")),
      modal_size = "ventana",
      footer = reactive(paste("Filtro activo:", .inicio_etiqueta_filtro(filtro())))
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
