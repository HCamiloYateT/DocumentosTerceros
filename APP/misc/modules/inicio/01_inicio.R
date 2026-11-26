# Módulo de inicio ----
InicioUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(
      class = "documentos-cajas-row",
      fluidRow(
        column(3, cajaValor_ui(ns("enviadas"), icono = "paper-plane", texto = "Enviadas",
                               mostrar_boton = FALSE, compact = TRUE)),
        column(3, cajaValor_ui(ns("abiertas"), icono = "link", texto = "Abiertas",
                               mostrar_boton = FALSE, compact = TRUE)),
        column(3, cajaValor_ui(ns("completas"), icono = "circle-check", texto = "Completas",
                               mostrar_boton = FALSE, compact = TRUE)),
        column(3, cajaValor_ui(ns("vencidas"), icono = "clock", texto = "Vencidas",
                               mostrar_boton = FALSE, compact = TRUE))
      )
    ),
    racafePanel("Estado de solicitudes", TablaVancouverUI(ns("tabla"), descargable = TRUE))
  )
}

InicioServer <- function(id, recarga) {
  moduleServer(id, function(input, output, session) {
    solicitudes <- reactive({
      recarga()
      ListarSolicitudes()
    })

    datos_tabla <- reactive(.inicio_datos_tabla(solicitudes()))

    cajaValor_server("enviadas", reactive({
      .inicio_porcentaje(!is.na(solicitudes()$EnviadoEn))
    }), "numero", formatear = .inicio_formatear_porcentaje)
    cajaValor_server("abiertas", reactive({
      .inicio_porcentaje(solicitudes()$TotalAperturas > 0)
    }), "numero", formatear = .inicio_formatear_porcentaje)
    cajaValor_server("completas", reactive({
      .inicio_porcentaje(solicitudes()$PorcentajeCompletitud >= 100)
    }), "numero", formatear = .inicio_formatear_porcentaje)
    cajaValor_server("vencidas", reactive({
      datos <- solicitudes()
      .inicio_porcentaje(!is.na(datos$VenceEn) & datos$VenceEn < as.POSIXct(Sys.time(), tz = "UTC") &
                           datos$PorcentajeCompletitud < 100)
    }), "numero", formatear = .inicio_formatear_porcentaje)

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
      modal_contenido_fn = .inicio_cronologia_ui,
      modal_size = "ventana"
    )
  })
}
