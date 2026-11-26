# Módulo de inicio ----
InicioUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(
      class = "documentos-cajas-row",
      fluidRow(
        column(3, cajaValor_ui(ns("solicitudes"), icono = "folder-open", texto = "Solicitudes",
                               mostrar_boton = FALSE, compact = TRUE)),
        column(3, cajaValor_ui(ns("enviadas"), icono = "paper-plane", texto = "Enviadas",
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

    cajaValor_server("solicitudes", reactive(nrow(solicitudes())), "numero",
                     formatear = .inicio_formatear_entero)
    cajaValor_server("enviadas", reactive({
      sum(solicitudes()$Estado %in% .INICIO_ESTADOS_ENVIADOS)
    }), "numero", formatear = .inicio_formatear_entero)
    cajaValor_server("completas", reactive({
      sum(solicitudes()$PorcentajeCompletitud >= 100)
    }), "numero", formatear = .inicio_formatear_entero)
    cajaValor_server("vencidas", reactive({
      datos <- solicitudes()
      sum(!is.na(datos$VenceEn) & datos$VenceEn < as.POSIXct(Sys.time(), tz = "UTC") &
            !datos$Estado %in% .INICIO_ESTADOS_CERRADOS)
    }), "numero", formatear = .inicio_formatear_entero)

    TablaVancouver(
      "tabla", data = datos_tabla, page_size = 10, nombre_archivo = "estado_solicitudes",
      col_labels = c(CreadoEn = "Creada", VenceEn = "Vence"),
      col_specs = list(
        PorcentajeCompletitud = list(name = "Completitud", format = list(digits = 1, suffix = "%"))
      )
    )
  })
}
