# Módulo de inicio ----
InicioUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$div(
      class = "documentos-cajas-row",
      shiny::fluidRow(
        shiny::column(3, racafeModulos::cajaValor_ui(ns("solicitudes"), icono = "folder-open",
                                                      texto = "Solicitudes", mostrar_boton = FALSE, compact = TRUE)),
        shiny::column(3, racafeModulos::cajaValor_ui(ns("enviadas"), icono = "paper-plane",
                                                      texto = "Enviadas", mostrar_boton = FALSE, compact = TRUE)),
        shiny::column(3, racafeModulos::cajaValor_ui(ns("completas"), icono = "circle-check",
                                                      texto = "Completas", mostrar_boton = FALSE, compact = TRUE)),
        shiny::column(3, racafeModulos::cajaValor_ui(ns("vencidas"), icono = "clock",
                                                      texto = "Vencidas", mostrar_boton = FALSE, compact = TRUE))
      )
    ),
    racafeForms::racafePanel(
      "Estado de solicitudes", racafeModulos::TablaVancouverUI(ns("tabla"), descargable = TRUE)
    )
  )
}
InicioServer <- function(id, recarga) {
  shiny::moduleServer(id, function(input, output, session) {
    solicitudes <- shiny::reactive({
      recarga()
      ListarSolicitudes()
    })

    formatear_entero <- function(valor) {
      shiny::tags$span(formatC(valor, format = "f", digits = 0, big.mark = ".", decimal.mark = ","))
    }
    racafeModulos::cajaValor_server("solicitudes", shiny::reactive(nrow(solicitudes())), "numero",
                                    formatear = formatear_entero)
    racafeModulos::cajaValor_server("enviadas", shiny::reactive({
      sum(solicitudes()$Estado %in% c("ENVIADA", "EN_DILIGENCIAMIENTO"))
    }), "numero", formatear = formatear_entero)
    racafeModulos::cajaValor_server("completas", shiny::reactive({
      sum(solicitudes()$PorcentajeCompletitud >= 100)
    }), "numero", formatear = formatear_entero)
    racafeModulos::cajaValor_server("vencidas", shiny::reactive({
      datos <- solicitudes()
      sum(!is.na(datos$VenceEn) & datos$VenceEn < as.POSIXct(Sys.time(), tz = "UTC") &
            !datos$Estado %in% c("APROBADA", "CANCELADA"))
    }), "numero", formatear = formatear_entero)

    datos_tabla <- shiny::reactive({
      solicitudes() %>%
        dplyr::select(CodigoSolicitud, CodigoTercero, Responsable, Estado, CreadoEn, VenceEn,
                      PorcentajeCompletitud)
    })

    racafeModulos::TablaVancouver(
      "tabla", data = datos_tabla, page_size = 10, nombre_archivo = "estado_solicitudes",
      col_labels = c(CreadoEn = "Creada", VenceEn = "Vence"),
      col_specs = list(
        PorcentajeCompletitud = list(
          name = "Completitud", format = list(digits = 1, suffix = "%")
        )
      )
    )
  })
}
