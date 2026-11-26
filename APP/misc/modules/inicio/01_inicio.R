# Módulo de inicio ----
InicioUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(3, racafeModulos::cajaValor_ui(ns("solicitudes"), icono = "folder-open",
                                                    texto = "Solicitudes", mostrar_boton = FALSE, compact = TRUE)),
      shiny::column(3, racafeModulos::cajaValor_ui(ns("enviadas"), icono = "paper-plane",
                                                    texto = "Enviadas", mostrar_boton = FALSE, compact = TRUE)),
      shiny::column(3, racafeModulos::cajaValor_ui(ns("completas"), icono = "circle-check",
                                                    texto = "Completas", mostrar_boton = FALSE, compact = TRUE)),
      shiny::column(3, racafeModulos::cajaValor_ui(ns("vencidas"), icono = "clock",
                                                    texto = "Vencidas", mostrar_boton = FALSE, compact = TRUE))
    ),
    racafeForms::racafePanel("Estado de solicitudes", reactable::reactableOutput(ns("tabla")))
  )
}

InicioServer <- function(id, recarga) {
  shiny::moduleServer(id, function(input, output, session) {
    solicitudes <- shiny::reactive({
      recarga()
      ListarSolicitudes()
    })

    formatear_entero <- function(valor) shiny::tags$span(format(valor, big.mark = ".", scientific = FALSE))
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

    output$tabla <- reactable::renderReactable({
      datos <- solicitudes() %>%
        dplyr::select(CodigoSolicitud, CodigoTercero, Responsable, Estado, CreadoEn, VenceEn,
                      PorcentajeCompletitud)
      reactable::reactable(
        datos, searchable = TRUE, pagination = TRUE, defaultPageSize = 10, highlight = TRUE,
        columns = list(
          CreadoEn = reactable::colDef(name = "Creada", format = reactable::colFormat(datetime = TRUE)),
          VenceEn = reactable::colDef(name = "Vence", format = reactable::colFormat(datetime = TRUE)),
          PorcentajeCompletitud = reactable::colDef(
            name = "Completitud", format = reactable::colFormat(digits = 1, suffix = "%")
          )
        ), language = reactable::reactableLang(searchPlaceholder = "Buscar", noData = "No hay solicitudes")
      )
    })
  })
}
