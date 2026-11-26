# Módulo de inicio ----
InicioUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      bs4Dash::bs4ValueBoxOutput(ns("solicitudes"), width = 3),
      bs4Dash::bs4ValueBoxOutput(ns("enviadas"), width = 3),
      bs4Dash::bs4ValueBoxOutput(ns("completas"), width = 3),
      bs4Dash::bs4ValueBoxOutput(ns("vencidas"), width = 3)
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

    output$solicitudes <- bs4Dash::renderbs4ValueBox({
      bs4Dash::bs4ValueBox(value = nrow(solicitudes()), subtitle = "Solicitudes", icon = shiny::icon("folder-open"),
                           color = "danger")
    })
    output$enviadas <- bs4Dash::renderbs4ValueBox({
      valor <- sum(solicitudes()$Estado %in% c("ENVIADA", "EN_DILIGENCIAMIENTO"))
      bs4Dash::bs4ValueBox(value = valor, subtitle = "Enviadas", icon = shiny::icon("paper-plane"), color = "info")
    })
    output$completas <- bs4Dash::renderbs4ValueBox({
      valor <- sum(solicitudes()$PorcentajeCompletitud >= 100)
      bs4Dash::bs4ValueBox(value = valor, subtitle = "Completas", icon = shiny::icon("circle-check"),
                           color = "success")
    })
    output$vencidas <- bs4Dash::renderbs4ValueBox({
      datos <- solicitudes()
      valor <- sum(!is.na(datos$VenceEn) & datos$VenceEn < as.POSIXct(Sys.time(), tz = "UTC") &
                     !datos$Estado %in% c("APROBADA", "CANCELADA"))
      bs4Dash::bs4ValueBox(value = valor, subtitle = "Vencidas", icon = shiny::icon("clock"), color = "warning")
    })

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
