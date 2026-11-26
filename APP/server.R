function(input, output, session) {
  # Estado de sesión ----
  recarga <- shiny::reactiveVal(0L)
  usuario <- shiny::reactive({
    valor <- session$user
    if (is.null(valor) || !length(valor) || is.na(valor[1L]) || !nzchar(trimws(valor[1L]))) return("HCYATE")
    stringr::str_to_upper(trimws(as.character(valor[1L])))
  })
  grupo <- shiny::reactive({
    valor <- session$groups
    if (is.null(valor) || !length(valor)) return("ANALÍTICA")
    unique(stringr::str_to_upper(trimws(as.character(valor))))
  })

  output$identidad <- shiny::renderUI({
    badges <- lapply(grupo(), function(x) shiny::tags$span(class = "badge badge-secondary ml-1", x))
    shiny::tagList(
      shiny::tags$span(class = "text-muted", usuario()),
      shiny::tags$span(class = "text-muted", " | "),
      badges
    )
  })

  shiny::observeEvent(input$BTN_Actualizar, {
    recarga(recarga() + 1L)
    racafeForms::racafeNotify(session, "Información actualizada", "Se recargaron los índices locales.",
                              type = "success")
  })

  # Módulos ----
  InicioServer("inicio", recarga)
  SolicitudesServer("solicitudes", usuario, recarga)
}
