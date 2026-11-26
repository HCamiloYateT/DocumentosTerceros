function(input, output, session) {
  # Estado de sesión ----
  recarga <- reactiveVal(0L)
  apertura_registrada <- reactiveVal(character())
  usuario <- reactive({
    valor <- session$user
    if (is.null(valor) || !length(valor) || is.na(valor[1L]) || !nzchar(trimws(valor[1L]))) return("HCYATE")
    stringr::str_to_upper(trimws(as.character(valor[1L])))
  })
  grupo <- reactive({
    valor <- session$groups
    if (is.null(valor) || !length(valor)) return("SIN GRUPO")
    unique(stringr::str_to_upper(trimws(as.character(valor))))
  })

  output$identidad <- renderUI({
    badges <- lapply(grupo(), function(x) tags$span(class = "badge badge-secondary ml-1", x))
    tagList(
      tags$span(class = "text-muted", usuario()),
      tags$span(class = "text-muted", " | "),
      badges
    )
  })

  observeEvent(input$BTN_Actualizar, {
    recarga(recarga() + 1L)
    racafeNotify(session, "Información actualizada", "Se recargaron los índices locales.", type = "success")
  })

  observeEvent(session$clientData$url_search, {
    parametros <- parseQueryString(sub("^\\?", "", session$clientData$url_search %||% ""))
    codigo <- parametros$solicitud %||% ""
    if (!nzchar(codigo) || codigo %in% apertura_registrada()) return(invisible(NULL))
    tryCatch({
      RegistrarAperturaSolicitud(codigo)
      apertura_registrada(c(apertura_registrada(), codigo))
      recarga(recarga() + 1L)
    }, error = function(e) invisible(NULL))
  }, ignoreInit = FALSE)

  # Módulos ----
  InicioServer("inicio", recarga, usuario, grupo)
  SolicitudesServer("solicitudes", usuario, recarga)
}
