body <- bs4Dash::bs4DashBody(
  shiny::includeCSS("www/app.css"),
  .recursos_compartidos,
  racafeShiny::racafeLoaderUI(startup = FALSE, busy = TRUE,
                              triggers = c(actualizar = "BTN_Actualizar", crear = "solicitudes-crear",
                                           enviar = "solicitudes-registrar_envio")),
  bs4Dash::bs4TabItems(
    bs4Dash::bs4TabItem(tabName = "IN_Inicio", shiny::tags$main(class = "documentos-main", InicioUI("inicio"))),
    bs4Dash::bs4TabItem(tabName = "SO_Solicitudes",
                        shiny::tags$main(class = "documentos-main", SolicitudesUI("solicitudes"))),
    bs4Dash::bs4TabItem(
      tabName = "VA_Documentos", shiny::tags$main(class = "documentos-main",
        racafeForms::racafePanel("Validación documental",
                                 shiny::p("La integración de carga y OCR corresponde a la fase siguiente.")))
    ),
    bs4Dash::bs4TabItem(
      tabName = "EX_Expedientes", shiny::tags$main(class = "documentos-main",
        racafeForms::racafePanel("Expedientes",
                                 shiny::p("Los expedientes se crean automáticamente al registrar solicitudes.")))
    )
  )
)
