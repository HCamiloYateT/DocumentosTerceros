body <- bs4Dash::bs4DashBody(
  .recursos_compartidos,
  racafeShiny::racafeLoaderUI(
    startup = FALSE, busy = TRUE,
    triggers = c(actualizar = "BTN_Actualizar", guardar = "solicitudes-crear_solicitud")
  ),
  bs4Dash::bs4TabItems(
    bs4Dash::bs4TabItem(tabName = "IN_Inicio", tags$main(class = "documentos-main", InicioUI("inicio"))),
    bs4Dash::bs4TabItem(tabName = "SO_Solicitudes",
                        tags$main(class = "documentos-main", SolicitudesUI("solicitudes"))),
    bs4Dash::bs4TabItem(tabName = "FO_Formularios",
                        tags$main(class = "documentos-main", FormulariosUI("formularios"))),
    bs4Dash::bs4TabItem(
      tabName = "VA_Documentos", tags$main(class = "documentos-main",
        racafePanel("Validación documental", p("La integración de carga y OCR corresponde a la fase siguiente.")))
    ),
    bs4Dash::bs4TabItem(
      tabName = "EX_Expedientes", tags$main(class = "documentos-main",
        racafePanel("Expedientes", p("Los expedientes se crean automáticamente al registrar solicitudes.")))
    )
  )
)
