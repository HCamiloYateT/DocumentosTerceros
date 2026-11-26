header <- bs4Dash::bs4DashNavbar(
  status = "white", border = FALSE,
  title = bs4Dash::dashboardBrand(title = tit_app, href = "#", image = .logo_url),
  leftUi = tags$li(
    class = "dropdown documentos-user", uiOutput("identidad", inline = TRUE),
    racafeShiny::Boton("BTN_Actualizar", label = NULL, icono = "sync", size = "xxs",
                       label_posicion = "below", titulo = "Actualizar")
  )
)
