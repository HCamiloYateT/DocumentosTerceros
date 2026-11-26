# Layout bs4Dash ----
ui <- bs4Dash::bs4DashPage(
  title = tit_app, dark = NULL, help = NULL, controlbar = NULL, scrollToTop = TRUE,
  preloader = racafeShiny::racafePreloader(type = "inicio", text = "Cargando aplicación…"),
  header = header, sidebar = sidebar, body = body, footer = footer
)

shiny::tagList(htmltools::suppressDependencies("ionicons", "googlefonts"), ui)
