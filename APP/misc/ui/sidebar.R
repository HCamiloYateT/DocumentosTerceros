sidebar <- bs4Dash::bs4DashSidebar(
  status = "danger", expandOnHover = FALSE,
  bs4Dash::bs4SidebarMenu(
    id = "menu_principal",
    bs4Dash::bs4SidebarMenuItem("Inicio", tabName = "IN_Inicio", icon = shiny::icon("house"), selected = TRUE),
    bs4Dash::bs4SidebarMenuItem("Solicitudes", tabName = "SO_Solicitudes", icon = shiny::icon("folder-plus")),
    bs4Dash::bs4SidebarMenuItem("Validación documental", tabName = "VA_Documentos",
                                icon = shiny::icon("file-circle-check")),
    bs4Dash::bs4SidebarMenuItem("Expedientes", tabName = "EX_Expedientes", icon = shiny::icon("folder-tree"))
  )
)
