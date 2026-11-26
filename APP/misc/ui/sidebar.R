sidebar <- bs4Dash::bs4DashSidebar(
  status = "danger", expandOnHover = FALSE,
  bs4Dash::bs4SidebarMenu(
    id = "menu_principal",
    bs4Dash::bs4SidebarMenuItem("Inicio", tabName = "IN_Inicio", icon = icon("house"), selected = TRUE),
    bs4Dash::bs4SidebarMenuItem("Solicitudes", tabName = "SO_Solicitudes", icon = icon("folder-plus")),
    bs4Dash::bs4SidebarMenuItem("Validación documental", tabName = "VA_Documentos",
                                icon = icon("file-circle-check")),
    bs4Dash::bs4SidebarMenuItem("Expedientes", tabName = "EX_Expedientes", icon = icon("folder-tree"))
  )
)
