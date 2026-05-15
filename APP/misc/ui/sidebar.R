sidebar <- bs4DashSidebar(status = "danger", expandOnHover = FALSE,
                          bs4SidebarMenu(id = "sidebar_menu",
                                         bs4SidebarHeader("Registro Mercantil"),
                                         bs4SidebarMenuItem(text = "Cámara de Comercio", tabName = "ccb", 
                                                            icon = icon("building-columns"))
                                         )
                          )
