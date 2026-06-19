sidebar <- bs4DashSidebar(status = "danger", expandOnHover = FALSE,
                          bs4SidebarMenu(id = "sidebar_menu",
                                         bs4SidebarHeader("Registro Mercantil"),
                                         bs4SidebarMenuItem(text    = "Certificados CERL",
                                                            tabName = "camaras",
                                                            icon    = icon("building-columns")
                                                            ),
                                         bs4SidebarHeader("Tributario"),
                                         bs4SidebarMenuItem(text = "RUT DIAN",
                                                            tabName = "rut",
                                                            icon = icon("file-invoice")
                                                            )
                                         )
                          )
