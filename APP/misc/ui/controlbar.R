controlbar <- bs4DashControlbar(id = "controlbar", skin = "light", pinned = NULL,
                                overlay = FALSE, width = "500px", type = "tabs",
                                title = "Control",
                                controlbarMenu(id = "Filtros", type = "tabs",
                                               controlbarItem("Filtros",
                                                              h6("Prueba")
                                                              )
                                               )
                                )
