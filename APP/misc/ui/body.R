body <- bs4DashBody(
  includeCSS("https://raw.githubusercontent.com/HCamiloYateT/Compartido/refs/heads/main/Styles/style.css"),
  use_waiter(),
  useShinyjs(),
  bs4TabItems(
    bs4TabItem(tabName = "camaras",
               fluidRow(
                 column(width = 12,
                        tags$div(class = "d-flex flex-column mb-3",
                                 tags$h4(class = "mb-1",
                                         "Certificado de Existencia y Representación Legal"),
                                 tags$small(class = "text-muted",
                                            "Multi-cámara — texto y escaneados")
                                 )
                        )
                 ),
               CamarasUI("camaras")
               )
    )
  )
