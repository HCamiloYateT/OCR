body <- bs4DashBody(
  includeCSS("https://raw.githubusercontent.com/HCamiloYateT/Compartido/refs/heads/main/Styles/style.css"),
  use_waiter(),
  useShinyjs(),
  bs4TabItems(
    # Cámara de Comercio de Bogotá ----
    bs4TabItem(tabName = "ccb",
      fluidRow(column(width = 12,
                      tags$div(class = "d-flex align-items-center mb-3", 
                               tags$div(
                                 tags$h4(class = "mb-0",
                                         "Certificado de Existencia y Representación Legal"),
                                 tags$small(class = "text-muted",
                                            "Cámara de Comercio de Bogotá"
                                            )
                                 )
                               )
                      )
               ),
      CCBUI("ccb")
      )
    )
  )
