function(input, output, session) {

  # Datos reactivos ----
  usuario <- reactive({
    if (is.null(session$user)) "HCYATE" else str_to_upper(session$user)
  })
  grupo <- reactive({
    if (is.null(session$group)) "ANALÍTICA" else stringr::str_to_upper(session$group)
  })
  # Outputs ----
  ## Header ----
  output$user <- renderUI({
    FormatearTexto(paste(usuario()) %>% HTML, negrita = T, tamano_pct = 0.75, alineacion = "center", color = "#999")
  })
  ## Cuerpo ----
  Camaras("camaras")
  RutServer("mod_rut")
  }

