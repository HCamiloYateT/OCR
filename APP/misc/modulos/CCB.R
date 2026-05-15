CCBUI <- function(id) {
  ns <- NS(id)
  tagList(
    # Fila superior: carga y estado de validación
    fluidRow(
      # Tarjeta de carga del PDF
      column(width = 4,
             bs4Card(title = tagList(icon("file-pdf"), "Cargar certificado"), width = NULL,
                     status = "white", solidHeader = TRUE, collapsible = FALSE,
                     fileInput(inputId     = ns("pdf_file"),
                               label       = "Seleccione el certificado CCB (PDF)",
                               accept      = ".pdf",
                               multiple    = FALSE,
                               buttonLabel = tagList(icon("upload")),
                               placeholder = "Ningún archivo seleccionado"),
                     # Resumen de metadatos del documento cargado
                     uiOutput(ns("meta_box"))
             )
      ),
      # Tarjeta de estado de validación
      column(width = 8,
             bs4Card(title = tagList("Estado de validación"),
                     width = NULL, status = "white", solidHeader = TRUE, collapsible = FALSE,
                     uiOutput(ns("validacion_ui"))
             )
      )
    ),
    # Fila de resultados: tabla + descarga (solo visible si extracción exitosa)
    uiOutput(ns("resultados_ui"))
  )
}
CCB <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Resultado completo de validación del archivo cargado
    rv_validacion <- reactive({
      req(input$pdf_file)
      path <- input$pdf_file$datapath
      validar_pdf_ccb(path)
    })
    # Extracción de administradores (solo si validación OK)
    rv_extraccion <- reactive({
      req(input$pdf_file)
      val <- rv_validacion()
      req(val$ok)
      extraer_administradores_ccb(input$pdf_file$datapath)
    })
    
    # Metadatos del archivo cargado (nombre, peso)
    output$meta_box <- renderUI({
      req(input$pdf_file)
      f    <- input$pdf_file
      val  <- rv_validacion()
      peso <- round(f$size / 1024, 1)
      
      tags$div(
        class = "small text-muted mt-2",
        tags$b("Archivo: "), f$name, tags$br(),
        tags$b("Tamaño: "), paste0(peso, " KB"), tags$br(),
        if (val$ok) {
          tagList(
            tags$b("Páginas: "), val$paginas, tags$br(),
            tags$b("Caracteres: "),
            format(val$chars_totales, big.mark = ".", decimal.mark = ",")
          )
        }
      )
    })
    
    # Panel de validación: alertas con estado detallado
    output$validacion_ui <- renderUI({
      if (is.null(input$pdf_file)) {
        return(
          bs4Callout(
            title  = "Esperando archivo",
            width  = NULL,
            status = "info",
            "Cargue un certificado de existencia y representación legal",
            "emitido por la Cámara de Comercio de Bogotá (formato PDF)."
          )
        )
      }
      
      val <- rv_validacion()
      
      if (!isTRUE(val$ok)) {
        return(
          bs4Callout(
            title  = tagList(icon("triangle-exclamation"), " Documento no válido"),
            width  = NULL,
            status = "danger",
            tags$p(val$mensaje),
            # isFALSE + isTRUE garantizan escalares; evita error si val retorna NULL o vector
            if (isFALSE(val$tiene_nombramientos) && isTRUE(val$chars_totales > 200L)) {
              tags$p(
                class = "mb-0 small",
                icon("circle-info"), " ",
                "Asegúrese de cargar el certificado completo que incluya",
                " la sección de NOMBRAMIENTOS."
              )
            }
          )
        )
      }
      
      ext <- rv_extraccion()
      
      if (!isTRUE(ext$ok)) {
        return(
          bs4Callout(
            title  = tagList(icon("triangle-exclamation"), " Error en extracción"),
            width  = NULL,
            color  = "warning",
            tags$p(val$mensaje),
            tags$p(ext$mensaje)
          )
        )
      }
      
      bs4Callout(
        title  = tagList(icon("circle-check"), " Documento procesado"),
        width  = NULL,
        status = "success",
        tags$p(val$mensaje),
        tags$p(
          tags$strong(ext$n_items),
          " administrador(es) identificado(s) y listos para descarga."),
        tags$p(
          class = "small text-muted mb-0",
          icon("microchip"), " Motor: ",
          tags$code(switch(ext$motor %||% "regex",
                           regex = "Regex",
                           llm   = "OpenAI (fallback)",
                           ext$motor
          ))
        )
      )
    })
    
    # Panel de resultados: TablaReactable2 + botón de descarga institucional
    output$resultados_ui <- renderUI({
      req(input$pdf_file)
      ext <- rv_extraccion()
      req(ext$ok)
      
      fluidRow(
        column(width = 12,
               bs4Card(title = tagList("Administradores extraídos", tags$span(class = "badge bg-white ms-2", ext$n_items)),
                       width = NULL, status = "white", solidHeader = TRUE, collapsible = FALSE,
                       footer = racafeShiny::BotonDescarga(button_id = "descargar_xlsx", ns = ns, title = "Descargar", 
                                                           size = "xs", color_fondo = "#dc3545"),
                       racafeModulos::TablaReactable2UI(ns("tabla_admins"), estilo = "minimal")
                       )
               )
        )
    })
    
    # Reactivo de datos de administradores con columnas renombradas para la tabla
    datos_admins_r <- reactive({
      ext <- rv_extraccion()
      req(ext$ok)
      ext$df %>%
        transmute(
          `Tipo ID`          = tipo_id_codigo,
          `Número ID`        = num_id,
          `Cód. Rol`         = tipo_admin_codigo,
          `Rol`              = tipo_admin_desc,
          `Primer Apellido`  = primer_apellido,
          `Segundo Apellido` = segundo_apellido,
          `Primer Nombre`    = primer_nombre,
          `Segundo Nombre`   = segundo_nombre
        )
    })
    
    # Módulo TablaReactable2: visualización sin selección ni modal
    racafeModulos::TablaReactable2(
      id             = "tabla_admins",
      data           = datos_admins_r,
      modo_seleccion = "ninguno",
      sortable       = TRUE,
      searchable     = FALSE,
      page_size      = 15,
      compact        = TRUE,
      mostrar_nota   = FALSE
    )
    
    # Handler de descarga: escribe sobre la plantilla institucional
    output$descargar_xlsx <- downloadHandler(
      filename = function() {
        nombre_base <- tools::file_path_sans_ext(input$pdf_file$name)
        paste0("Admins_", nombre_base, "_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
      },
      content = function(file) {
        ext <- rv_extraccion()
        req(ext$ok)
        tmp <- escribir_plantilla_ccb(ext$df, PLANTILLA_CCB_PATH)
        file.copy(tmp, file)
      },
      contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
    )
  })
}