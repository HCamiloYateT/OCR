CamarasUI <- function(id) {
  ns <- NS(id)
  tagList(
    
    # Fila 1: carga + metadatos + validación
    fluidRow(
      
      # Tarjeta de carga del PDF
      column(width = 4,
             bs4Card(
               title       = tagList(icon("file-pdf"), " Cargar certificado"),
               width       = NULL,
               status      = "white",
               solidHeader = TRUE,
               collapsible = FALSE,
               fileInput(
                 inputId     = ns("pdf_file"),
                 label       = "Certificado de existencia y representación legal (PDF)",
                 accept      = ".pdf",
                 multiple    = FALSE,
                 buttonLabel = tagList(icon("upload")),
                 placeholder = "Ningún archivo seleccionado"
               ),
               hr(class = "my-2"),
               uiOutput(ns("meta_box"))
             )
      ),
      
      # Tarjeta de estado + resumen de extracción
      column(width = 8,
             bs4Card(
               title       = "Estado del documento",
               width       = NULL,
               status      = "white",
               solidHeader = TRUE,
               collapsible = FALSE,
               # El preloader se muestra sobre este panel mientras procesa
               uiOutput(ns("validacion_ui"))
             )
      )
    ),
    
    # Fila 2: tabla de administradores
    uiOutput(ns("resultados_ui"))
  )
}
Camaras <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Preloader corporativo: se muestra sobre la tarjeta de estado
    w <- waiter::Waiter$new(
      id    = ns("validacion_ui"),
      html  = preloader_calculando$html,
      color = preloader_calculando$color
    )
    
    # Clasificación rápida del PDF (sin extracción, sin API)
    rv_clasificacion <- reactive({
      req(input$pdf_file)
      clasificar_pdf(input$pdf_file$datapath)
    })
    
    # Pipeline completo: validación + extracción + tiempo
    rv_procesado <- reactive({
      req(input$pdf_file)
      procesar_cerl(
        pdf_path       = input$pdf_file$datapath,
        nombre_archivo = input$pdf_file$name,
        verbose        = TRUE
      )
    })
    
    # Mostrar preloader al cargar nuevo archivo
    observeEvent(input$pdf_file, {
      req(input$pdf_file)
      w$show()
    })
    
    # Ocultar preloader cuando el pipeline termina
    observeEvent(rv_procesado(), {
      w$hide()
    }, ignoreNULL = TRUE)
    
    
    # ── Metadatos del archivo ─────────────────────────────────
    
    output$meta_box <- renderUI({
      req(input$pdf_file)
      f    <- input$pdf_file
      cls  <- rv_clasificacion()
      peso <- round(f$size / 1024, 1)
      
      tipo_cfg <- switch(cls$tipo_pdf,
                         texto     = list(color = "success", label = "Texto digital", icon = "file-lines"),
                         escaneado = list(color = "warning", label = "Escaneado",     icon = "file-image"),
                         mixto     = list(color = "info",    label = "Mixto",         icon = "file-circle-question"),
                         list(color = "danger", label = "Error", icon = "file-circle-xmark")
      )
      
      tags$div(class = "small text-muted mt-1",
               tags$b("Archivo: "),   f$name,             tags$br(),
               tags$b("Tamaño: "),    paste0(peso, " KB"), tags$br(),
               tags$b("Páginas: "),   cls$paginas,         tags$br(),
               tags$b("Tipo PDF: "),
               tags$span(
                 class = paste0("badge bg-", tipo_cfg$color),
                 icon(tipo_cfg$icon), " ", tipo_cfg$label
               ),
               tags$br(),
               if (cls$tipo_pdf == "escaneado") {
                 tags$small(class = "text-muted fst-italic",
                            icon("robot"), " Se procesará con LLM vision directamente.")
               }
      )
    })
    
    
    # ── Panel de validación ───────────────────────────────────
    
    output$validacion_ui <- renderUI({
      
      # Sin archivo cargado
      if (is.null(input$pdf_file)) {
        return(bs4Callout(
          title  = "Esperando archivo",
          width  = NULL,
          status = "info",
          tags$p("Cargue un certificado de existencia y representación legal."),
          tags$small(class = "text-muted",
                     icon("circle-info"),
                     " Compatible con cualquier cámara de comercio colombiana.",
                     tags$br(),
                     icon("robot"), " PDFs escaneados procesados con LLM vision.",
                     tags$br(),
                     icon("code"), " PDFs de texto procesados con regex + LLM.")
        ))
      }
      
      # Resultado del pipeline (incluye tiempo)
      proc <- rv_procesado()
      
      # Error en validación
      if (!isTRUE(proc$ok)) {
        return(bs4Callout(
          title  = tagList(icon("triangle-exclamation"), " Documento no válido"),
          width  = NULL,
          status = "danger",
          tags$p(proc$mensaje),
          if (length(proc$validacion$advertencias) > 0L)
            tags$ul(class = "mb-0 small",
                    lapply(proc$validacion$advertencias, tags$li)),
          .badge_tiempo(proc$tiempo_seg)
        ))
      }
      
      ext <- proc$extraccion
      
      # Error en extracción (documento válido pero sin resultados)
      if (!isTRUE(ext$ok)) {
        return(bs4Callout(
          title  = tagList(icon("triangle-exclamation"), " Sin resultados"),
          width  = NULL,
          status = "warning",
          tags$p(proc$mensaje),
          tags$p(class = "small mb-1",
                 icon("code"),  " Regex: ", ext$mensaje_regex),
          tags$p(class = "small mb-0",
                 icon("robot"), " LLM: ",   ext$mensaje_llm),
          .badge_tiempo(proc$tiempo_seg)
        ))
      }
      
      val <- proc$validacion
      
      # Éxito — resumen con advertencias + estadísticas + tiempo
      bs4Callout(
        title  = tagList(icon("circle-check"), " Documento procesado"),
        width  = NULL,
        status = "success",
        tags$p(proc$mensaje),
        
        # Advertencias no bloqueantes (cámara desconocida, OCR bajo, etc.)
        if (length(val$advertencias) > 0L)
          tags$div(class = "mb-2",
                   lapply(val$advertencias, function(adv)
                     tags$p(class = "mb-1 small text-warning",
                            icon("triangle-exclamation"), " ", adv))),
        
        # Resumen de extracción según método
        .resumen_extraccion(ext),
        
        tags$hr(class = "my-2"),
        
        # Detalle de mensajes de cada motor
        tags$div(class = "small text-muted",
                 if (ext$metodo == "dual") {
                   tagList(
                     tags$span(icon("code"),  " Regex: ", ext$mensaje_regex), tags$br(),
                     tags$span(icon("robot"), " LLM: ",   ext$mensaje_llm)
                   )
                 } else {
                   tags$span(icon("robot"), " Vision LLM: ", ext$mensaje_llm)
                 }
        ),
        
        # Tiempo total
        tags$hr(class = "my-2"),
        .badge_tiempo(proc$tiempo_seg)
      )
    })
    
    
    # ── Panel de resultados ───────────────────────────────────
    
    output$resultados_ui <- renderUI({
      proc <- rv_procesado()
      req(isTRUE(proc$ok), isTRUE(proc$extraccion$ok))
      ext <- proc$extraccion
      
      fluidRow(
        column(width = 12,
               bs4Card(
                 title       = tagList(
                   icon("users"), " Administradores extraídos",
                   tags$span(class = "badge bg-secondary ms-2", ext$n_items)
                 ),
                 width       = NULL,
                 status      = "white",
                 solidHeader = TRUE,
                 collapsible = FALSE,
                 footer      = racafeShiny::BotonDescarga(
                   button_id   = "descargar_xlsx",
                   ns          = ns,
                   title       = "Descargar plantilla",
                   size        = "xs",
                   color_fondo = "#dc3545"
                 ),
                 racafeModulos::TablaReactable2UI(ns("tabla_admins"), estilo = "minimal")
               )
        )
      )
    })
    
    # Datos para la tabla — sin columna fuente
    datos_tabla_r <- reactive({
      proc <- rv_procesado()
      req(isTRUE(proc$ok), isTRUE(proc$extraccion$ok))
      proc$extraccion$df %>%
        transmute(
          `Tipo ID`          = tipo_id_codigo,
          `Número ID`        = num_id,
          `Rol`              = tipo_admin_desc,
          `Primer Nombre`    = primer_nombre,
          `Segundo Nombre`   = segundo_nombre,
          `Primer Apellido`  = primer_apellido, 
          `Segundo Apellido` = segundo_apellido
        )
    })
    
    racafeModulos::TablaReactable2(
      id             = "tabla_admins",
      data           = datos_tabla_r,
      modo_seleccion = "ninguno",
      sortable       = TRUE,
      searchable     = FALSE,
      page_size      = 20L,
      compact        = TRUE,
      mostrar_nota   = FALSE
    )
    
    # Descarga — plantilla sin columna fuente
    output$descargar_xlsx <- downloadHandler(
      filename = function() {
        nombre_base <- tools::file_path_sans_ext(input$pdf_file$name)
        paste0("Admins_", nombre_base, "_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
      },
      content = function(file) {
        proc <- rv_procesado()
        req(isTRUE(proc$ok), isTRUE(proc$extraccion$ok))
        tmp <- escribir_plantilla_ccb(proc$extraccion$df, PLANTILLA_CCB_PATH)
        file.copy(tmp, file)
      },
      contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
    )
    
  })
}


# ── Helpers visuales internos ────────────────────────────────

# Badge de tiempo total de procesamiento
.badge_tiempo <- function(tiempo_seg) {
  if (is.null(tiempo_seg) || is.na(tiempo_seg)) return(NULL)
  color  <- if (tiempo_seg < 5) "#059669" else if (tiempo_seg < 20) "#D97706" else "#DC2626"
  tags$div(
    class = "d-flex align-items-center gap-2",
    tags$span(icon("clock"), class = "text-muted small", " Tiempo total:"),
    tags$span(
      style = sprintf(
        "font-weight:700; color:%s; font-size:13px;", color
      ),
      sprintf("%.1f segundos", tiempo_seg)
    )
  )
}

# Resumen visual de la extracción según el método usado
.resumen_extraccion <- function(ext) {
  if (ext$metodo == "vision") {
    # PDF escaneado: solo muestra total via LLM vision
    fluidRow(
      column(6,
             tags$div(class = "text-center",
                      tags$div(
                        style = "font-size:22px; font-weight:700; color:#7C3AED;",
                        icon("robot", style = "font-size:14px; margin-right:4px;"), ext$n_items
                      ),
                      tags$div(class = "small text-muted", "LLM Vision")
             )
      ),
      column(6,
             tags$div(class = "text-center",
                      tags$div(
                        style = "font-size:22px; font-weight:700; color:#374151;",
                        icon("users", style = "font-size:14px; margin-right:4px;"), ext$n_items
                      ),
                      tags$div(class = "small text-muted", "Total admin(s)")
             )
      )
    )
  } else {
    # PDF texto: muestra regex + LLM + total
    fluidRow(
      column(4,
             tags$div(class = "text-center",
                      tags$div(
                        style = "font-size:22px; font-weight:700; color:#1D4ED8;",
                        icon("code", style = "font-size:14px; margin-right:4px;"), ext$n_regex
                      ),
                      tags$div(class = "small text-muted", "Regex")
             )
      ),
      column(4,
             tags$div(class = "text-center",
                      tags$div(
                        style = "font-size:22px; font-weight:700; color:#7C3AED;",
                        icon("robot", style = "font-size:14px; margin-right:4px;"), ext$n_llm
                      ),
                      tags$div(class = "small text-muted", "LLM")
             )
      ),
      column(4,
             tags$div(class = "text-center",
                      tags$div(
                        style = "font-size:22px; font-weight:700; color:#374151;",
                        icon("users", style = "font-size:14px; margin-right:4px;"), ext$n_items
                      ),
                      tags$div(class = "small text-muted", "Total")
             )
      )
    )
  }
}