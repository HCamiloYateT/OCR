# APP/misc/modulos/RUT.R
# Módulo Shiny para extracción de representantes legales desde RUT DIAN.
# Estructura y convenciones idénticas al módulo Camaras.R.
# El procesamiento se dispara automáticamente al completar la carga del archivo.


# UI ----

RutUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    
    # Fila principal: carga y estado ----
    fluidRow(
      
      # Panel izquierdo: selector de archivo
      column(width = 4,
             bs4Card(
               title       = tagList(icon("file-invoice"), " Cargar RUT"),
               width       = NULL,
               status      = "white",
               solidHeader = TRUE,
               collapsible = FALSE,
               fileInput(
                 inputId     = ns("pdf_file"),
                 label       = "Seleccionar RUT (PDF)",
                 multiple    = FALSE,
                 accept      = ".pdf",
                 buttonLabel = tagList(icon("upload"), " Examinar"),
                 placeholder = "Ningún archivo seleccionado"
               )
             )
      ),
      
      # Panel derecho: estado del procesamiento
      column(width = 8,
             bs4Card(
               title       = tagList(icon("circle-info"), " Estado"),
               width       = NULL,
               status      = "white",
               solidHeader = TRUE,
               collapsible = FALSE,
               uiOutput(ns("estado_ui"))
             )
      )
    ),
    
    # Fila de resultados: tabla y descarga ----
    fluidRow(
      column(width = 12,
             uiOutput(ns("resultados_ui"))
      )
    )
  )
}


# Server ----

RutServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Estado reactivo del procesamiento
    rv_procesado <- reactiveVal(NULL)
    
    # Disparar procesamiento al completar la carga del archivo ----
    observeEvent(input$pdf_file, {
      req(input$pdf_file)
      rv_procesado(NULL)
      
      withProgress(message = "Procesando RUT…", value = 0, {
        incProgress(0.3, detail = "Leyendo PDF")
        proc <- tryCatch(
          procesar_rut(
            pdf_path       = input$pdf_file$datapath,
            nombre_archivo = input$pdf_file$name,
            verbose        = TRUE
          ),
          error = function(e) {
            list(
              ok         = FALSE,
              df         = data.frame(),
              n_items    = 0L,
              metadatos  = list(),
              mensaje    = paste("Error inesperado:", e$message),
              tiempo_seg = 0
            )
          }
        )
        incProgress(0.7, detail = "Construyendo tabla")
        rv_procesado(proc)
      })
    })
    
    
    # Panel de estado ----
    
    output$estado_ui <- renderUI({
      proc <- rv_procesado()
      
      if (is.null(proc)) {
        return(tags$p(class = "text-muted small mt-1",
                      icon("hand-point-up"), " Cargue un RUT para iniciar el procesamiento."
        ))
      }
      
      status_color <- if (isTRUE(proc$ok)) "success" else "danger"
      status_icon  <- if (isTRUE(proc$ok)) "circle-check" else "circle-xmark"
      
      tagList(
        tags$div(
          class = paste0("callout callout-", status_color),
          tags$h5(icon(status_icon), " Documento procesado"),
          tags$p(class = "mb-1", proc$mensaje),
          .badge_tiempo_rut(proc$tiempo_seg)
        )
      )
    })
    
    
    # Panel de resultados ----
    
    output$resultados_ui <- renderUI({
      proc <- rv_procesado()
      req(isTRUE(proc$ok), nrow(proc$df) > 0L)
      
      fluidRow(
        column(width = 12,
               bs4Card(
                 title       = tagList(
                   icon("users"), " Representantes extraídos",
                   tags$span(class = "badge bg-secondary ms-2", proc$n_items)
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
                 racafeModulos::TablaReactable2UI(ns("tabla_rut"), estilo = "minimal")
               )
        )
      )
    })
    
    
    # Datos para la tabla ----
    
    datos_tabla_r <- reactive({
      proc <- rv_procesado()
      req(isTRUE(proc$ok), nrow(proc$df) > 0L)
      proc$df %>%
        transmute(
          `Tipo ID`          = tipo_id_codigo,
          `Número ID`        = num_id,
          `Rol`              = tipo_admin_desc,
          `Primer Apellido`  = primer_apellido,
          `Segundo Apellido` = segundo_apellido,
          `Primer Nombre`    = primer_nombre,
          `Segundo Nombre`   = segundo_nombre,
          `Razón Social`     = razon_social
        )
    })
    
    # Registro tabla (page_size fijo — reactable no acepta reactive aquí)
    racafeModulos::TablaReactable2(
      id             = "tabla_rut",
      data           = datos_tabla_r,
      modo_seleccion = "ninguno",
      sortable       = TRUE,
      searchable     = FALSE,
      page_size      = 500L,
      compact        = TRUE,
      mostrar_nota   = FALSE
    )
    
    
    # Descarga Excel (misma plantilla que CERL) ----
    
    output$descargar_xlsx <- downloadHandler(
      filename = function() {
        nombre_base <- tools::file_path_sans_ext(input$pdf_file$name)
        paste0("Admins_RUT_", nombre_base, "_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
      },
      content = function(file) {
        proc <- rv_procesado()
        req(isTRUE(proc$ok), nrow(proc$df) > 0L)
        tmp <- escribir_plantilla_rut(proc$df, PLANTILLA_CCB_PATH)
        file.copy(tmp, file)
      },
      contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
    )
    
  })
}


# Helper visual local ----

.badge_tiempo_rut <- function(tiempo_seg) {
  if (is.null(tiempo_seg) || is.na(tiempo_seg)) return(NULL)
  color <- if (tiempo_seg < 5) "#059669" else if (tiempo_seg < 20) "#D97706" else "#DC2626"
  tags$div(
    class = "d-flex align-items-center gap-2",
    tags$span(icon("clock"), class = "text-muted small", " Tiempo total:"),
    tags$span(
      style = sprintf("font-weight:700; color:%s; font-size:13px;", color),
      sprintf("%.1f segundos", tiempo_seg)
    )
  )
}