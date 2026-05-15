# Configuraciones Iniciales ----
## Variables de control
# Flags de entorno para comportamiento en produccion vs desarrollo
verbose <- FALSE
debug   <- FALSE

## Configuracion regional y del sistema
# Zona horaria Colombia (UTC-5, sin DST): afecta POSIXct, lubridate y logs
Sys.setenv(TZ = "America/Bogota")

# LANG: mensajes de error de librerias C en Linux; ignorado en Windows
Sys.setenv(LANG = "es_CO.UTF-8")

# LC_TIME: nombres de meses/dias en espanol para strftime y lubridate
# LC_MONETARY: simbolo COP disponible en format(style = "currency")
# LC_MESSAGES: warnings del SO en espanol
locales_es <- c("es_CO.UTF-8", "es_ES.UTF-8", "Spanish_Spain.1252", "es_ES", "Spanish")
for (.cat in c("LC_TIME", "LC_MONETARY", "LC_MESSAGES")) {
  .actual <- tryCatch(Sys.getlocale(.cat), error = function(e) "C")
  if (identical(.actual, "C") || !nzchar(.actual)) {
    for (.loc in locales_es) {
      .res <- suppressWarnings(
        tryCatch(Sys.setlocale(.cat, .loc), error = function(e) "C")
      )
      if (!identical(.res, "C") && nzchar(.res)) break
    }
  }
}
rm(.cat, .actual)

# Verificacion de locales activos al arranque
if (verbose) {
  message("[INFO] Estado de locales al arranque:")
  for (.cat in c("LC_TIME", "LC_MONETARY", "LC_NUMERIC", "LC_MESSAGES")) {
    message(sprintf("  %s = %s", .cat, Sys.getlocale(.cat)))
  }
  rm(.cat)
}

## Opciones globales
options(
  # Repositorio estable CRAN
  repos                  = c(CRAN = "https://cloud.r-project.org"),
  # dplyr: silenciar mensajes de summarise y progreso en produccion
  dplyr.summarise.inform = FALSE,
  dplyr.show_progress    = FALSE,
  # Numeros: punto decimal anglosajón — formato visual solo en capa de presentacion
  OutDec                 = ".",
  scipen                 = 999,
  # lubridate: semana ISO lunes=1, sin mensajes
  lubridate.week.start   = 1,
  lubridate.verbose      = FALSE,
  lubridate.quiet        = TRUE,
  # pillar: control de prints de tibbles en consola y logs
  pillar.sigfig          = 4,
  pillar.print_max       = 30,
  pillar.print_min       = 10,
  # Consola
  encoding               = "UTF-8",
  width                  = 120,
  max.print              = 1000,
  # Shiny: full stack trace y error handler segun modo activo
  shiny.autoreload       = FALSE,
  shiny.fullstacktrace   = debug,
  shiny.error            = if (debug) NULL else function() invisible(NULL)
)

# Librerias ----
library("racafeCore")
library("racafeBD")
library("racafeDrive")
library("racafeGraph")
library("racafeShiny")
library("racafeModulos")
racafeCore::Loadpkg(c("shiny", "bs4Dash", "shinyBS", "shinyjs", "DT", "shinycssloaders",
                      "shinyWidgets", "tidyverse", "gt",  "scales", "plotly",  "rlang",
                      "waiter", "glue", "lubridate", "stringr", "purrr", "pdftools", 
                      "httr2", "jsonlite"))

# Impresiones ----
tit_app <- "Lector OCR"
# valores: prototipo, pruebas, staging, produccion, demo, mantenimiento, ninguno
badge_estado <- "pruebas"

# Datos ----
## Plantillas
PLANTILLA_CCB_PATH <- "data/Templates/PlantillaCCB.xlsx"

# Carga datos precargados desde RData
if (file.exists("data/data.RData")) {
  tryCatch(
    {
      load("data/data.RData", envir = globalenv())
      if (verbose) message("[OK] Datos cargados desde data/data.RData")
    },
    error = function(e) {
      # Error real de lectura: siempre visible independiente de verbose
      message(sprintf("[ERROR] Fallo leyendo data/data.RData: %s", e$message))
    }
  )
} else {
  if (verbose) message("[INFO] data/data.RData no encontrado. App inicia sin datos precargados.")
}

# Sources ----
# Carga modulos ordenados por profundidad de ruta ascendente
# Reintenta fallidos en multi-pasada para resolver dependencias cruzadas
# Sin progreso en una pasada completa: aborta y reporta irresolubles
load_modules <- function(path = "misc", verbose = FALSE, progress = TRUE) {

  # Verificar dependencias internas antes de ejecutar
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("[load_modules] stringr no disponible. Debe cargarse antes de invocar load_modules.")
  }
  if (!requireNamespace("purrr", quietly = TRUE)) {
    stop("[load_modules] purrr no disponible. Debe cargarse antes de invocar load_modules.")
  }

  # Recolectar y normalizar rutas, excluir global.R
  files <- list.files(path, pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
  files <- unique(normalizePath(files, winslash = "/", mustWork = TRUE))
  files <- files[!grepl("/global\\.R$", files, ignore.case = TRUE)]

  # Salida temprana si no hay archivos en el path indicado
  if (length(files) == 0L) {
    message(sprintf("[WARN] No se encontraron archivos .R en '%s'", path))
    return(invisible(list(ok = 0L, fallidos = character(0), errores = list())))
  }

  # Ordenar por profundidad ascendente: menos separadores = mas cerca a raiz
  depth   <- stringr::str_count(files, "/")
  files   <- files[order(depth, files)]
  n_total <- length(files)

  if (verbose) {
    message(sprintf("[INFO] %d archivos encontrados en '%s':", n_total, path))
    purrr::walk(files, ~ message(sprintf("  [depth=%d] %s", stringr::str_count(.x, "/"), .x)))
  }

  # Inicializar barra de progreso si se solicita y no esta en modo verbose
  use_pb <- progress && !verbose && n_total > 0L
  pb <- if (use_pb) {
    txtProgressBar(min = 0L, max = n_total, style = 3, width = 60, char = "=")
  } else {
    NULL
  }

  # Estado de carga acumulado entre pasadas
  pendientes  <- files
  errores     <- list()
  cargados    <- character(0)
  pasada      <- 1L
  n_procesado <- 0L

  while (length(pendientes) > 0L) {
    fallidos_pasada <- character(0)

    for (f in pendientes) {
      resultado <- tryCatch(
        {
          sys.source(f, envir = globalenv())
          "ok"
        },
        error = function(e) e$message
      )

      if (identical(resultado, "ok")) {
        cargados    <- c(cargados, f)
        n_procesado <- n_procesado + 1L
        # Actualizar barra o imprimir OK segun modo activo
        if (use_pb) {
          setTxtProgressBar(pb, n_procesado)
        } else {
          message(sprintf("  [OK] %s", f))
        }
      } else {
        fallidos_pasada <- c(fallidos_pasada, f)
        errores[[f]]    <- resultado
      }
    }

    # Sin progreso: ninguno de los pendientes pudo cargarse en esta pasada
    if (length(fallidos_pasada) == length(pendientes)) {
      # Cerrar barra antes de imprimir errores para no mezclar output
      if (use_pb) { close(pb); use_pb <- FALSE; cat("\n") }
      message(sprintf("\n[ERROR] Pasada %d sin progreso. Archivos irresolubles:", pasada))
      purrr::walk(fallidos_pasada, function(f) {
        message(sprintf("  [FAIL] %s\n         -> %s", f, errores[[f]]))
      })
      break
    }

    # Hay progreso pero quedan fallidos: reintentar en siguiente pasada
    if (verbose && length(fallidos_pasada) > 0L) {
      message(sprintf("[RETRY] Pasada %d: reintentando %d archivo(s) fallido(s)",
                      pasada, length(fallidos_pasada)))
    }

    pendientes <- fallidos_pasada
    pasada     <- pasada + 1L
  }

  # Cerrar barra si todos los archivos cargaron correctamente
  if (use_pb) { close(pb); cat("\n") }

  # Resumen final siempre visible independiente de verbose
  message(sprintf("[DONE] Modulos: %d cargados | %d fallidos de %d totales",
                  length(cargados), length(errores), n_total))

  invisible(list(ok = length(cargados), fallidos = names(errores), errores = errores))
}

# Ejecutar carga heredando flag verbose del entorno global
load_modules(verbose = verbose)
