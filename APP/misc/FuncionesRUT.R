# FuncionesRUT.R
# Extracción de representantes legales desde RUT DIAN (formulario 001).
#
# Estrategia: pdftotext -layout preserva la alineación horizontal.
# Patrón del formulario (campos 98-110, se repite hasta 5 veces por hoja):
#
#   Línea etiqueta:  "    98. Representación           99. Fecha..."
#   Línea valor:     "     REPRS LEGAL PRIN    18      2 0 2 2 0 8 0 5"
#   ...
#   Línea etiqueta:  "    100. Tipo de documento   101. Número de identificación..."
#   Línea valor:     "     Cédula de Ciudadaní 1 3    7  9  2  4  9  6  6  9"
#   ...
#   Línea etiqueta:  "    104. Primer apellido    105. Segundo apellido    106. ..."
#   Línea valor:     "    SANCHEZ                 ANZOLA                   ALVARO   ANDRES"
#
# Las columnas de nombres se separan con 2+ espacios consecutivos → tokenizar.
# Los dígitos del número de ID están separados por espacios → reconstruir.


# Constantes ----

RUT_REPRS_TO_ADMIN_CODE <- c(
  "REPRS LEGAL PRIN"              = 1L,
  "REPRS LEGAL SUPL"              = 2L,
  "REP LEGAL PRINC"               = 1L,
  "REP LEGAL SUPLEN"              = 2L,
  "REPRESENTANTE LEGAL PRINCIPAL" = 1L,
  "REPRESENTANTE LEGAL SUPLENTE"  = 2L
)

# Mapeo sin tildes (se compara contra texto normalizado)
RUT_DOC_TO_CODE <- c(
  "CEDULA DE CIUDADANI" = "13",
  "CEDULA DE CIUDADAN"  = "13",
  "CEDULA DE CIUDADA"   = "13",
  "PASAPORTE"           = "21",
  "NIT"                 = "31",
  "CEDULA DE EXTRANJERI" = "22"
)

RUT_ADMIN_CODE_TO_DESC <- c(
  "1" = "Representante Legal Principal",
  "2" = "Representante Legal Suplente"
)


# Helpers internos ----

# Elimina tildes y convierte a mayúsculas para comparaciones robustas
.rut_norm <- function(x) {
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  toupper(x)
}

# Detecta el tipo de representación en la línea de valor del campo 98
.rut_tipo_reprs <- function(linea) {
  ln <- .rut_norm(linea)
  for (tr in names(RUT_REPRS_TO_ADMIN_CODE)) {
    if (stringr::str_detect(ln, stringr::fixed(tr))) return(tr)
  }
  NA_character_
}

# Detecta tipo de documento (campo 100) y su código
.rut_tipo_doc <- function(linea) {
  ln <- .rut_norm(linea)
  for (td in names(RUT_DOC_TO_CODE)) {
    if (stringr::str_detect(ln, stringr::fixed(td))) return(td)
  }
  NA_character_
}

# Reconstruye el número de identificación desde dígitos separados por espacios.
# Descarta el código de tipo de documento (p.e. "13") si queda pegado al inicio.
.rut_reconstruir_num <- function(linea, codigo_doc = NA_character_) {
  # Buscar todos los grupos de dígitos separados por espacios (>= 4 dígitos totales)
  matches <- stringr::str_extract_all(linea, "(?:\\d\\s+){3,}\\d|\\d{6,}")[[1]]
  if (length(matches) == 0L) return(NA_character_)
  # Reconstruir eliminando espacios internos
  nums <- stringr::str_replace_all(matches, "\\s+", "")
  # El número de ID es el más largo
  num <- nums[which.max(nchar(nums))]
  # Si el código de tipo quedó pegado al inicio, removerlo
  if (!is.na(codigo_doc) && nzchar(codigo_doc) && stringr::str_starts(num, codigo_doc)) {
    num <- substr(num, nchar(codigo_doc) + 1L, nchar(num))
  }
  if (nchar(num) < 4L) NA_character_ else num
}

# Tokeniza la línea de valores de los campos 104-107.
# Los cuatro campos están separados por 2+ espacios consecutivos.
# Retorna lista con pa, sa, pn, sn (NA si el token no existe o está vacío).
.rut_tokenizar_nombres <- function(linea_valores) {
  lv <- stringr::str_trim(toupper(linea_valores))
  if (!nzchar(lv)) return(list(pa = NA, sa = NA, pn = NA, sn = NA))
  
  # Separar por 2 o más espacios consecutivos
  tokens <- stringr::str_split(lv, "\\s{2,}")[[1]]
  tokens <- tokens[nzchar(stringr::str_trim(tokens))]
  
  limpiar <- function(i) {
    if (i > length(tokens)) return(NA_character_)
    v <- stringr::str_trim(tokens[i])
    if (nzchar(v)) v else NA_character_
  }
  
  list(pa = limpiar(1L), sa = limpiar(2L), pn = limpiar(3L), sn = limpiar(4L))
}


# Parser principal ----

.rut_parsear_layout <- function(texto_layout) {
  lineas <- stringr::str_split(texto_layout, "\n")[[1]]
  n      <- length(lineas)
  result <- list()
  
  i <- 1L
  while (i <= n) {
    lu <- .rut_norm(lineas[i])
    
    # Detectar etiqueta del campo 98
    if (!stringr::str_detect(lu, "98\\.\\s*REPRESENTACI")) { i <- i + 1L; next }
    
    # Valor del campo 98: línea siguiente
    val_98     <- if (i + 1L <= n) lineas[i + 1L] else ""
    tipo_reprs <- .rut_tipo_reprs(val_98)
    
    # Saltar bloques sin tipo mapeado (slots vacíos del formulario)
    if (is.na(tipo_reprs)) { i <- i + 2L; next }
    
    tipo_doc        <- NA_character_
    num_id          <- NA_character_
    primer_apellido <- NA_character_
    seg_apellido    <- NA_character_
    primer_nombre   <- NA_character_
    seg_nombre      <- NA_character_
    
    j     <- i + 2L
    j_fin <- min(i + 16L, n)
    
    while (j <= j_fin) {
      lj   <- lineas[j]
      lj_u <- .rut_norm(lj)
      
      # Etiqueta campo 100 (tipo de documento) en línea separada
      if (stringr::str_detect(lj_u, "100\\.\\s*TIPO\\s*DE\\s*DOCUMENTO")) {
        val_100  <- if (j + 1L <= n) lineas[j + 1L] else ""
        tipo_doc <- .rut_tipo_doc(val_100)
        num_id   <- .rut_reconstruir_num(val_100, RUT_DOC_TO_CODE[tipo_doc])
        j <- j + 2L; next
      }
      
      # Alternativa: tipo doc y número en la misma línea (sin etiqueta 100 separada)
      if (!stringr::str_detect(lj_u, "\\d{1,2}\\.") &&
          stringr::str_detect(lj_u, "CEDULA|PASAPORTE") &&
          stringr::str_detect(lj_u, "\\d")) {
        tipo_doc <- .rut_tipo_doc(lj)
        num_id   <- .rut_reconstruir_num(lj, RUT_DOC_TO_CODE[tipo_doc])
        j <- j + 1L; next
      }
      
      # Etiqueta campo 104 (nombres)
      if (stringr::str_detect(lj_u, "104\\.\\s*PRIMER\\s*APELLIDO")) {
        val_104 <- if (j + 1L <= n) lineas[j + 1L] else ""
        nms     <- .rut_tokenizar_nombres(val_104)
        primer_apellido <- nms$pa
        seg_apellido    <- nms$sa
        primer_nombre   <- nms$pn
        seg_nombre      <- nms$sn
        j <- j + 2L; next
      }
      
      # Fin del bloque actual: nueva etiqueta 98 o campo 108
      if (stringr::str_detect(lj_u, "98\\.\\s*REPRESENTACI|108\\.\\s*N")) break
      
      j <- j + 1L
    }
    
    # Registrar solo si hay al menos un nombre o apellido extraído
    if (!all(is.na(c(primer_apellido, primer_nombre)))) {
      result <- c(result, list(list(
        tipo_reprs       = tipo_reprs,
        tipo_doc         = tipo_doc,
        num_id           = num_id,
        primer_apellido  = primer_apellido,
        segundo_apellido = seg_apellido,
        primer_nombre    = primer_nombre,
        segundo_nombre   = seg_nombre
      )))
    }
    
    i <- j
  }
  
  result
}


# Construcción del data.frame de salida ----

build_admin_df_rut <- function(representantes) {
  if (length(representantes) == 0L) {
    warning("[RUT] No se encontraron representantes legales en el documento.")
    return(data.frame())
  }
  
  rows <- purrr::map(representantes, function(rep) {
    admin_code <- RUT_REPRS_TO_ADMIN_CODE[rep$tipo_reprs]
    if (is.na(admin_code)) {
      message(sprintf("[RUT][SKIP] Tipo '%s' sin mapeo — descartado.", rep$tipo_reprs))
      return(NULL)
    }
    doc_code <- if (!is.na(rep$tipo_doc)) RUT_DOC_TO_CODE[rep$tipo_doc] else NA_character_
    
    data.frame(
      tipo_id_codigo    = as.character(doc_code),
      num_id            = as.character(rep$num_id %||% NA_character_),
      tipo_admin_codigo = as.integer(admin_code),
      tipo_admin_desc   = unname(RUT_ADMIN_CODE_TO_DESC[as.character(admin_code)]),
      primer_apellido   = rep$primer_apellido  %||% NA_character_,
      segundo_apellido  = rep$segundo_apellido %||% NA_character_,
      primer_nombre     = rep$primer_nombre    %||% NA_character_,
      segundo_nombre    = rep$segundo_nombre   %||% NA_character_,
      razon_social      = NA_character_,
      porcentaje        = NA_real_,
      id_administrador  = NA_character_,
      stringsAsFactors  = FALSE,
      row.names         = NULL
    )
  })
  
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0L) { warning("[RUT] Ninguna fila válida."); return(data.frame()) }
  dplyr::bind_rows(rows) %>% dplyr::arrange(tipo_admin_codigo)
}


# Metadatos básicos del RUT ----

extraer_metadatos_rut <- function(texto_pag1) {
  lu        <- toupper(stringr::str_replace_all(texto_pag1, "\\s+", " "))
  nit_match <- stringr::str_match(lu, "NIT\\)[^\\d]*((?:\\d\\s*){9,11})")
  nit_raw   <- nit_match[1, 2]
  nit       <- if (!is.na(nit_raw)) stringr::str_replace_all(nit_raw, "\\s", "") else NA_character_
  
  rs_match     <- stringr::str_match(texto_pag1, "(?i)35\\.\\s*Raz[oó]n\\s+social\\s*\n\\s*([^\n]+)")
  razon_social <- stringr::str_trim(rs_match[1, 2] %||% NA_character_)
  
  if (is.na(razon_social) || !nzchar(razon_social)) {
    rs2          <- stringr::str_match(texto_pag1, "(?i)Raz[oó]n\\s+social\\s*\n\\s*([^\n]+)")
    razon_social <- stringr::str_trim(rs2[1, 2] %||% NA_character_)
  }
  
  list(nit = nit, razon_social = razon_social)
}


# Función principal de procesamiento ----

procesar_rut <- function(pdf_path, nombre_archivo = NULL, verbose = FALSE) {
  t_inicio   <- Sys.time()
  nombre_log <- nombre_archivo %||% basename(pdf_path)
  if (verbose) message("[RUT] Iniciando: ", nombre_log)
  
  if (!file.exists(pdf_path)) {
    return(list(ok = FALSE, df = data.frame(), n_items = 0L, metadatos = list(),
                mensaje = "Archivo no encontrado.", tiempo_seg = 0))
  }
  
  paginas_texto <- tryCatch(pdftools::pdf_text(pdf_path), error = function(e) NULL)
  if (is.null(paginas_texto) || all(nchar(trimws(paginas_texto)) == 0L)) {
    return(list(ok = FALSE, df = data.frame(), n_items = 0L, metadatos = list(),
                mensaje = "PDF sin texto extraíble (posiblemente escaneado — requiere OCR).",
                tiempo_seg = round(as.numeric(difftime(Sys.time(), t_inicio, units = "secs")), 1)))
  }
  
  metadatos <- extraer_metadatos_rut(paginas_texto[1])
  if (verbose) message(sprintf("[RUT] NIT: %s | RS: %s", metadatos$nit, metadatos$razon_social))
  
  # Releer con layout para máxima fidelidad posicional
  texto_layout <- tryCatch({
    tmp <- tempfile(fileext = ".txt")
    system2("pdftotext",
            args   = c("-layout", "-enc", "UTF-8", shQuote(pdf_path), shQuote(tmp)),
            stdout = FALSE, stderr = FALSE)
    if (file.exists(tmp)) {
      txt <- paste(readLines(tmp, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
      unlink(tmp)
      txt
    } else {
      paste(paginas_texto, collapse = "\n")
    }
  }, error = function(e) paste(paginas_texto, collapse = "\n"))
  
  representantes <- tryCatch(
    .rut_parsear_layout(texto_layout),
    error = function(e) { message("[RUT] Parser falló: ", e$message); list() }
  )
  
  df         <- build_admin_df_rut(representantes)
  tiempo_seg <- round(as.numeric(difftime(Sys.time(), t_inicio, units = "secs")), 1)
  n_ok       <- nrow(df) > 0L
  
  if (verbose) message(sprintf("[RUT] %d representantes en %.1fs", nrow(df), tiempo_seg))
  
  list(
    ok         = n_ok,
    df         = df,
    n_items    = nrow(df),
    metadatos  = metadatos,
    mensaje    = if (n_ok)
      sprintf("RUT | %s | %d representante(s)", metadatos$razon_social %||% "—", nrow(df))
    else
      "No se encontraron representantes legales (verifique que el PDF tenga texto extraíble).",
    tiempo_seg = tiempo_seg
  )
}


# Escritura Excel (misma plantilla que CERL) ----

escribir_plantilla_rut <- function(df, plantilla_path) {
  escribir_plantilla_ccb(df, plantilla_path)
}