# Limpieza y normalización de texto ----

# Convierte a ASCII eliminando tildes y caracteres especiales
remove_accents <- function(x) iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")

# Elimina puntos y ceros iniciales en el número de identificación
clean_id <- function(x) {
  x %>%
    str_replace_all("\\D", "") %>%
    sub(pattern = "^0+", replacement = "", x = .)
}

# Normaliza a ASCII mayúsculas con espacios simples
normalize_line <- function(x) {
  x %>%
    remove_accents() %>%
    toupper() %>%
    str_replace_all("\\s+", " ") %>%
    str_trim()
}

# Separa nombre completo formato CCB (apellidos primero) en sus cuatro campos
split_ccb_name <- function(full_name) {
  parts <- str_split(str_trim(full_name), "\\s+")[[1]]
  n     <- length(parts)
  if (n == 0L) return(list(pa = "",        sa = "", pn = "",        sn = ""))
  if (n == 1L) return(list(pa = parts[1],  sa = "", pn = "",        sn = ""))
  if (n == 2L) return(list(pa = parts[1],  sa = "", pn = parts[2],  sn = ""))
  if (n == 3L) return(list(pa = parts[1],  sa = parts[2], pn = parts[3], sn = ""))
  list(pa = parts[1], sa = parts[2], pn = parts[3],
       sn = paste(parts[4:n], collapse = " "))
}


# Validación del documento ----

# Verifica que el PDF sea legible, tenga texto suficiente y contenga NOMBRAMIENTOS
validar_pdf_ccb <- function(pdf_path) {
  resultado <- list(ok = FALSE, paginas = 0L, chars_totales = 0L,
                    tiene_nombramientos = FALSE, mensaje = "")
  
  info <- tryCatch(pdf_info(pdf_path), error = function(e) NULL)
  if (is.null(info)) {
    resultado$mensaje <- "El archivo no es un PDF válido o está protegido."
    return(resultado)
  }
  resultado$paginas <- as.integer(info$pages)
  
  texto <- tryCatch(
    pdf_text(pdf_path) %>% paste(collapse = "\n"),
    error = function(e) ""
  )
  resultado$chars_totales <- nchar(texto)
  
  if (resultado$chars_totales < 200L) {
    resultado$mensaje <- paste0(
      "El PDF tiene muy poco texto extraíble (",
      resultado$chars_totales, " caracteres). Puede ser un documento escaneado."
    )
    return(resultado)
  }
  
  texto_up <- remove_accents(toupper(texto))
  resultado$tiene_nombramientos <- str_detect(texto_up, "NOMBRAMIENTOS")
  
  if (!resultado$tiene_nombramientos) {
    resultado$mensaje <- paste0(
      "El PDF es legible pero no contiene la sección 'NOMBRAMIENTOS'. ",
      "Verifique que sea un certificado de existencia CCB."
    )
    return(resultado)
  }
  
  resultado$ok      <- TRUE
  resultado$mensaje <- sprintf(
    "PDF válido: %d páginas, %s caracteres extraídos.",
    resultado$paginas, format(resultado$chars_totales, big.mark = ".")
  )
  resultado
}


# Extracción de texto del PDF ----

# Lee el PDF, concatena páginas y elimina el encabezado CCB repetido por página
extract_pdf_text <- function(pdf_path) {
  full <- pdf_text(pdf_path) %>% paste(collapse = "\n")
  full %>%
    str_replace_all(
      regex("Camara de Comercio.*?-{60,}\\s*", dotall = TRUE, ignore_case = TRUE), "\n"
    ) %>%
    str_replace_all("[ \t]+", " ") %>%
    str_replace_all("\n{3,}", "\n\n")
}

# Delimita el texto al bloque NOMBRAMIENTOS … RECURSOS CONTRA LOS ACTOS
extract_nombramientos_block <- function(text) {
  t_up  <- remove_accents(toupper(text))
  start <- str_locate(t_up, "NOMBRAMIENTOS")[1L, "start"]
  
  if (is.na(start)) {
    warning("Sección NOMBRAMIENTOS no encontrada; usando texto completo.")
    return(text)
  }
  
  tail    <- substr(t_up, start, nchar(t_up))
  end_rel <- str_locate(tail, "RECURSOS CONTRA LOS ACTOS")[1L, "start"]
  
  if (is.na(end_rel)) {
    warning("Marcador de fin no encontrado; tomando hasta el final.")
    return(substr(text, start, nchar(text)))
  }
  substr(text, start, start + end_rel - 2L)
}


# Pipeline regex — detección de secciones y cargos ----

# Detecta la sección activa del documento desde una línea normalizada
detect_section <- function(ln) {
  jd <- str_detect(ln, "JUNTA DIRECTIVA")
  if (jd && str_detect(ln, "PRINCIPAL")) return("JD_PRINCIPAL")
  if (jd && str_detect(ln, "SUPLENTE"))  return("JD_SUPLENTE")
  
  ca <- str_detect(ln, "CONSEJO") && str_detect(ln, "ADMINISTRACION")
  if (ca && str_detect(ln, "PRINCIPAL")) return("CA_PRINCIPAL")
  if (ca && str_detect(ln, "SUPLENTE"))  return("CA_SUPLENTE")
  
  cd <- str_detect(ln, "CONSEJO") && str_detect(ln, "DIRECTIVO")
  if (cd && str_detect(ln, "PRINCIPAL")) return("CD_PRINCIPAL")
  if (cd && str_detect(ln, "SUPLENTE"))  return("CD_SUPLENTE")
  
  cm <- str_detect(ln, "COMITE") && str_detect(ln, "ADMINISTRACION")
  if (cm && str_detect(ln, "PRINCIPAL")) return("CM_PRINCIPAL")
  if (cm && str_detect(ln, "SUPLENTE"))  return("CM_SUPLENTE")
  
  if (ln == "REPRESENTANTES LEGALES")         return("REP_LEGAL")
  if (ln == "REVISORES FISCALES")             return("REVISORES")
  if (str_detect(ln, "^SOCIOS|^ACCIONISTAS")) return("SOCIOS")
  
  NA_character_
}

# Detecta etiqueta de cargo y retorna el rol canónico
detect_cargo_role <- function(ln, section) {
  hit <- CARGO_TO_ROLE[ln]
  if (!is.na(hit)) return(unname(hit))
  
  if (str_detect(ln, RENGLON_PAT)) {
    fallback <- "MIEMBRO_JUNTA_DIRECTIVA"
    return(unname(SECTION_TO_RENGLON_ROLE[section %||% ""] %||% fallback))
  }
  NA_character_
}

# Extrae campos de una línea de persona via regex PERSON_PAT
parse_person_line <- function(ln) {
  m <- str_match(ln, PERSON_PAT)
  if (is.na(m[1L, 1L])) return(NULL)
  list(
    full_name = str_trim(m[1L, 2L]),
    doc_type  = str_replace_all(m[1L, 3L], "\\.", ""),
    doc_num   = clean_id(m[1L, 4L])
  )
}

# Recorre el bloque línea a línea manteniendo estado de sección y cargo activo
parse_block_contextual <- function(block) {
  lines   <- str_split(block, "\n")[[1]]
  items   <- list()
  section <- NA_character_
  cargo   <- NA_character_
  
  for (raw_line in lines) {
    ln <- normalize_line(raw_line)
    if (!nzchar(ln) || str_detect(ln, "^NOMBRE\\s+IDENTIFICACI")) next
    
    # Prioridad 1: línea de persona con cargo activo
    person <- parse_person_line(ln)
    if (!is.null(person) && !is.na(cargo)) {
      items[[length(items) + 1L]] <- list(
        role      = cargo,
        full_name = person$full_name,
        doc_type  = person$doc_type,
        doc_num   = person$doc_num
      )
      next
    }
    
    # Prioridad 2: cambio de sección (resetea cargo activo)
    new_section <- detect_section(ln)
    if (!is.na(new_section)) {
      section <- new_section
      cargo   <- NA_character_
      next
    }
    
    # Prioridad 3: etiqueta de cargo (actualiza cargo activo)
    new_cargo <- detect_cargo_role(ln, section)
    if (!is.na(new_cargo)) {
      cargo <- new_cargo
      next
    }
  }
  items
}


# Construcción del dataframe final ----

# Convierte la lista de items (role, full_name, doc_type, doc_num) al dataframe de cargue
build_admin_df <- function(items) {
  rows <- lapply(items, function(it) {
    role     <- it$role     %||% ""
    doc_type <- toupper(it$doc_type %||% "")
    
    admin_code <- ROLE_TO_ADMIN_CODE[role]
    doc_code   <- DOC_TYPE_TO_CODE[doc_type]
    
    if (is.na(admin_code)) {
      warning(sprintf("Rol '%s' sin mapeo -> fila omitida.", role))
      return(NULL)
    }
    if (is.na(doc_code)) {
      warning(sprintf("Tipo doc '%s' sin mapeo -> NA.", doc_type))
      doc_code <- NA_character_
    }
    
    nm <- split_ccb_name(it$full_name %||% "")
    
    data.frame(
      tipo_id_codigo    = as.character(doc_code),
      num_id            = as.character(it$doc_num %||% NA_character_),
      tipo_admin_codigo = as.integer(admin_code),
      tipo_admin_desc   = unname(ROLE_TO_ADMIN_DESC[as.character(admin_code)]),
      primer_apellido   = nm$pa,
      segundo_apellido  = nm$sa,
      primer_nombre     = nm$pn,
      segundo_nombre    = nm$sn,
      razon_social      = NA_character_,
      porcentaje        = NA_real_,
      id_administrador  = NA_character_,
      stringsAsFactors  = FALSE,
      row.names         = NULL
    )
  })
  
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0L) {
    warning("Ninguna fila válida construida.")
    return(data.frame())
  }
  bind_rows(rows) %>% arrange(tipo_admin_codigo)
}


# Función principal — extracción por regex ----

# PDF CCB → dataframe limpio vía regex. Uso interno; en producción usar
# extraer_administradores_ccb() que incluye fallback LLM automático.
.extraer_por_regex <- function(pdf_path) {
  resultado <- list(ok = FALSE, df = data.frame(), n_items = 0L,
                    mensaje = "", motor = "regex")
  
  raw_text <- tryCatch(
    extract_pdf_text(pdf_path),
    error = function(e) {
      resultado$mensaje <<- paste("Error leyendo PDF:", conditionMessage(e))
      NULL
    }
  )
  if (is.null(raw_text)) return(resultado)
  
  block <- extract_nombramientos_block(raw_text)
  items <- parse_block_contextual(block)
  
  if (length(items) == 0L) {
    resultado$mensaje <- paste0(
      "No se encontraron administradores en el bloque NOMBRAMIENTOS. ",
      "Verifique el formato del certificado."
    )
    return(resultado)
  }
  
  df <- build_admin_df(items)
  
  if (nrow(df) == 0L) {
    resultado$mensaje <- "Items detectados pero ninguno pudo mapearse a un rol válido."
    return(resultado)
  }
  
  resultado$ok      <- TRUE
  resultado$df      <- df
  resultado$n_items <- nrow(df)
  resultado$mensaje <- sprintf("%d administrador(es) extraído(s) por regex.", nrow(df))
  resultado
}


# Funciones LLM — prompt y llamada HTTP ----

# Constantes del cliente OpenAI
OPENAI_ENDPOINT  <- "https://api.openai.com/v1/chat/completions"
OPENAI_MODEL_DEF <- "gpt-4o-mini"
OPENAI_MAX_CHARS <- 12000L

# Genera el prompt del sistema leyendo roles directamente desde ROLE_TO_ADMIN_CODE.
# Garantiza sincronía entre prompt y tabla de mapeo sin mantenimiento manual.
.build_prompt_ccb <- function() {
  roles <- names(ROLE_TO_ADMIN_CODE) %>% paste(collapse = "\n  ")
  paste0(
    "Eres un extractor de certificados CCB (Cámara de Comercio de Bogotá).\n",
    "Extrae TODOS los administradores del bloque NOMBRAMIENTOS.\n\n",
    "Devuelve ÚNICAMENTE un array JSON válido sin markdown ni texto adicional.\n",
    "Cada objeto tiene exactamente estos campos:\n",
    "- \"role\": OBLIGATORIO, solo estos valores exactos:\n",
    "  ", roles, "\n\n",
    "Guía de mapeo de cargos CCB a roles:\n",
    "  Representante legal principal          -> REPRESENTANTE_LEGAL\n",
    "  Representante legal suplente           -> REPRESENTANTE_LEGAL_SUPLENTE\n",
    "  Gerente / Presidente                   -> GERENTE\n",
    "  Subgerente / Gerente suplente          -> GERENTE_SUPLENTE\n",
    "  Consejo de administración              -> MIEMBRO_CONSEJO_ADMINISTRACION\n",
    "  Consejo directivo                      -> MIEMBRO_CONSEJO_DIRECTIVO\n",
    "  Comité de administración               -> MIEMBRO_COMITE_ADMINISTRACION\n",
    "  Junta directiva (principal y suplente) -> MIEMBRO_JUNTA_DIRECTIVA\n",
    "  Revisor fiscal principal               -> REVISOR_FISCAL\n",
    "  Revisor fiscal suplente                -> REVISOR_FISCAL_SUPLENTE\n",
    "  Socio / Accionista                     -> SOCIO_ACCIONISTA\n",
    "  Contador                               -> CONTADOR\n",
    "  Beneficiario final                     -> BENEFICIARIO_FINAL\n\n",
    "- \"full_name\": nombre completo como aparece en el texto (APELLIDOS NOMBRES)\n",
    "- \"doc_type\": CC, CE, NIT, PA, PEP, TI o RC\n",
    "- \"doc_num\": número sin puntos ni espacios\n\n",
    "Si un campo no está disponible usa null. Nada fuera del array JSON."
  )
}

# Construye el body JSON para la petición chat/completions con JSON mode forzado
.build_openai_body <- function(block_text, model, temperatura) {
  list(
    model           = model,
    temperature     = temperatura,
    response_format = list(type = "json_object"),
    messages        = list(
      list(role = "system", content = .build_prompt_ccb()),
      list(role = "user",   content = paste0(
        "Extrae todos los administradores del siguiente texto CCB:\n\n",
        substr(block_text, 1L, OPENAI_MAX_CHARS)
      ))
    )
  )
}

# Ejecuta POST a OpenAI con reintentos automáticos en 429/5xx
.llamar_openai <- function(block_text,
                           api_key     = Sys.getenv("OPENAI_API_KEY"),
                           model       = OPENAI_MODEL_DEF,
                           temperatura = 0) {
  
  if (!requireNamespace("httr2",    quietly = TRUE)) stop("Paquete httr2 requerido.")
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Paquete jsonlite requerido.")
  if (nchar(api_key) == 0L) stop("OPENAI_API_KEY no definida o vacía.")
  
  resp <- tryCatch(
    httr2::request(OPENAI_ENDPOINT) %>%
      httr2::req_headers(
        Authorization  = paste("Bearer", api_key),
        `Content-Type` = "application/json"
      ) %>%
      httr2::req_body_json(.build_openai_body(block_text, model, temperatura)) %>%
      httr2::req_retry(max_tries = 3L, backoff = ~ 2 ^ .x) %>%
      httr2::req_error(is_error = function(r) FALSE) %>%
      httr2::req_perform(),
    error = function(e) stop("Error de red al llamar OpenAI: ", conditionMessage(e))
  )
  
  status <- httr2::resp_status(resp)
  if (status != 200L) {
    msg_err <- tryCatch(
      httr2::resp_body_json(resp)$error$message,
      error = function(e) paste("HTTP", status)
    )
    stop(sprintf("OpenAI devolvió %d: %s", status, msg_err))
  }
  
  content <- httr2::resp_body_json(resp, simplifyVector = FALSE)$choices[[1L]]$message$content
  if (is.null(content) || nchar(content) == 0L) stop("OpenAI devolvió contenido vacío.")
  content
}

# Convierte el JSON de OpenAI a lista de items compatibles con build_admin_df()
.parse_openai_items <- function(json_text) {
  limpio <- json_text %>%
    str_replace_all("(?s)^```json\\s*", "") %>%
    str_replace_all("(?s)```\\s*$",     "") %>%
    str_trim()
  
  parsed <- tryCatch(
    jsonlite::fromJSON(limpio, simplifyVector = FALSE),
    error = function(e) stop("JSON de OpenAI no parseable: ", conditionMessage(e))
  )
  
  # Normaliza a lista plana si el modelo envuelve en objeto raíz
  items_raw <- if (is.list(parsed) && !is.data.frame(parsed)) {
    listas <- Filter(is.list, parsed)
    if (length(listas) > 0L) listas[[1L]] else list()
  } else {
    parsed
  }
  
  if (length(items_raw) == 0L) {
    warning("OpenAI devolvió lista vacía de administradores.")
    return(list())
  }
  
  lapply(items_raw, function(it) {
    list(
      role      = it$role      %||% NA_character_,
      full_name = it$full_name %||% "",
      doc_type  = it$doc_type  %||% "",
      doc_num   = clean_id(as.character(it$doc_num %||% ""))
    )
  })
}

# Extracción LLM interna: bloque de texto ya extraído → dataframe
.extraer_por_llm <- function(pdf_path,
                             api_key     = Sys.getenv("OPENAI_API_KEY"),
                             model       = OPENAI_MODEL_DEF,
                             temperatura = 0) {
  
  resultado <- list(ok = FALSE, df = data.frame(), n_items = 0L,
                    mensaje = "", motor = "llm")
  
  raw_text <- tryCatch(
    extract_pdf_text(pdf_path),
    error = function(e) {
      resultado$mensaje <<- paste("Error leyendo PDF:", conditionMessage(e))
      NULL
    }
  )
  if (is.null(raw_text)) return(resultado)
  
  block    <- extract_nombramientos_block(raw_text)
  json_raw <- tryCatch(
    .llamar_openai(block, api_key = api_key, model = model, temperatura = temperatura),
    error = function(e) {
      resultado$mensaje <<- paste("Error en OpenAI:", conditionMessage(e))
      NULL
    }
  )
  if (is.null(json_raw)) return(resultado)
  
  items <- tryCatch(
    .parse_openai_items(json_raw),
    error = function(e) {
      resultado$mensaje <<- paste("Error parseando respuesta LLM:", conditionMessage(e))
      NULL
    }
  )
  if (is.null(items) || length(items) == 0L) {
    if (nchar(resultado$mensaje) == 0L) resultado$mensaje <- "OpenAI no devolvió administradores."
    return(resultado)
  }
  
  df <- tryCatch(
    build_admin_df(items),
    error = function(e) {
      resultado$mensaje <<- paste("Error construyendo dataframe:", conditionMessage(e))
      NULL
    }
  )
  if (is.null(df) || nrow(df) == 0L) return(resultado)
  
  resultado$ok      <- TRUE
  resultado$df      <- df
  resultado$n_items <- nrow(df)
  resultado$mensaje <- sprintf(
    "%d administrador(es) extraído(s) via OpenAI (%s).", nrow(df), model
  )
  resultado
}


# Función principal pública ----

# PDF CCB → dataframe limpio de administradores.
# Intenta regex primero; activa OpenAI solo si regex falla o devuelve 0 filas.
# Retorna lista con: ok, df, n_items, mensaje, motor ("regex" | "llm").
extraer_administradores_ccb <- function(pdf_path,
                                        api_key     = Sys.getenv("OPENAI_API_KEY"),
                                        model       = OPENAI_MODEL_DEF,
                                        temperatura = 0,
                                        verbose     = FALSE) {
  
  if (verbose) message("[CCB] Intentando extracción por regex...")
  
  res <- .extraer_por_regex(pdf_path)
  
  if (isTRUE(res$ok) && res$n_items > 0L) {
    if (verbose) message("[CCB] Regex exitoso: ", res$n_items, " registros.")
    return(res)
  }
  
  if (verbose) {
    message("[CCB] Regex sin resultados. Activando fallback OpenAI...")
    message("[CCB] Razón: ", res$mensaje)
  }
  
  res_llm <- .extraer_por_llm(pdf_path, api_key, model, temperatura)
  
  if (!isTRUE(res_llm$ok)) {
    res_llm$mensaje <- sprintf(
      "Regex: [%s] | LLM: [%s]", res$mensaje, res_llm$mensaje
    )
  }
  res_llm
}


# Escritura a plantilla de cargue ----

# Escribe el dataframe de administradores sobre la plantilla institucional Excel
escribir_plantilla_ccb <- function(df, plantilla_path) {
  wb <- loadWorkbook(plantilla_path)
  
  # Orden de columnas según la plantilla
  df_out <- df %>%
    select(tipo_id_codigo, num_id,
           tipo_admin_codigo, tipo_admin_desc,
           primer_nombre, segundo_nombre,
           primer_apellido, segundo_apellido,
           razon_social, porcentaje, id_administrador)
  
  # Datos desde fila 3 (después de instrucción y cabecera), columna B (2)
  writeData(wb, sheet = "ADMINISTRADORES", x = df_out,
            startRow = 3L, startCol = 2L, colNames = FALSE)
  
  tmp <- tempfile(fileext = ".xlsx")
  saveWorkbook(wb, tmp, overwrite = TRUE)
  tmp
}