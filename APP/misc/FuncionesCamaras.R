# 1. Constantes ----

UMBRAL_CHARS_POR_PAG    <- 150L
OPENAI_ENDPOINT         <- "https://api.openai.com/v1/chat/completions"
OPENAI_MODEL_DEF        <- "gpt-4o-mini"
OPENAI_MAX_CHARS        <- 12000L
DPI_VISION              <- 120L
MAX_PAGS_VISION         <- 8L
PAUSA_ENTRE_BATCHES_SEG <- 2L

CAMARAS_CONOCIDAS <- tribble(
  ~patron,                              ~nombre_camara,                       ~codigo,
  "CAMARA DE COMERCIO DE BOGOTA",       "Cámara de Comercio de Bogotá",       "CCB",
  "CAMARA DE COMERCIO DEL HUILA",       "Cámara de Comercio del Huila",       "CCH",
  "CAMARA DE COMERCIO DE CASANARE",     "Cámara de Comercio de Casanare",     "CCC",
  "CAMARA DE COMERCIO DE MEDELLIN",     "Cámara de Comercio de Medellín",     "CCM",
  "CAMARA DE COMERCIO DE CALI",         "Cámara de Comercio de Cali",         "CCA",
  "CAMARA DE COMERCIO DE BARRANQUILLA", "Cámara de Comercio de Barranquilla", "CCBA",
  "CAMARA DE COMERCIO DE BUCARAMANGA",  "Cámara de Comercio de Bucaramanga",  "CCBU",
  "CAMARA DE COMERCIO DE MANIZALES",    "Cámara de Comercio de Manizales",    "CCMZ"
)

# Roles canónicos válidos para los prompts LLM (sin aliases ni roles descartables)
.ROLES_VALIDOS <- c(
  "REPRESENTANTE_LEGAL", "REPRESENTANTE_LEGAL_SUPLENTE",
  "GERENTE", "GERENTE_SUPLENTE",
  "MIEMBRO_CONSEJO_ADMINISTRACION", "MIEMBRO_CONSEJO_DIRECTIVO",
  "MIEMBRO_COMITE_ADMINISTRACION",  "MIEMBRO_JUNTA_DIRECTIVA",
  "REVISOR_FISCAL", "REVISOR_FISCAL_SUPLENTE",
  "SOCIO_ACCIONISTA", "CONTADOR", "BENEFICIARIO_FINAL"
)


# 2. Utilidades de texto ----

remove_accents <- function(x) iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")

clean_id <- function(x) {
  x %>% str_replace_all("\\D", "") %>%
    sub(pattern = "^0+", replacement = "", x = .)
}

normalize_line <- function(x) {
  x %>% remove_accents() %>% toupper() %>%
    str_replace_all("\\s+", " ") %>% str_trim()
}

# Escapa caracteres especiales de regex para usar un string como literal en patrón
.regex_escape <- function(x) str_replace_all(x, "([.+*?^${}()|\\[\\]\\\\])", "\\\\1")

# Verifica si un token es una partícula que puede formar parte de un apellido compuesto
.es_particula <- function(tok) toupper(tok) %in% PARTICULAS_APELLIDO

# Descompone un nombre completo en pa/sa/pn/sn según el orden de la cámara.
#
# orden = "NOM_AP" (nombres primero — CCC, CCH y variantes):
#   Los apellidos están al FINAL. Las 2 últimas palabras son los apellidos,
#   pero si la penúltima es una partícula (DE, DEL, DE LA…) se extiende el
#   apellido hacia la izquierda hasta encontrar un token que no sea partícula.
#   Ej: MARIA DE LA ROSA PEREZ → pa="DE LA ROSA" sa="PEREZ" pn="MARIA"
#   Ej: ROCIO DEL PILAR SILVA ARANGUREN → pa="SILVA" sa="ARANGUREN" pn="ROCIO" sn="DEL PILAR"
#
# orden = "AP_NOM" (apellidos primero — CCB formal, mayoría):
#   Los apellidos están al INICIO. Split clásico: pa sa pn sn.
#   Ej: PARDO RAMIREZ SANTIAGO → pa="PARDO" sa="RAMIREZ" pn="SANTIAGO"
split_name_by_camara <- function(full_name, orden = "AP_NOM") {
  parts <- str_split(str_trim(full_name), "\\s+")[[1]]
  n     <- length(parts)
  empty <- list(pa = "", sa = "", pn = "", sn = "")
  if (n == 0L) return(empty)
  if (n == 1L) return(list(pa = parts[1], sa = "", pn = "", sn = ""))
  
  if (orden == "NOM_AP") {
    if (n == 2L) return(list(pa = parts[2], sa = "",        pn = parts[1], sn = ""))
    # Apellido 2 = última palabra (siempre simple)
    ap2 <- parts[n]
    # Apellido 1: empieza en n-1, se extiende hacia la izquierda
    # mientras el token ANTERIOR al candidato sea una partícula
    ap1_start <- n - 1L
    while (ap1_start > 1L && .es_particula(parts[ap1_start - 1L])) {
      ap1_start <- ap1_start - 1L
    }
    ap1         <- paste(parts[ap1_start:(n - 1L)], collapse = " ")
    nombre_parts <- parts[seq_len(ap1_start - 1L)]
    pn           <- if (length(nombre_parts) >= 1L) nombre_parts[1] else ""
    sn           <- if (length(nombre_parts) >= 2L) paste(nombre_parts[-1], collapse = " ") else ""
    list(pa = ap1, sa = ap2, pn = pn, sn = sn)
    
  } else {
    # AP_NOM: apellidos primero — split clásico sin extensión por partículas
    if (n == 2L) return(list(pa = parts[1], sa = "",        pn = parts[2],        sn = ""))
    if (n == 3L) return(list(pa = parts[1], sa = parts[2],  pn = parts[3],        sn = ""))
    list(pa = parts[1], sa = parts[2],
         pn = parts[3], sn = paste(parts[4:n], collapse = " "))
  }
}

.orden_por_camara <- function(codigo_camara) {
  unname(ORDEN_NOMBRES_CAMARA[codigo_camara] %||% "AP_NOM")
}


# 3. Funciones de PDF ----

clasificar_pdf <- function(pdf_path) {
  resultado <- list(tipo_pdf = "error", paginas = 0L,
                    chars_por_pagina = integer(0), chars_totales = 0L,
                    pags_texto = 0L, pags_escaneadas = 0L, mensaje_tipo = "")
  info <- tryCatch(pdftools::pdf_info(pdf_path), error = function(e) NULL)
  if (is.null(info)) {
    resultado$mensaje_tipo <- "No es un PDF válido o está protegido."
    return(resultado)
  }
  resultado$paginas <- as.integer(info$pages)
  textos_pag <- tryCatch(pdftools::pdf_text(pdf_path),
                         error = function(e) character(resultado$paginas))
  chars_pag <- nchar(textos_pag)
  resultado$chars_por_pagina <- chars_pag
  resultado$chars_totales    <- sum(chars_pag)
  resultado$pags_texto       <- sum(chars_pag >= UMBRAL_CHARS_POR_PAG)
  resultado$pags_escaneadas  <- sum(chars_pag <  UMBRAL_CHARS_POR_PAG)
  prop <- resultado$pags_texto / resultado$paginas
  resultado$tipo_pdf <- dplyr::case_when(
    prop >= 0.9 ~ "texto",
    prop <= 0.1 ~ "escaneado",
    TRUE        ~ "mixto"
  )
  resultado$mensaje_tipo <- switch(resultado$tipo_pdf,
                                   texto     = sprintf("PDF texto digital — %d págs.", resultado$paginas),
                                   escaneado = sprintf("PDF escaneado — %d págs. Se procesará con LLM vision.",
                                                       resultado$paginas),
                                   mixto     = sprintf("PDF mixto — %d pág. texto / %d escaneadas.",
                                                       resultado$pags_texto, resultado$pags_escaneadas)
  )
  resultado
}
detectar_camara <- function(texto_completo) {
  resultado <- list(ok = FALSE, codigo = "DESCONOCIDA",
                    nombre_camara = "Cámara desconocida", mensaje = "")
  cabecera <- texto_completo %>% substr(1L, 3000L) %>% remove_accents() %>% toupper()
  for (i in seq_len(nrow(CAMARAS_CONOCIDAS))) {
    if (str_detect(cabecera, fixed(CAMARAS_CONOCIDAS$patron[i]))) {
      resultado$ok            <- TRUE
      resultado$codigo        <- CAMARAS_CONOCIDAS$codigo[i]
      resultado$nombre_camara <- CAMARAS_CONOCIDAS$nombre_camara[i]
      resultado$mensaje       <- sprintf("Emisor: %s (%s).",
                                         resultado$nombre_camara, resultado$codigo)
      return(resultado)
    }
  }
  resultado$mensaje <- "Cámara no identificada en la tabla de reconocidas."
  resultado
}
extract_pdf_text <- function(pdf_path) {
  pdftools::pdf_text(pdf_path) %>%
    paste(collapse = "\n") %>%
    str_replace_all(
      regex("C[aá]mara de Comercio.*?-{60,}\\s*", dotall = TRUE, ignore_case = TRUE), "\n"
    ) %>%
    str_replace_all("[ \t]+", " ") %>%
    str_replace_all("\n{3,}", "\n\n")
}


# 4. Extracción del bloque NOMBRAMIENTOS ----

# Delimita NOMBRAMIENTOS…RECURSOS buscando "NOMBRAMIENTOS" como línea sola
# para no confundirlo con el sub-encabezado "** Nombramientos **" de cada acta.
extract_nombramientos_block <- function(text) {
  t_up  <- remove_accents(toupper(text))
  m     <- str_locate(t_up, regex("(?m)^\\s*NOMBRAMIENTOS\\s*$"))[1L, ]
  start <- m["start"]
  if (is.na(start)) start <- str_locate(t_up, "NOMBRAMIENTOS")[1L, "start"]
  if (is.na(start)) { warning("NOMBRAMIENTOS no encontrado."); return(text) }
  tail    <- substr(t_up, start, nchar(t_up))
  end_rel <- str_locate(tail, "RECURSOS CONTRA LOS ACTOS")[1L, "start"]
  if (is.na(end_rel)) return(substr(text, start, nchar(text)))
  substr(text, start, start + end_rel - 2L)
}

limpiar_texto_cerl <- function(texto, codigo_camara = "CCB") {
  patron <- switch(codigo_camara,
                   CCB = regex("C[aá]mara de Comercio.*?-{40,}\\s*",
                               dotall = TRUE, ignore_case = TRUE),
                   CCH = regex("C[aá]MARA DE COMERCIO DEL HUILA.*?CODIGO DE VERIFICACI[oó]N[^\n]+\n",
                               dotall = TRUE, ignore_case = TRUE),
                   CCC = regex("C[aá]MARA DE COMERCIO DE CASANARE.*?CODIGO DE VERIFICACI[oó]N[^\n]+\n",
                               dotall = TRUE, ignore_case = TRUE),
                   regex("C[aá]MARA DE COMERCIO.*?CODIGO DE VERIFICACI[oó]N[^\n]+\n",
                         dotall = TRUE, ignore_case = TRUE)
  )
  texto %>%
    str_replace_all(patron, "\n") %>%
    str_replace_all("[ \t]+", " ") %>%
    str_replace_all("\n{3,}", "\n\n") %>%
    str_trim()
}


# 5. Regex de parsing de líneas ----

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
  if (ln == "REPRESENTANTES LEGALES")                      return("REP_LEGAL")
  if (ln == "REVISORES FISCALES")                          return("REVISORES")
  if (str_detect(ln, "^SOCIOS|^ACCIONISTAS"))              return("SOCIOS")
  if (str_detect(ln, "ORGANO DE ADMINISTRACION"))          return("ORGANO_ADMIN")
  if (str_detect(ln, "^REFORMAS DE ESTATUTOS|^REFORMAS:")) return("REFORMAS")
  if (ln == "PRINCIPALES") return("CA_PRINCIPAL")
  if (ln == "SUPLENTES")   return("CA_SUPLENTE")
  NA_character_
}

detect_cargo_role <- function(ln, section) {
  hit <- CARGO_TO_ROLE[ln]
  if (!is.na(hit)) return(unname(hit))
  if (str_detect(ln, RENGLON_PAT)) {
    fallback <- "MIEMBRO_JUNTA_DIRECTIVA"
    return(unname(SECTION_TO_RENGLON_ROLE[section %||% ""] %||% fallback))
  }
  NA_character_
}

parse_person_line <- function(ln) {
  m <- str_match(ln, PERSON_PAT)
  if (is.na(m[1L, 1L])) return(NULL)
  list(full_name = str_trim(m[1L, 2L]),
       doc_type  = str_replace_all(m[1L, 3L], "\\.", ""),
       doc_num   = clean_id(m[1L, 4L]))
}

# parse_combined_line: cargo + persona en la misma línea (formato tabular).
# BUG ANTERIOR corregido: str_starts + fixed() no funcionaba — se usa str_detect + regex_escape.
parse_combined_line <- function(ln) {
  cargos <- names(CARGO_TO_ROLE)[order(-nchar(names(CARGO_TO_ROLE)))]
  for (cargo_key in cargos) {
    pat <- paste0("^", .regex_escape(cargo_key), "\\s")
    if (!str_detect(ln, pat)) next
    resto  <- str_trim(str_remove(ln, paste0("^", .regex_escape(cargo_key), "\\s+")))
    person <- parse_person_line(resto)
    if (!is.null(person) && nzchar(person$doc_num)) {
      return(list(role      = unname(CARGO_TO_ROLE[cargo_key]),
                  full_name = person$full_name,
                  doc_type  = person$doc_type,
                  doc_num   = person$doc_num))
    }
  }
  NULL
}

.es_continuacion_nombre <- function(ln) {
  !str_detect(ln, paste0(
    "\\d|C\\.C\\.|C\\.E\\.|NIT|PPT|PEP|",
    "CARGO|NOMBRE|RECURSO|PAGINA|T\\.\\s*PROF|",
    "CAMARA|COMERCIO|CERTIFICADO|SEDE|",
    "EXISTENCIA|REPRESENTACION|VERIFICACION|",
    "EXPEDICION|RECIBO|VALOR|CODIGO|INSCRIPCION|",
    "INFORMACION|CLASIFICACION|ACTIVIDADES|",
    "RENGLON|ORGANO|REFORMAS|ESTATUTOS|",
    "REVISORES|SOCIOS|ACCIONISTAS|JUNTA|",
    "PRINCIPALES|SUPLENTES"
  )) &&
    str_detect(ln, "^[A-ZÁÉÍÓÚÜÑ][A-ZÁÉÍÓÚÜÑ ]+$") &&
    nchar(ln) <= 40L
}

parse_block_contextual <- function(block) {
  lines      <- str_split(block, "\n")[[1]]
  items      <- list()
  section    <- NA_character_
  cargo      <- NA_character_
  ultimo_idx <- 0L
  
  for (raw_line in lines) {
    ln <- normalize_line(raw_line)
    if (!nzchar(ln)) next
    if (str_detect(ln, "^(NOMBRE|CARGO)\\s+(NOMBRE|IDENTIFICACI)")) next
    if (str_detect(ln, "^CARGO\\s+NOMBRE\\s+IDENTIFICACION")) next
    
    # P1: cargo + persona en la misma línea (tabular)
    combined <- parse_combined_line(ln)
    if (!is.null(combined)) {
      items[[length(items) + 1L]] <- combined
      ultimo_idx <- length(items)
      cargo      <- NA_character_
      next
    }
    
    # P2: persona con cargo activo previo (multilinea clásico)
    person <- parse_person_line(ln)
    if (!is.null(person) && !is.na(cargo)) {
      items[[length(items) + 1L]] <- list(role      = cargo,
                                          full_name = person$full_name,
                                          doc_type  = person$doc_type,
                                          doc_num   = person$doc_num)
      ultimo_idx <- length(items)
      next
    }
    
    # P3: "SUPLENTE ..." después de GERENTE → GERENTE_SUPLENTE
    if (str_starts(ln, "SUPLENTE") && ultimo_idx > 0L &&
        items[[ultimo_idx]]$role == "GERENTE") {
      items[[ultimo_idx]]$role <- "GERENTE_SUPLENTE"
      resto <- str_trim(str_remove(ln, "^SUPLENTE\\s*"))
      if (nzchar(resto)) items[[ultimo_idx]]$full_name <- paste(items[[ultimo_idx]]$full_name, resto)
      ultimo_idx <- 0L
      next
    }
    
    # P4: cambio de sección
    new_section <- detect_section(ln)
    if (!is.na(new_section)) {
      section    <- new_section
      cargo      <- NA_character_
      ultimo_idx <- 0L
      next
    }
    
    # P5: cargo en su propia línea
    new_cargo <- detect_cargo_role(ln, section)
    if (!is.na(new_cargo)) {
      cargo      <- new_cargo
      ultimo_idx <- 0L
      next
    }
    
    # P6: continuación de nombre (apellido en línea siguiente)
    if (ultimo_idx > 0L && .es_continuacion_nombre(ln)) {
      items[[ultimo_idx]]$full_name <- paste(items[[ultimo_idx]]$full_name, ln)
      next
    }
  }
  items
}


# 6. Extracción por regex ----

.extraer_por_regex <- function(texto_limpio) {
  resultado <- list(ok = FALSE, df = data.frame(), n_items = 0L, mensaje = "")
  block <- tryCatch(extract_nombramientos_block(texto_limpio),
                    error = function(e) { resultado$mensaje <<- conditionMessage(e); NULL })
  if (is.null(block)) return(resultado)
  if (nchar(block) < 50L) {
    resultado$mensaje <- sprintf(
      "Regex: bloque NOMBRAMIENTOS muy corto (%d chars) — posible fallo de delimitación.",
      nchar(block)
    )
    return(resultado)
  }
  items <- parse_block_contextual(block)
  if (length(items) == 0L) {
    resultado$mensaje <- "Regex: no se encontraron administradores en el bloque."
    return(resultado)
  }
  df <- build_admin_df(items)
  if (nrow(df) == 0L) {
    resultado$mensaje <- "Regex: sin mapeo de rol válido para los items encontrados."
    return(resultado)
  }
  resultado$ok      <- TRUE
  resultado$df      <- df
  resultado$n_items <- nrow(df)
  resultado$mensaje <- sprintf("Regex: %d admin(s).", nrow(df))
  resultado
}


# 7. LLM texto ----

.build_prompt_texto <- function() {
  roles <- paste(.ROLES_VALIDOS, collapse = "\n  ")
  paste0(
    "Eres un extractor de Certificados de Existencia y Representación Legal ",
    "de cámaras de comercio colombianas.\n",
    "Extrae TODOS los administradores del bloque NOMBRAMIENTOS.\n\n",
    "REGLA CRÍTICA de roles: usa ÚNICAMENTE los valores de la lista de abajo.\n",
    "Está PROHIBIDO inventar variantes. Si el cargo no coincide exactamente ",
    "con la guía, elige el valor más cercano de la lista.\n\n",
    "IMPORTANTE sobre nombres:\n",
    "  Copia full_name exactamente como aparece en el documento, sin reordenar.\n\n",
    "Para personas jurídicas (NIT): full_name = razón social completa.\n\n",
    "Devuelve ÚNICAMENTE un array JSON válido sin markdown ni texto adicional.\n",
    "Cada objeto tiene exactamente estos campos:\n",
    "- \"role\": OBLIGATORIO — solo estos valores exactos:\n  ", roles, "\n\n",
    "Guía de mapeo de cargos a roles:\n",
    "  Representante legal / RL principal     -> REPRESENTANTE_LEGAL\n",
    "  Representante legal suplente           -> REPRESENTANTE_LEGAL_SUPLENTE\n",
    "  Gerente / Presidente                   -> GERENTE\n",
    "  Subgerente / Gerente suplente          -> GERENTE_SUPLENTE\n",
    "  Director ejecutivo / Director general  -> GERENTE\n",
    "  Firma revisora / Delegado de la firma  -> REVISOR_FISCAL\n",
    "  Revisor fiscal principal               -> REVISOR_FISCAL\n",
    "  Revisor fiscal suplente                -> REVISOR_FISCAL_SUPLENTE\n",
    "  Consejo de administración (ppal/sup)   -> MIEMBRO_CONSEJO_ADMINISTRACION\n",
    "  Consejo directivo (ppal/sup)           -> MIEMBRO_CONSEJO_DIRECTIVO\n",
    "  Comité de administración (ppal/sup)    -> MIEMBRO_COMITE_ADMINISTRACION\n",
    "  Junta directiva (ppal/sup)             -> MIEMBRO_JUNTA_DIRECTIVA\n",
    "  Revisor fiscal principal               -> REVISOR_FISCAL\n",
    "  Revisor fiscal suplente                -> REVISOR_FISCAL_SUPLENTE\n",
    "  Socio / Accionista                     -> SOCIO_ACCIONISTA\n",
    "  Contador                               -> CONTADOR\n",
    "  Beneficiario final                     -> BENEFICIARIO_FINAL\n\n",
    "- \"full_name\": nombre completo tal como aparece en el documento\n",
    "- \"doc_type\": CC, CE, NIT, PA, PEP, TI o RC\n",
    "- \"doc_num\": número sin puntos, comas, espacios ni dígito verificador\n",
    "Si un campo no está disponible usa null. Nada fuera del array JSON."
  )
}

.llamar_openai_texto <- function(block, api_key, model = OPENAI_MODEL_DEF, temperatura = 0) {
  texto <- substr(block, 1L, OPENAI_MAX_CHARS)
  resp  <- httr::POST(
    url    = OPENAI_ENDPOINT,
    httr::add_headers(Authorization  = paste("Bearer", api_key),
                      `Content-Type` = "application/json"),
    body   = jsonlite::toJSON(list(
      model       = model,
      temperature = temperatura,
      messages    = list(
        list(role = "system", content = .build_prompt_texto()),
        list(role = "user",   content = texto)
      )
    ), auto_unbox = TRUE),
    encode = "raw"
  )
  httr::stop_for_status(resp)
  cnt <- httr::content(resp, as = "parsed", encoding = "UTF-8")
  cnt$choices[[1]]$message$content
}

.parse_openai_items <- function(json_raw) {
  clean     <- json_raw %>% str_replace_all("```json|```", "") %>% str_trim()
  items_raw <- tryCatch(jsonlite::fromJSON(clean, simplifyVector = FALSE),
                        error = function(e) list())
  if (!is.list(items_raw) || length(items_raw) == 0L) return(list())
  lapply(items_raw, function(it) {
    list(role      = it$role      %||% NA_character_,
         full_name = it$full_name %||% "",
         doc_type  = it$doc_type  %||% "",
         doc_num   = clean_id(as.character(it$doc_num %||% "")))
  })
}

.extraer_por_llm_texto <- function(texto_limpio,
                                   api_key     = Sys.getenv("OPENAI_API_KEY"),
                                   model       = OPENAI_MODEL_DEF,
                                   temperatura = 0) {
  resultado <- list(ok = FALSE, df = data.frame(), n_items = 0L, mensaje = "")
  if (nchar(api_key) < 10L) { resultado$mensaje <- "LLM: API key no configurada."; return(resultado) }
  block <- tryCatch(extract_nombramientos_block(texto_limpio),
                    error = function(e) { resultado$mensaje <<- conditionMessage(e); NULL })
  if (is.null(block)) return(resultado)
  json_raw <- tryCatch(
    .llamar_openai_texto(block, api_key, model, temperatura),
    error = function(e) { resultado$mensaje <<- paste("LLM:", conditionMessage(e)); NULL }
  )
  if (is.null(json_raw)) return(resultado)
  items <- tryCatch(.parse_openai_items(json_raw),
                    error = function(e) { resultado$mensaje <<- conditionMessage(e); NULL })
  if (is.null(items) || length(items) == 0L) {
    if (!nzchar(resultado$mensaje)) resultado$mensaje <- "LLM: respuesta vacía."
    return(resultado)
  }
  df <- tryCatch(build_admin_df(items),
                 error = function(e) { resultado$mensaje <<- conditionMessage(e); NULL })
  if (is.null(df) || nrow(df) == 0L) return(resultado)
  resultado$ok      <- TRUE
  resultado$df      <- df
  resultado$n_items <- nrow(df)
  resultado$mensaje <- sprintf("LLM texto: %d admin(s).", nrow(df))
  resultado
}


# 8. LLM vision ----

.build_prompt_vision <- function() {
  roles <- paste(.ROLES_VALIDOS, collapse = "\n  ")
  paste0(
    "Estas imágenes son páginas de un Certificado de Existencia y Representación Legal ",
    "de una cámara de comercio colombiana.\n",
    "Extrae TODOS los administradores en la sección NOMBRAMIENTOS.\n\n",
    "REGLA CRÍTICA de roles: usa ÚNICAMENTE los valores de la lista de abajo.\n",
    "Está PROHIBIDO inventar variantes. Si el cargo no coincide, elige el más cercano.\n\n",
    "Para personas jurídicas (NIT): full_name = razón social completa.\n\n",
    "Devuelve ÚNICAMENTE un array JSON válido sin markdown ni texto adicional.\n",
    "Cada objeto tiene exactamente estos campos:\n",
    "- \"role\": OBLIGATORIO — solo estos valores exactos:\n  ", roles, "\n\n",
    "Guía de mapeo:\n",
    "  Representante legal / RL principal     -> REPRESENTANTE_LEGAL\n",
    "  Representante legal suplente           -> REPRESENTANTE_LEGAL_SUPLENTE\n",
    "  Gerente / Presidente                   -> GERENTE\n",
    "  Subgerente / Gerente suplente          -> GERENTE_SUPLENTE\n",
    "  Director ejecutivo / Director general  -> GERENTE\n",
    "  Firma revisora / Delegado de la firma  -> REVISOR_FISCAL\n",
    "  Revisor fiscal principal               -> REVISOR_FISCAL\n",
    "  Revisor fiscal suplente                -> REVISOR_FISCAL_SUPLENTE\n",
    "  Consejo de administración (ppal/sup)   -> MIEMBRO_CONSEJO_ADMINISTRACION\n",
    "  Consejo directivo (ppal/sup)           -> MIEMBRO_CONSEJO_DIRECTIVO\n",
    "  Comité de administración (ppal/sup)    -> MIEMBRO_COMITE_ADMINISTRACION\n",
    "  Junta directiva (ppal/sup)             -> MIEMBRO_JUNTA_DIRECTIVA\n",
    "  Socio / Accionista                     -> SOCIO_ACCIONISTA\n",
    "  Contador                               -> CONTADOR\n",
    "  Beneficiario final                     -> BENEFICIARIO_FINAL\n\n",
    "- \"full_name\": nombre completo tal como aparece en el documento\n",
    "- \"doc_type\": CC, CE, NIT, PA, PEP, TI o RC\n",
    "- \"doc_num\": número sin puntos, comas, espacios ni dígito verificador\n",
    "Si un campo no está disponible usa null. Nada fuera del array JSON."
  )
}

.pagina_a_b64 <- function(pdf_path, numero_pagina, dpi = DPI_VISION) {
  bitmap <- tryCatch(
    pdftools::pdf_render_page(pdf_path, page = numero_pagina, dpi = dpi, numeric = FALSE),
    error = function(e) NULL
  )
  if (is.null(bitmap)) return(NULL)
  tryCatch({
    raw_png <- magick::image_read(bitmap) %>% magick::image_write(format = "png")
    jsonlite::base64_enc(raw_png)
  }, error = function(e) NULL)
}

# Reintenta ante errores 429 con backoff exponencial y pausa mínima de 60s en rate limit.
.con_reintento <- function(fn, max_intentos = 6L, verbose = FALSE) {
  for (intento in seq_len(max_intentos)) {
    resultado <- tryCatch(fn(), error = function(e) e)
    if (!inherits(resultado, "error")) return(resultado)
    msg    <- conditionMessage(resultado)
    es_429 <- str_detect(msg, "429|Too Many Requests|Rate limit")
    if (!es_429 || intento == max_intentos) stop(resultado)
    # Para 429: pausa mínima de 60s o Retry-After del header, lo que sea mayor
    espera <- tryCatch({
      ra <- resultado$response$headers[["retry-after"]]
      ra_num <- suppressWarnings(as.numeric(ra))
      base   <- 2^intento
      if (!is.na(ra_num)) max(ra_num + 1, 60) else max(base, 60)
    }, error = function(e) max(2^intento, 60))
    if (verbose) message(sprintf(
      "[Reintento %d/%d] 429 — esperando %.0f s...",
      intento, max_intentos - 1L, espera
    ))
    Sys.sleep(espera)
  }
}

.llamar_openai_vision <- function(paginas_b64, api_key, model = OPENAI_MODEL_DEF) {
  content <- c(
    list(list(type = "text", text = .build_prompt_vision())),
    lapply(paginas_b64, function(b64) {
      list(type      = "image_url",
           image_url = list(url = paste0("data:image/png;base64,", b64)))
    })
  )
  resp <- .con_reintento(function() {
    r <- httr::POST(
      url    = OPENAI_ENDPOINT,
      httr::add_headers(Authorization  = paste("Bearer", api_key),
                        `Content-Type` = "application/json"),
      body   = jsonlite::toJSON(list(
        model    = model,
        messages = list(list(role = "user", content = content))
      ), auto_unbox = TRUE),
      encode = "raw"
    )
    httr::stop_for_status(r)
    r
  }, max_intentos = 6L, verbose = TRUE)
  cnt <- httr::content(resp, as = "parsed", encoding = "UTF-8")
  cnt$choices[[1]]$message$content
}

.extraer_escaneado_vision <- function(pdf_path,
                                      api_key       = Sys.getenv("OPENAI_API_KEY"),
                                      model         = OPENAI_MODEL_DEF,
                                      dpi           = DPI_VISION,
                                      codigo_camara = "default") {
  resultado <- list(ok = FALSE, df = data.frame(), n_items = 0L,
                    mensaje = "", n_batches = 0L)
  if (nchar(api_key) < 10L) {
    resultado$mensaje <- "Vision LLM: API key no configurada."
    return(resultado)
  }
  n_pags <- tryCatch(as.integer(pdftools::pdf_info(pdf_path)$pages),
                     error = function(e) 0L)
  if (n_pags == 0L) { resultado$mensaje <- "No se pudo leer el número de páginas."; return(resultado) }
  skip_cfg    <- SKIP_PAGS_CAMARA[[codigo_camara]] %||% SKIP_PAGS_CAMARA[["default"]]
  pag_inicio  <- skip_cfg$inicio + 1L
  pag_fin     <- max(pag_inicio, n_pags - skip_cfg$fin)
  pags_utiles <- seq(pag_inicio, pag_fin)
  if (length(pags_utiles) == 0L) {
    resultado$mensaje <- sprintf("Vision: skip deja 0 páginas útiles (total=%d).", n_pags)
    return(resultado)
  }
  message(sprintf("[Vision] %s: %d págs. útiles de %d (skip ini=%d, fin=%d).",
                  codigo_camara, length(pags_utiles), n_pags,
                  skip_cfg$inicio, skip_cfg$fin))
  batches     <- split(pags_utiles, ceiling(seq_along(pags_utiles) / MAX_PAGS_VISION))
  resultado$n_batches <- length(batches)
  todos_items <- list()
  for (batch_idx in seq_along(batches)) {
    if (batch_idx > 1L) {
      message(sprintf("[Vision] Pausa %ds entre batches...", PAUSA_ENTRE_BATCHES_SEG))
      Sys.sleep(PAUSA_ENTRE_BATCHES_SEG)
    }
    pags_batch <- batches[[batch_idx]]
    message(sprintf("[Vision] Batch %d/%d (págs. %d-%d)...",
                    batch_idx, length(batches), min(pags_batch), max(pags_batch)))
    b64_list <- lapply(pags_batch, function(p) .pagina_a_b64(pdf_path, p, dpi))
    b64_list <- Filter(Negate(is.null), b64_list)
    if (length(b64_list) == 0L) next
    json_raw <- tryCatch(
      .llamar_openai_vision(b64_list, api_key, model),
      error = function(e) {
        message(sprintf("[Vision] Error batch %d: %s", batch_idx, conditionMessage(e)))
        NULL
      }
    )
    if (is.null(json_raw)) next
    items_batch <- tryCatch(.parse_openai_items(json_raw), error = function(e) list())
    todos_items <- c(todos_items, items_batch)
  }
  if (length(todos_items) == 0L) {
    resultado$mensaje <- "Vision LLM: ningún administrador detectado."
    return(resultado)
  }
  df <- tryCatch(build_admin_df(todos_items),
                 error = function(e) { resultado$mensaje <<- conditionMessage(e); NULL })
  if (is.null(df) || nrow(df) == 0L) return(resultado)
  df <- df %>%
    group_by(num_id) %>% slice(1L) %>% ungroup() %>%
    arrange(tipo_admin_codigo)
  resultado$ok      <- TRUE
  resultado$df      <- df
  resultado$n_items <- nrow(df)
  resultado$mensaje <- sprintf("Vision LLM: %d admin(s) en %d batch(es).",
                               nrow(df), resultado$n_batches)
  resultado
}


# 9. Extracción unificada ----

extraer_administradores <- function(pdf_path         = NULL,
                                    texto_limpio      = "",
                                    tipo_pdf          = "texto",
                                    codigo_camara     = "CCB",
                                    api_key           = Sys.getenv("OPENAI_API_KEY"),
                                    model             = OPENAI_MODEL_DEF,
                                    verbose           = FALSE) {
  if (tipo_pdf %in% c("texto", "mixto") && nchar(texto_limpio) >= 100L) {
    return(.extraer_dual_texto(texto_limpio, codigo_camara, api_key, model, verbose))
  }
  if (!is.null(pdf_path)) {
    return(.extraer_vision(pdf_path, codigo_camara, api_key, model, verbose))
  }
  list(ok = FALSE, df = data.frame(), n_items = 0L, n_regex = 0L, n_llm = 0L,
       mensaje_regex = "Sin texto disponible.", mensaje_llm = "Sin PDF para vision.",
       metodo = "ninguno")
}

.extraer_dual_texto <- function(texto_limpio, codigo_camara, api_key, model, verbose) {
  if (verbose) message("[DUAL] Regex...")
  res_regex <- .extraer_por_regex(texto_limpio)
  if (verbose) message("[DUAL] LLM texto...")
  res_llm   <- .extraer_por_llm_texto(texto_limpio, api_key, model)
  if (!isTRUE(res_regex$ok) && !isTRUE(res_llm$ok)) {
    return(list(ok = FALSE, df = data.frame(), n_items = 0L, n_regex = 0L, n_llm = 0L,
                mensaje_regex = res_regex$mensaje, mensaje_llm = res_llm$mensaje,
                metodo = "dual"))
  }
  if (isTRUE(res_regex$ok) && !isTRUE(res_llm$ok)) {
    return(list(ok = TRUE, df = res_regex$df, n_items = res_regex$n_items,
                n_regex = res_regex$n_items, n_llm = 0L,
                mensaje_regex = res_regex$mensaje, mensaje_llm = res_llm$mensaje,
                metodo = "dual"))
  }
  if (!isTRUE(res_regex$ok) && isTRUE(res_llm$ok)) {
    return(list(ok = TRUE, df = res_llm$df, n_items = res_llm$n_items,
                n_regex = 0L, n_llm = res_llm$n_items,
                mensaje_regex = res_regex$mensaje, mensaje_llm = res_llm$mensaje,
                metodo = "dual"))
  }
  ids_llm_nuevos <- setdiff(res_llm$df$num_id, res_regex$df$num_id)
  df_extra <- res_llm$df %>% filter(num_id %in% ids_llm_nuevos)
  df_final <- bind_rows(res_regex$df, df_extra) %>% arrange(tipo_admin_codigo)
  list(ok = TRUE, df = df_final, n_items = nrow(df_final),
       n_regex = res_regex$n_items, n_llm = res_llm$n_items,
       mensaje_regex = res_regex$mensaje, mensaje_llm = res_llm$mensaje,
       metodo = "dual")
}

.extraer_vision <- function(pdf_path, codigo_camara, api_key, model, verbose) {
  if (verbose) message("[VISION] Extrayendo desde imágenes...")
  res <- .extraer_escaneado_vision(pdf_path, api_key, model, codigo_camara = codigo_camara)
  list(ok = res$ok, df = res$df, n_items = res$n_items, n_regex = 0L,
       n_llm = res$n_items, mensaje_regex = "PDF escaneado — regex no aplica.",
       mensaje_llm = res$mensaje, metodo = "vision")
}


# 10. Validación y pipeline principal ----

validar_cerl <- function(pdf_path) {
  resultado <- list(
    ok = FALSE, paginas = 0L, chars_totales = 0L, tipo_pdf = "error",
    camara = list(ok = FALSE, codigo = "DESCONOCIDA", nombre_camara = ""),
    tiene_nombramientos = FALSE, texto_extraido = "",
    mensaje = "", advertencias = character(0)
  )
  cls <- clasificar_pdf(pdf_path)
  resultado$paginas  <- cls$paginas
  resultado$tipo_pdf <- cls$tipo_pdf
  if (cls$tipo_pdf == "error") { resultado$mensaje <- cls$mensaje_tipo; return(resultado) }
  if (cls$tipo_pdf %in% c("texto", "mixto")) {
    texto_crudo <- tryCatch(
      pdftools::pdf_text(pdf_path) %>% paste(collapse = "\n"), error = function(e) ""
    )
    texto_raw <- tryCatch(extract_pdf_text(pdf_path), error = function(e) "")
    resultado$chars_totales  <- nchar(texto_raw)
    resultado$texto_extraido <- texto_raw
    resultado$camara         <- detectar_camara(texto_crudo)
    texto_up <- remove_accents(toupper(texto_raw))
    resultado$tiene_nombramientos <- str_detect(texto_up, "NOMBRAMIENTOS")
  } else {
    resultado$chars_totales       <- 0L
    resultado$texto_extraido      <- ""
    resultado$tiene_nombramientos <- TRUE
    resultado$camara <- list(ok = FALSE, codigo = "DESCONOCIDA",
                             nombre_camara = "Desconocida (escaneado)", mensaje = "")
  }
  if (!resultado$camara$ok)
    resultado$advertencias <- c(resultado$advertencias, resultado$camara$mensaje)
  if (cls$tipo_pdf != "escaneado" && !resultado$tiene_nombramientos) {
    resultado$mensaje <- "El documento no contiene la sección 'NOMBRAMIENTOS'."
    return(resultado)
  }
  resultado$ok <- TRUE
  resultado$mensaje <- sprintf("%s | %s | %d págs%s",
                               resultado$camara$nombre_camara,
                               toupper(resultado$tipo_pdf),
                               resultado$paginas,
                               if (resultado$chars_totales > 0L)
                                 sprintf(" | %s chars", format(resultado$chars_totales, big.mark = "."))
                               else " | imagen")
  resultado
}

procesar_cerl <- function(pdf_path,
                          nombre_archivo = NULL,
                          api_key        = Sys.getenv("OPENAI_API_KEY"),
                          model          = OPENAI_MODEL_DEF,
                          verbose        = FALSE) {
  t_inicio   <- Sys.time()
  nombre_log <- nombre_archivo %||% basename(pdf_path)
  if (verbose) message("[CERL] Iniciando: ", nombre_log)
  val <- validar_cerl(pdf_path)
  if (!isTRUE(val$ok)) {
    return(list(ok = FALSE, validacion = val, extraccion = NULL,
                texto_limpio = "", camara = val$camara, mensaje = val$mensaje,
                tiempo_seg = round(as.numeric(difftime(Sys.time(), t_inicio, units = "secs")), 1)))
  }
  if (verbose) message("[CERL] ", val$mensaje)
  texto_limpio <- if (nchar(val$texto_extraido) > 0L)
    limpiar_texto_cerl(val$texto_extraido, val$camara$codigo)
  else ""
  ext <- extraer_administradores(
    pdf_path      = pdf_path,
    texto_limpio  = texto_limpio,
    tipo_pdf      = val$tipo_pdf,
    codigo_camara = val$camara$codigo,
    api_key       = api_key,
    model         = model,
    verbose       = verbose
  )
  tiempo_seg <- round(as.numeric(difftime(Sys.time(), t_inicio, units = "secs")), 1)
  list(
    ok           = ext$ok,
    validacion   = val,
    extraccion   = ext,
    texto_limpio = texto_limpio,
    camara       = val$camara,
    mensaje      = if (ext$ok) val$mensaje
    else paste(val$mensaje, "| Sin resultados de extracción."),
    tiempo_seg   = tiempo_seg
  )
}


# 11. Construcción del df de cargue ----

# Convierte items a data.frame normalizado.
# Personas jurídicas (NIT) → razon_social; naturales → nombres/apellidos por orden de cámara.
# Roles no mapeados en ROLE_TO_ADMIN_CODE se descartan (con mensaje en log).
# Roles en ROLES_DESCARTAR se silencian completamente (sin warning al usuario).
build_admin_df <- function(items, codigo_camara = "CCB") {
  orden <- .orden_por_camara(codigo_camara)
  rows  <- lapply(items, function(it) {
    role     <- it$role     %||% ""
    doc_type <- toupper(it$doc_type %||% "")
    # Descartar roles explícitamente excluidos (ASISTENTE_ADMINISTRATIVO, etc.)
    if (role %in% ROLES_DESCARTAR) {
      message(sprintf("[DESCARTADO] Rol '%s' no corresponde a cargo de administración.", role))
      return(NULL)
    }
    admin_code <- ROLE_TO_ADMIN_CODE[role]
    doc_code   <- DOC_TYPE_TO_CODE[doc_type]
    if (is.na(admin_code)) {
      message(sprintf("[SKIP] Rol '%s' sin mapeo — fila descartada.", role))
      return(NULL)
    }
    if (is.na(doc_code)) doc_code <- NA_character_
    es_juridica <- doc_type %in% DOC_TYPES_JURIDICA
    if (es_juridica) {
      nm           <- list(pa = "", sa = "", pn = "", sn = "")
      razon_social <- str_trim(it$full_name %||% "")
    } else {
      nm           <- split_name_by_camara(it$full_name %||% "", orden = orden)
      razon_social <- NA_character_
    }
    data.frame(
      tipo_id_codigo    = as.character(doc_code),
      num_id            = as.character(it$doc_num %||% NA_character_),
      tipo_admin_codigo = as.integer(admin_code),
      tipo_admin_desc   = unname(ROLE_TO_ADMIN_DESC[as.character(admin_code)]),
      primer_apellido   = nm$pa,
      segundo_apellido  = nm$sa,
      primer_nombre     = nm$pn,
      segundo_nombre    = nm$sn,
      razon_social      = razon_social,
      porcentaje        = NA_real_,
      id_administrador  = NA_character_,
      stringsAsFactors  = FALSE,
      row.names         = NULL
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0L) { warning("Ninguna fila válida."); return(data.frame()) }
  bind_rows(rows) %>% arrange(tipo_admin_codigo)
}


# 12. Escritura sobre plantilla Excel ----

escribir_plantilla_ccb <- function(df, plantilla_path) {
  wb <- openxlsx::loadWorkbook(plantilla_path)
  df_out <- df %>%
    select(tipo_id_codigo, num_id, tipo_admin_codigo, tipo_admin_desc,
           primer_nombre, segundo_nombre, primer_apellido, segundo_apellido,
           razon_social, porcentaje, id_administrador)
  openxlsx::writeData(wb, sheet = "ADMINISTRADORES", x = df_out,
                      startRow = 3L, startCol = 1L, colNames = FALSE)
  tmp <- tempfile(fileext = ".xlsx")
  openxlsx::saveWorkbook(wb, tmp, overwrite = TRUE)
  tmp
}