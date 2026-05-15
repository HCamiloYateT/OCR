# Parámetros.R
# Abreviatura de tipo de documento -> código en la plantilla de cargue
DOC_TYPE_TO_CODE <- c(CC  = "C",  
                      CE  = "E",  
                      NIT = "N",  
                      PA  = "P",
                      PPT = "H",  
                      PEP = "J",  
                      RC  = "R",  
                      TI  = "T",
                      CD  = "D",  
                      INT = "I",  
                      PL  = "K")

# Rol canónico -> código numérico de la plantilla de cargue
ROLE_TO_ADMIN_CODE <- c(REPRESENTANTE_LEGAL            = 1L,
                        REPRESENTANTE_LEGAL_SUPLENTE   = 2L,
                        GERENTE                        = 3L,
                        GERENTE_SUPLENTE               = 4L,
                        MIEMBRO_CONSEJO_ADMINISTRACION = 5L,
                        MIEMBRO_CONSEJO_DIRECTIVO      = 6L,
                        MIEMBRO_COMITE_ADMINISTRACION  = 7L,
                        MIEMBRO_JUNTA_DIRECTIVA        = 8L,
                        REVISOR_FISCAL                 = 9L,
                        REVISOR_FISCAL_SUPLENTE        = 10L,
                        SOCIO_ACCIONISTA               = 11L,
                        CONTADOR                       = 12L,
                        BENEFICIARIO_FINAL             = 13L)

# Código numérico (string) -> descripción legible para la plantilla
ROLE_TO_ADMIN_DESC <- c("1"  = "Representante Legal",
                        "2"  = "Representante Legal Suplente",
                        "3"  = "Gerente",
                        "4"  = "Gerente Suplente",
                        "5"  = "Miembro Consejo de Administracion",
                        "6"  = "Miembro Consejo Directivo",
                        "7"  = "Miembro Comite de Administracion",
                        "8"  = "Miembro Junta Directiva",
                        "9"  = "Revisor Fiscal",
                        "10" = "Revisor Fiscal Suplente",
                        "11" = "Socio Accionista",
                        "12" = "Contador",
                        "13" = "Beneficiario Final")

# Etiqueta de cargo en el CCB -> rol canónico (orden: más específico primero)
CARGO_TO_ROLE <- c("REPRESENTANTE LEGAL PARA EFECTOS JUDICIALES Y ADMINISTRATIVOS" = "REPRESENTANTE_LEGAL",
                   "REPRESENTANTE LEGAL SUPLENTE" = "REPRESENTANTE_LEGAL_SUPLENTE",
                   "REPRESENTANTE LEGAL" = "REPRESENTANTE_LEGAL",
                   "REVISOR FISCAL SUPLENTE" = "REVISOR_FISCAL_SUPLENTE",
                   "REVISOR FISCAL PRINCIPAL" = "REVISOR_FISCAL",
                   "REVISOR FISCAL" = "REVISOR_FISCAL",
                   "SUPLENTE DEL GERENTE" = "GERENTE_SUPLENTE",
                   "SUBGERENTE" = "GERENTE_SUPLENTE",
                   "GERENTE" = "GERENTE",
                   "CONTADOR" = "CONTADOR",
                   "MIEMBRO CONSEJO DE ADMINISTRACION" = "MIEMBRO_CONSEJO_ADMINISTRACION",
                   "MIEMBRO CONSEJO DIRECTIVO" = "MIEMBRO_CONSEJO_DIRECTIVO",
                   "MIEMBRO COMITE DE ADMINISTRACION"  = "MIEMBRO_COMITE_ADMINISTRACION",
                   "MIEMBRO JUNTA DIRECTIVA" = "MIEMBRO_JUNTA_DIRECTIVA",
                   "SOCIO ACCIONISTA" = "SOCIO_ACCIONISTA",
                   "BENEFICIARIO FINAL" = "BENEFICIARIO_FINAL")

# Sección del documento -> rol para renglones de órgano colegiado
SECTION_TO_RENGLON_ROLE <- c(JD_PRINCIPAL = "MIEMBRO_JUNTA_DIRECTIVA",
                             JD_SUPLENTE  = "MIEMBRO_JUNTA_DIRECTIVA",
                             CA_PRINCIPAL = "MIEMBRO_CONSEJO_ADMINISTRACION",
                             CA_SUPLENTE  = "MIEMBRO_CONSEJO_ADMINISTRACION",
                             CD_PRINCIPAL = "MIEMBRO_CONSEJO_DIRECTIVO",
                             CD_SUPLENTE  = "MIEMBRO_CONSEJO_DIRECTIVO",
                             CM_PRINCIPAL = "MIEMBRO_COMITE_ADMINISTRACION",
                             CM_SUPLENTE  = "MIEMBRO_COMITE_ADMINISTRACION")

# Patrón de renglón de órgano colegiado (PRIMER RENGLON, SEGUNDO RENGLON, ...)
RENGLON_PAT <- paste0("^(PRIMER|SEGUNDO|TERCER|CUARTO|QUINTO|",
                      "SEXTO|SEPTIMO|OCTAVO|NOVENO|DECIMO)\\s+RENGLON$")

# Patrón de línea de persona: APELLIDOS NOMBRES  C.C. 000000XXXXXXXXX
PERSON_PAT <- paste0("^([A-Z][A-Z ]+?)\\s+",
                     "(C\\.C\\.|C\\.E\\.|NIT|PPT|PEP|RC\\b|TI\\b|CD\\b|PA\\b)",
                     "\\.?\\s*(?:NO\\.?\\s*)?(\\d[\\d.]*)")