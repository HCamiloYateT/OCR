# Tipos de documento ----

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

# Tipos de doc de persona jurídica — usan razon_social, pa/sa/pn/sn vacíos
DOC_TYPES_JURIDICA <- c("NIT")

# Partículas que pueden formar parte de un apellido compuesto.
# Usadas en split_name_by_camara para extender el apellido hacia la izquierda.
# Ej: "DE LA ROSA" o "DEL CASTILLO" se reconocen como un solo apellido.
PARTICULAS_APELLIDO <- c("DE", "DEL", "DE LA", "DE LOS", "DE LAS",
                         "LA", "LOS", "LAS", "VAN", "VON", "SAN", "SANTA")


# Roles canónicos ----

ROLE_TO_ADMIN_CODE <- c(
  REPRESENTANTE_LEGAL            = 1L,
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
  BENEFICIARIO_FINAL             = 13L,
  # Alias LLM — el modelo puede devolver estos; mapeados al código correcto
  MIEMBRO_SUPLENTE_CONSEJO       = 5L   # alias → MIEMBRO_CONSEJO_ADMINISTRACION
  # NOTA: ASISTENTE_ADMINISTRATIVO se elimina deliberadamente.
  # Si el LLM devuelve ese rol, build_admin_df lo descarta con advertencia
  # porque no corresponde a ningún cargo de administración válido en la plantilla.
)

ROLE_TO_ADMIN_DESC <- c(
  "1"  = "Representante Legal",
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
  "13" = "Beneficiario Final"
)

# Roles que el LLM puede inventar pero que deben descartarse silenciosamente.
# No van a la plantilla de cargue. Se emite advertencia en los logs, no en UI.
ROLES_DESCARTAR <- c("ASISTENTE_ADMINISTRATIVO", "ASISTENTE_ADMINISTRATIVA")


# Cargos textuales -> rol canónico ----
# Orden: más específico primero (más largo), para que el matching greedy sea correcto.
# NOTA: ASISTENTE ADMINISTRATIVO/A se elimina. Si aparece en el documento
# no se registra como cargo de administración.
CARGO_TO_ROLE <- c(
  "REPRESENTANTE LEGAL PARA EFECTOS JUDICIALES Y ADMINISTRATIVOS" = "REPRESENTANTE_LEGAL",
  "REPRESENTANTE LEGAL SUPLENTE"                                  = "REPRESENTANTE_LEGAL_SUPLENTE",
  "REPRESENTANTE LEGAL"                                           = "REPRESENTANTE_LEGAL",
  "REVISOR FISCAL SUPLENTE DELEGADO DE LA FIRMA"                  = "REVISOR_FISCAL_SUPLENTE",
  "REVISOR FISCAL PRINCIPAL DELEGADO DE LA FIRMA"                 = "REVISOR_FISCAL",
  "REVISOR FISCAL SUPLENTE"                                       = "REVISOR_FISCAL_SUPLENTE",
  "REVISOR FISCAL PRINCIPAL"                                      = "REVISOR_FISCAL",
  "REVISOR FISCAL"                                                = "REVISOR_FISCAL",
  "FIRMA REVISORA"                                                = "REVISOR_FISCAL",
  "GERENTE SUPLENTE"                                              = "GERENTE_SUPLENTE",
  "SUPLENTE"                                                      = "GERENTE_SUPLENTE",
  "SUPLENTE DEL GERENTE"                                          = "GERENTE_SUPLENTE",
  "SUBGERENTE"                                                    = "GERENTE_SUPLENTE",
  "GERENTE"                                                       = "GERENTE",
  "CONTADOR"                                                      = "CONTADOR",
  "PRESIDENTE CONSEJO DE ADMINISTRACION"                          = "MIEMBRO_CONSEJO_ADMINISTRACION",
  "SECRETARIO CONSEJO DE ADMINISTRACION"                          = "MIEMBRO_CONSEJO_ADMINISTRACION",
  "DELEGADO PRINCIPAL CONSEJO DE ADMINISTRACION"                  = "MIEMBRO_CONSEJO_ADMINISTRACION",
  "DELEGADO SUPLENTE CONSEJO DE ADMINISTRACION"                   = "MIEMBRO_CONSEJO_ADMINISTRACION",
  "MIEMBRO PRINCIPAL CONSEJO DE ADMINISTRACION"                   = "MIEMBRO_CONSEJO_ADMINISTRACION",
  "MIEMBRO SUPLENTE CONSEJO DE ADMINISTRACION"                    = "MIEMBRO_CONSEJO_ADMINISTRACION",
  "MIEMBRO CONSEJO DE ADMINISTRACION"                             = "MIEMBRO_CONSEJO_ADMINISTRACION",
  "MIEMBRO PRINCIPAL CONSEJO DIRECTIVO"                           = "MIEMBRO_CONSEJO_DIRECTIVO",
  "MIEMBRO SUPLENTE CONSEJO DIRECTIVO"                            = "MIEMBRO_CONSEJO_DIRECTIVO",
  "MIEMBRO CONSEJO DIRECTIVO"                                     = "MIEMBRO_CONSEJO_DIRECTIVO",
  "MIEMBRO PRINCIPAL COMITE DE ADMINISTRACION"                    = "MIEMBRO_COMITE_ADMINISTRACION",
  "MIEMBRO SUPLENTE COMITE DE ADMINISTRACION"                     = "MIEMBRO_COMITE_ADMINISTRACION",
  "MIEMBRO COMITE DE ADMINISTRACION"                              = "MIEMBRO_COMITE_ADMINISTRACION",
  "MIEMBRO PRINCIPAL JUNTA DIRECTIVA"                             = "MIEMBRO_JUNTA_DIRECTIVA",
  "MIEMBRO SUPLENTE JUNTA DIRECTIVA"                              = "MIEMBRO_JUNTA_DIRECTIVA",
  "MIEMBRO JUNTA DIRECTIVA"                                       = "MIEMBRO_JUNTA_DIRECTIVA",
  "SOCIO ACCIONISTA"                                              = "SOCIO_ACCIONISTA",
  "BENEFICIARIO FINAL"                                            = "BENEFICIARIO_FINAL",
  "ADMINISTRADOR"                                                 = "GERENTE",
  "DIRECTOR EJECUTIVO"                                            = "GERENTE",
  "DIRECTOR GENERAL"                                              = "GERENTE"
)


# Secciones ----
SECTION_TO_RENGLON_ROLE <- c(
  JD_PRINCIPAL = "MIEMBRO_JUNTA_DIRECTIVA",
  JD_SUPLENTE  = "MIEMBRO_JUNTA_DIRECTIVA",
  CA_PRINCIPAL = "MIEMBRO_CONSEJO_ADMINISTRACION",
  CA_SUPLENTE  = "MIEMBRO_CONSEJO_ADMINISTRACION",
  CD_PRINCIPAL = "MIEMBRO_CONSEJO_DIRECTIVO",
  CD_SUPLENTE  = "MIEMBRO_CONSEJO_DIRECTIVO",
  CM_PRINCIPAL = "MIEMBRO_COMITE_ADMINISTRACION",
  CM_SUPLENTE  = "MIEMBRO_COMITE_ADMINISTRACION"
)


# Patrones de regex ----

RENGLON_PAT <- paste0(
  "^(PRIMER|SEGUNDO|TERCER|CUARTO|QUINTO|",
  "SEXTO|SEPTIMO|OCTAVO|NOVENO|DECIMO)\\s+RENGLON$"
)

# Permite sufijo opcional (ej. tarjeta profesional "142685-T")
PERSON_PAT <- paste0(
  "^([A-Z][A-Z ]+?)\\s+",
  "(C\\.C\\.|C\\.E\\.|NIT|PPT|PEP|RC\\b|TI\\b|CD\\b|PA\\b)",
  "\\.?\\s*(?:N[Oo]\\.?\\s*)?(\\d[\\d.]*)"
)


# Orden de nombres por cámara ----
ORDEN_NOMBRES_CAMARA <- c(
  CCB  = "AUTO",  
  CCH  = "NOM_AP",
  CCC  = "NOM_AP",
  CCM  = "AP_NOM",
  CCA  = "AP_NOM",
  CCBA = "AP_NOM",
  CCBU = "AP_NOM",
  CCMZ = "AP_NOM"
)


# Configuración de páginas a omitir en visión ----
SKIP_PAGS_CAMARA <- list(
  CCH     = list(inicio = 2L, fin = 2L),
  CCB     = list(inicio = 1L, fin = 2L),
  default = list(inicio = 0L, fin = 0L)
)