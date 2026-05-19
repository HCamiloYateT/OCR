.badge_catalog <- list(
  prototipo     = list(label = "PROTOTIPO", clase = "app-badge--prototipo"),
  pruebas       = list(label = "PRUEBAS", clase = "app-badge--pruebas"),
  staging       = list(label = "STAGING", clase = "app-badge--staging"),
  produccion    = list(label = "PRODUCCION", clase = "app-badge--produccion"),
  demo          = list(label = "DEMO", clase = "app-badge--demo"),
  mantenimiento = list(label = "MANT.", clase = "app-badge--mantenimiento"),
  ninguno       = list(label = "", clase = "app-badge--ninguno")
)

app_badge <- function(estado = badge_estado) {
  env    <- tolower(trimws(badge_estado))
  config <- .badge_catalog[[env]]

  # Fallback a prototipo si el estado no esta en el catalogo
  if (is.null(config)) {
    warning(sprintf(
      "[app_badge] Estado '%s' no reconocido. Usando 'prototipo' como fallback.", env
    ))
    config <- .badge_catalog[["prototipo"]]
  }

  tags$span(class = paste("app-badge", config$clase), config$label)
}

footer <- bs4DashFooter(
  left = tags$div(
    app_badge(badge_estado)
  ),
  right = tags$img(
    src = "https://raw.githubusercontent.com/HCamiloYateT/Compartido/main/img/logo.png",
    style = "height:30px;"
  ),
  fixed = TRUE
)
