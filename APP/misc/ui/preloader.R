.preloader_html <- function(texto, spinner_class) {
  tagList(
    tags$div(class = "preloader-card",
             tags$img(src   = "https://raw.githubusercontent.com/HCamiloYateT/Compartido/refs/heads/main/img/logo2.png",
                      class = "preloader-logo"),
             tags$h3(texto, class = "preloader-texto"),
             tags$div(class = paste(spinner_class, "preloader-spinner"),
                      role  = "status")
             )
    )
}

preloader_inicio <- list(
  html  = .preloader_html(texto = "Cargando ...", spinner_class = "spinner-border preloader-spinner-inicio"),
  color = "#e5e7e9"
  )

preloader_actualizar <- list(
  html  = .preloader_html(texto = "Actualizando ...", spinner_class = "spinner-grow preloader-spinner-actualizar"),
  color = "transparent"
  )

preloader_calculando <- list(
  html  = .preloader_html(texto = "Calculando ...", spinner_class = "spinner-border preloader-spinner-calculando"),
  color = "transparent"
  )
