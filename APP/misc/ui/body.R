body <- bs4DashBody(
  includeCSS("https://raw.githubusercontent.com/HCamiloYateT/Compartido/refs/heads/main/Styles/style.css"),
  use_waiter(),
  useShinyjs(),
  bs4TabItems(
    tabItem(tabName = "camaras", CamarasUI("mod_camaras")),
    tabItem(tabName = "rut", RutUI("mod_rut"))
    )
  )
