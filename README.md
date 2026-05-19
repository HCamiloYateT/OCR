# Lector OCR

Aplicación **Shiny** para lectura y procesamiento OCR, construida sobre componentes del ecosistema `racafe*` y `bs4Dash`.

## Estructura del proyecto

- `APP/global.R`: configuración global (locales, opciones, librerías, carga de datos y módulos).
- `APP/ui.R`: composición de la interfaz principal (`bs4DashPage`).
- `APP/server.R`: lógica de servidor y reactividad.
- `APP/misc/`: módulos, funciones auxiliares y componentes UI.

## Requisitos

Antes de ejecutar, asegúrate de tener:

- R (versión reciente compatible con Shiny)
- Paquetes internos: `racafeCore`, `racafeBD`, `racafeDrive`, `racafeGraph`, `racafeShiny`, `racafeModulos`
- Dependencias CRAN usadas por la app (por ejemplo: `shiny`, `bs4Dash`, `tidyverse`, `plotly`, `lubridate`, `openxlsx`, `reactable`)

## Ejecución local

Desde la raíz del repositorio:

```r
setwd("APP")
shiny::runApp()
```

## Datos esperados

- Plantilla: `APP/data/Templates/PlantillaCCB.xlsx`
- Datos precargados opcionales: `APP/data/data.RData`

Si `data.RData` no está presente, la aplicación inicia sin datos precargados.

## Notas de configuración

- La app fija zona horaria en `America/Bogota`.
- Se intentan locales en español para fechas, moneda y mensajes del sistema.
- La carga de scripts en `APP/misc` se hace en múltiples pasadas para resolver dependencias cruzadas.
