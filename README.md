# Lector OCR CCB

Aplicación Shiny para cargar certificados de existencia y representación legal de la Cámara de Comercio de Bogotá (CCB), validar que el PDF tenga texto extraíble y extraer los administradores encontrados en la sección **NOMBRAMIENTOS**. El resultado se muestra en una tabla y se descarga como archivo Excel usando una plantilla institucional.

## ¿Qué resuelve?

El repositorio automatiza una tarea operativa frecuente: leer certificados CCB en PDF y transformar los nombramientos de administradores, representantes legales, revisores fiscales, socios y cargos relacionados en una estructura tabular compatible con una plantilla de cargue.

> Nota: aunque el proyecto se llama “OCR”, el flujo actual depende de texto extraíble con `pdftools`. Los PDFs puramente escaneados se detectan como documentos con poco texto y no se procesan automáticamente.

## Funcionalidades principales

- Carga de un único PDF desde la interfaz Shiny.
- Validación del archivo: formato PDF, número de páginas, volumen mínimo de texto y presencia de la sección `NOMBRAMIENTOS`.
- Extracción primaria mediante reglas y expresiones regulares.
- Fallback opcional con OpenAI cuando la extracción por reglas no encuentra administradores.
- Normalización de nombres, documentos y roles a códigos de la plantilla de cargue.
- Visualización de administradores extraídos en una tabla interactiva.
- Descarga de un `.xlsx` diligenciado sobre la plantilla configurada.

## Estructura del repositorio

```text
APP/
├── global.R                 # Configuración global, librerías, variables y carga de módulos
├── server.R                 # Lógica server principal de Shiny
├── ui.R                     # Ensamble general de la UI bs4Dash
└── misc/
    ├── FuncionesCCB.R       # Validación, extracción, fallback LLM y escritura Excel
    ├── parametros.R         # Diccionarios de documentos, roles y patrones regex
    ├── values.R             # Valores compartidos de presentación
    ├── factores.R           # Reservado para factores/catálogos
    ├── functions.R          # Reservado para funciones generales
    ├── modulos/CCB.R        # Módulo Shiny de carga, validación, tabla y descarga
    └── ui/                  # Componentes visuales: header, sidebar, body, footer, preloader
```

## Requisitos

### Runtime

- R reciente compatible con Shiny.
- Paquetes internos de RACAFE:
  - `racafeCore`
  - `racafeBD`
  - `racafeDrive`
  - `racafeGraph`
  - `racafeShiny`
  - `racafeModulos`
- Paquetes CRAN cargados desde `global.R`, entre ellos:
  - `shiny`, `bs4Dash`, `shinyBS`, `shinyjs`, `DT`, `shinycssloaders`
  - `shinyWidgets`, `tidyverse`, `gt`, `scales`, `plotly`, `rlang`
  - `waiter`, `glue`, `lubridate`, `stringr`, `purrr`, `pdftools`
  - `httr2`, `jsonlite`

### Archivos esperados en despliegue

La aplicación referencia estos recursos externos o artefactos de datos:

- `APP/data/Templates/PlantillaCCB.xlsx`: plantilla Excel usada para generar la descarga.
- `APP/data/data.RData` (opcional): datos precargados si existen.
- Recursos gráficos/CSS alojados en repositorios compartidos de RACAFE.

## Configuración

### Variables de entorno

| Variable | Obligatoria | Uso |
| --- | --- | --- |
| `OPENAI_API_KEY` | No | Activa el fallback LLM cuando la extracción por regex falla. |
| `TZ` | No | Se fija en `America/Bogota` desde `global.R`. |
| `LANG` | No | Se fija en `es_CO.UTF-8` desde `global.R`. |

Si `OPENAI_API_KEY` no está definida, la aplicación sigue funcionando con el motor regex; el fallback LLM devolverá un mensaje de error controlado si llega a necesitarse.

### Parámetros clave

Los mapeos de tipos de documento, roles de administradores, secciones y patrones de persona viven en `APP/misc/parametros.R`. Para ajustar nuevas variantes de certificados, este suele ser el primer archivo a revisar.

## Ejecución local

Desde la raíz del repositorio:

```r
shiny::runApp("APP")
```

También puede ejecutarse desde una terminal:

```bash
Rscript -e 'shiny::runApp("APP", host = "0.0.0.0", port = 3838)'
```

## Flujo funcional

1. El usuario carga un certificado CCB en PDF.
2. `validar_pdf_ccb()` verifica que el archivo sea legible y contenga `NOMBRAMIENTOS`.
3. `extraer_administradores_ccb()` intenta extraer datos por regex.
4. Si regex falla y existe configuración OpenAI, se llama al fallback LLM.
5. `build_admin_df()` normaliza los campos al esquema de administradores.
6. El módulo `CCB` muestra la tabla y habilita la descarga.
7. `escribir_plantilla_ccb()` escribe los datos en la hoja `ADMINISTRADORES` de la plantilla Excel.

## Limitaciones conocidas

- No realiza OCR visual sobre imágenes; requiere texto embebido en el PDF.
- Los nombres se separan con una heurística simple basada en el orden típico `APELLIDOS NOMBRES`.
- El fallback LLM depende de conectividad, cuota y una llave válida de OpenAI.
- La escritura Excel depende de que la plantilla exista y tenga la hoja esperada.
- Cambios en el formato de certificados CCB pueden requerir actualizar patrones en `parametros.R` o reglas en `FuncionesCCB.R`.

## Documentación adicional

- [Arquitectura](docs/ARQUITECTURA.md)
- [Configuración y despliegue](docs/CONFIGURACION.md)
- [Guía de uso](docs/USO.md)
- [Mantenimiento](docs/MANTENIMIENTO.md)
