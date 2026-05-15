# Configuración y despliegue

## Prerrequisitos

1. R instalado en el servidor o estación de trabajo.
2. Acceso a los paquetes internos `racafe*` usados por la aplicación.
3. Paquetes CRAN definidos en `APP/global.R`.
4. Plantilla Excel disponible en `APP/data/Templates/PlantillaCCB.xlsx`.
5. Opcionalmente, variable `OPENAI_API_KEY` para fallback LLM.

## Instalación de dependencias

La aplicación usa `racafeCore::Loadpkg()` para cargar dependencias CRAN e internas. En ambientes nuevos, valide primero que los paquetes internos estén instalados y disponibles en `.libPaths()`.

Una comprobación básica desde la raíz del repositorio:

```bash
Rscript -e 'source("APP/global.R")'
```

Si el comando falla por ausencia de paquetes internos, instálelos desde el repositorio corporativo correspondiente antes de desplegar.

## Variables de entorno

### `OPENAI_API_KEY`

Solo es necesaria para el fallback LLM. Ejemplo temporal en Linux/macOS:

```bash
export OPENAI_API_KEY="sk-..."
```

Para despliegues permanentes, configure la variable en el servicio que ejecute Shiny, Posit Connect, Shiny Server o el mecanismo de orquestación usado por el equipo.

### Locales y zona horaria

`APP/global.R` configura:

- `TZ = "America/Bogota"`
- `LANG = "es_CO.UTF-8"`
- Intentos de locale para tiempo, moneda y mensajes en español.

En Linux, instale el locale `es_CO.UTF-8` si se requieren nombres de fechas y mensajes localizados de forma consistente.

## Archivos de datos

Cree la estructura de datos esperada si no existe:

```text
APP/data/
└── Templates/
    └── PlantillaCCB.xlsx
```

`APP/data/data.RData` es opcional. Si existe, `global.R` lo carga al ambiente global; si no existe, la aplicación inicia sin datos precargados.

## Ejecución

### Desarrollo local

```bash
Rscript -e 'shiny::runApp("APP", host = "127.0.0.1", port = 3838)'
```

### Servidor

```bash
Rscript -e 'shiny::runApp("APP", host = "0.0.0.0", port = 3838)'
```

## Checklist de despliegue

- [ ] Los paquetes `racafe*` están instalados.
- [ ] Las dependencias CRAN cargan correctamente.
- [ ] Existe `APP/data/Templates/PlantillaCCB.xlsx`.
- [ ] La hoja `ADMINISTRADORES` existe en la plantilla.
- [ ] La aplicación puede leer PDFs con `pdftools`.
- [ ] `OPENAI_API_KEY` está configurada si se requiere fallback LLM.
- [ ] El servidor puede acceder a los recursos visuales remotos usados por header, footer y CSS.
