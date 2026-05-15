# Arquitectura

## Visión general

La aplicación está construida como una app Shiny con `bs4Dash`. El archivo `APP/global.R` prepara el ambiente, carga librerías, define parámetros globales y ejecuta una carga dinámica de módulos R dentro de `APP/misc`. La UI principal se ensambla en `APP/ui.R`, la lógica Shiny principal vive en `APP/server.R` y la funcionalidad específica de certificados CCB está encapsulada en `APP/misc/modulos/CCB.R`.

## Componentes principales

| Componente | Responsabilidad |
| --- | --- |
| `APP/global.R` | Configuración regional, opciones globales, carga de paquetes, título, rutas de plantilla y carga de módulos. |
| `APP/ui.R` | Construye la página `bs4DashPage` con header, sidebar, body y footer. |
| `APP/server.R` | Define datos reactivos de usuario/grupo y registra el módulo `CCB`. |
| `APP/misc/ui/*.R` | Componentes visuales reutilizables de layout. |
| `APP/misc/modulos/CCB.R` | Módulo Shiny para carga del PDF, validación, visualización y descarga. |
| `APP/misc/FuncionesCCB.R` | Núcleo de negocio: validación PDF, extracción por regex, fallback LLM, normalización y escritura Excel. |
| `APP/misc/parametros.R` | Diccionarios y patrones que controlan documentos, roles y cargos reconocidos. |

## Flujo de carga

1. Shiny ejecuta `global.R` antes de `ui.R` y `server.R`.
2. `global.R` configura locales, opciones, librerías y variables globales.
3. `load_modules()` busca archivos `.R` bajo `APP/misc` y los carga por profundidad ascendente.
4. `ui.R` arma el layout usando objetos creados en `APP/misc/ui`.
5. `server.R` registra `CCB("ccb")`.
6. El módulo `CCB` expone UI y server para operar el certificado.

## Pipeline de extracción

```text
PDF cargado
   │
   ▼
validar_pdf_ccb()
   │  valida PDF, texto mínimo y sección NOMBRAMIENTOS
   ▼
extraer_administradores_ccb()
   │
   ├─ .extraer_por_regex()
   │    ├─ extract_pdf_text()
   │    ├─ extract_nombramientos_block()
   │    ├─ parse_block_contextual()
   │    └─ build_admin_df()
   │
   └─ .extraer_por_llm()  [solo si regex falla]
        ├─ .llamar_openai()
        ├─ .parse_openai_items()
        └─ build_admin_df()
```

## Modelo de datos de salida

El dataframe final contiene columnas pensadas para la plantilla institucional:

- `tipo_id_codigo`
- `num_id`
- `tipo_admin_codigo`
- `tipo_admin_desc`
- `primer_apellido`
- `segundo_apellido`
- `primer_nombre`
- `segundo_nombre`
- `razon_social`
- `porcentaje`
- `id_administrador`

Antes de escribir el Excel, `escribir_plantilla_ccb()` reordena columnas para la hoja `ADMINISTRADORES`.

## Fallback OpenAI

El fallback se usa únicamente cuando la extracción por regex no produce filas válidas. El cliente HTTP usa `httr2`, envía un prompt con los roles permitidos y espera una respuesta JSON parseable. La llave se obtiene desde `OPENAI_API_KEY`.

## Puntos de extensión

- Nuevos tipos de documento: actualizar `DOC_TYPE_TO_CODE` en `APP/misc/parametros.R`.
- Nuevos roles: actualizar `ROLE_TO_ADMIN_CODE`, `ROLE_TO_ADMIN_DESC` y, si aplica, `CARGO_TO_ROLE`.
- Nuevas secciones de certificados: ajustar `detect_section()` en `APP/misc/FuncionesCCB.R` y `SECTION_TO_RENGLON_ROLE`.
- Cambios visuales: editar componentes en `APP/misc/ui` o el módulo `APP/misc/modulos/CCB.R`.
