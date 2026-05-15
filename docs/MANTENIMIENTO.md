# Mantenimiento

## Dónde hacer cambios frecuentes

| Necesidad | Archivo sugerido |
| --- | --- |
| Agregar tipos de documento | `APP/misc/parametros.R` |
| Agregar o corregir cargos reconocidos | `APP/misc/parametros.R` |
| Ajustar detección de secciones del certificado | `APP/misc/FuncionesCCB.R` |
| Cambiar validaciones del PDF | `APP/misc/FuncionesCCB.R` |
| Cambiar textos, tarjetas o tabla | `APP/misc/modulos/CCB.R` |
| Cambiar menú, header, footer o preloader | `APP/misc/ui/*.R` |
| Cambiar título o estado de la aplicación | `APP/global.R` |

## Añadir un nuevo cargo

1. Identifique cómo aparece el cargo en el certificado ya normalizado: mayúsculas, sin tildes y con espacios simples.
2. Agregue el cargo a `CARGO_TO_ROLE` en `APP/misc/parametros.R`.
3. Si el rol canónico no existe, agréguelo también a:
   - `ROLE_TO_ADMIN_CODE`
   - `ROLE_TO_ADMIN_DESC`
4. Pruebe con un PDF real que contenga el cargo.

## Añadir un nuevo tipo de documento

1. Agregue la abreviatura al patrón `PERSON_PAT` si aún no se reconoce.
2. Agregue su código de plantilla a `DOC_TYPE_TO_CODE`.
3. Ejecute una prueba con una línea de persona que use ese tipo documental.

## Ajustar extracción por regex

La extracción por regex mantiene estado de sección y cargo activo. El orden esperado es:

1. Detectar sección (`detect_section()`).
2. Detectar cargo (`detect_cargo_role()`).
3. Detectar persona (`parse_person_line()`).
4. Construir filas (`build_admin_df()`).

Al modificar esta lógica, cuide que los cambios no rompan certificados que ya funcionan. Lo ideal es conservar PDFs de ejemplo anonimizados o textos de prueba en un conjunto de regresión.

## Fallback LLM

El fallback LLM debe tratarse como respaldo, no como camino principal. Si un formato aparece con frecuencia y el LLM lo resuelve, conviene convertir ese aprendizaje en reglas determinísticas dentro de `parametros.R` o `FuncionesCCB.R`.

## Validaciones recomendadas antes de publicar

Ejecute, como mínimo:

```bash
Rscript -e 'parse(file = "APP/global.R"); parse(file = "APP/server.R"); parse(file = "APP/ui.R"); invisible(lapply(list.files("APP/misc", pattern = "\\.R$", recursive = TRUE, full.names = TRUE), parse))'
```

Si el ambiente tiene dependencias instaladas, ejecute también:

```bash
Rscript -e 'source("APP/global.R")'
```

## Convenciones

- Mantenga los textos de usuario en español.
- Prefiera reglas determinísticas antes de aumentar dependencia del LLM.
- Documente en este archivo cualquier supuesto nuevo del formato CCB.
- No incluya certificados reales con datos sensibles en el repositorio.
