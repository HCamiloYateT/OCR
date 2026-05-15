# Guía de uso

## Objetivo

Esta guía describe cómo usar la aplicación para extraer administradores de un certificado de existencia y representación legal de la Cámara de Comercio de Bogotá.

## Preparar el archivo

Use un certificado CCB en PDF que cumpla estas condiciones:

- Es un PDF válido y no protegido.
- Contiene texto seleccionable o extraíble.
- Incluye la sección `NOMBRAMIENTOS`.
- Corresponde al certificado completo, no a una captura parcial.

Si el documento es una imagen escaneada sin texto embebido, la aplicación lo rechazará por bajo volumen de caracteres extraíbles.

## Pasos en la interfaz

1. Abra la aplicación Shiny.
2. Ingrese a la opción **Cámara de Comercio** del menú lateral.
3. En la tarjeta **Cargar certificado**, seleccione el PDF.
4. Revise la tarjeta **Estado de validación**:
   - Si el documento no es válido, lea el mensaje y cargue un PDF correcto.
   - Si el documento se procesa correctamente, verá el número de administradores identificados.
5. Revise la tabla **Administradores extraídos**.
6. Use el botón **Descargar** para obtener el archivo Excel diligenciado.

## Interpretación de estados

| Estado | Significado | Acción recomendada |
| --- | --- | --- |
| Esperando archivo | Aún no se ha cargado un PDF. | Seleccione un certificado. |
| Documento no válido | El PDF no se puede leer, tiene poco texto o no contiene `NOMBRAMIENTOS`. | Verifique el archivo fuente. |
| Error en extracción | El PDF pasó validación, pero no se pudieron extraer administradores. | Revise formato del certificado o configure fallback LLM. |
| Documento procesado | La extracción fue exitosa. | Revise tabla y descargue Excel. |

## Campos generados

La tabla de salida muestra:

- Tipo y número de identificación.
- Código y descripción del rol administrativo.
- Primer y segundo apellido.
- Primer y segundo nombre.

## Recomendaciones de calidad

- Compare una muestra de filas contra el certificado original antes de usar el Excel en procesos posteriores.
- Preste atención a nombres compuestos: la separación de apellidos/nombres es heurística.
- Si faltan cargos recurrentes, solicite ajustar los mapeos de `APP/misc/parametros.R`.
- Si un certificado tiene formato atípico, conserve el PDF para pruebas de mantenimiento.
