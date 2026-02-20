---
title: "Desarrollo asistido por IA"
type: docs
---
Las herramientas modernas de inteligencia artificial pueden acelerar significativamente el desarrollo de complementos Lumi al actuar como un socio de codificación colaborativo.

## Código VS en modo agente

El uso de Visual Studio Code con un asistente de IA en **modo Agente** (como el modo Agente de GitHub Copilot u otros asistentes habilitados para herramientas) le permite realizar tareas complejas de varios pasos utilizando lenguaje natural.

En lugar de simplemente completar una sola línea de código, un Agente puede:
- Lea todo su espacio de trabajo para comprender el contexto.
- Crear nuevos archivos y directorios.
- Ejecutar comandos de terminal para probar o validar scripts.
- Busque patrones existentes en su código base.

## Acceso al repositorio

La asistencia de IA es más efectiva cuando el Agente tiene acceso a **lumi-dev** o al repositorio de su proyecto específico. Con visibilidad del código base existente, el Agente puede:
- Utilice **[Utility Libraries](@@LUMI_TOKEN_4@@)** como referencia para las funciones auxiliares.
- Seguir patrones existentes para operaciones GEGL y gestión de capas.
- Reutilizar código repetitivo de complementos establecidos.

## Ejemplo de flujo de trabajo

Puede pedirle directamente al Agente que genere un complemento completo describiendo el resultado funcional deseado:

> "Usando las utilidades y ejemplos de Scheme disponibles en el espacio de trabajo, escriba un nuevo complemento que cree una guía horizontal del 50% en la imagen activa y la llame 'Guía central'".

El Agente buscará cómo crear guías, identificará la función de utilidad correcta (como `lumi-image-add-hguide-percent` de `common.scm`) y generará el archivo `.scm` completo con el modelo de registro correcto.

## Mejores prácticas

- **Sea específico**: describa exactamente lo que desea que haga el complemento.
- **Utilidades de referencia**: anime al agente a buscar ayudas de alto nivel en el directorio `share/lumi/scripts/`.
- **Revisar y probar**: pruebe siempre el complemento generado por la IA; suele ser un proceso iterativo y creativo.