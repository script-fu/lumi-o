---
title: "Desarrollo asistido por IA"
type: docs
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 7867639dd5e951131133f23635a10898d35de3c275f48b78b7ed7091c73e15c4
url: "hub/scripting/tools/ai-assisted"
---
Las herramientas de IA modernas pueden acelerar considerablemente el desarrollo de complementos para Lumi actuando como un compañero de codificación colaborativo.

## VS Code en modo Agent

Usar Visual Studio Code con un asistente de IA en **modo Agent** (como el modo Agent de GitHub Copilot u otros asistentes con herramientas) le permite realizar tareas complejas de varios pasos en lenguaje natural.

En lugar de completar solo una línea de código, un agente puede:
- leer todo el espacio de trabajo para entender el contexto
- crear archivos y directorios nuevos
- ejecutar comandos de terminal para probar o validar scripts
- buscar patrones existentes en la base de código

## Acceso al repositorio

La asistencia de IA es más eficaz cuando el agente tiene acceso a **lumi-dev** o al repositorio de su proyecto. Con visibilidad del código existente, el agente puede:
- usar las **[bibliotecas de utilidades]({{< ref "/hub/scripting/reference/utility-browser" >}})** como referencia de funciones auxiliares
- seguir patrones existentes para operaciones GEGL y gestión de capas
- reutilizar código repetitivo de complementos establecidos

## Flujo de trabajo de ejemplo

Puede pedir directamente al agente que genere un complemento completo describiendo el resultado funcional deseado:

> "Usando las utilidades Scheme y los ejemplos disponibles en el espacio de trabajo, escribe un nuevo complemento que cree una guía horizontal al 50 % en la imagen activa y la llame 'Center Guide'."

El agente buscará cómo crear guías, identificará la función de utilidad correcta (como `lumi-image-add-hguide-percent` de `common.scm`) y generará el archivo `.scm` completo con el boilerplate de registro correcto.

## Buenas prácticas

- **Sea específico**: describa exactamente lo que debe hacer el complemento.
- **Referencie utilidades**: anime al agente a consultar el directorio `share/lumi/scripts/` para encontrar ayudantes de alto nivel.
- **Revise y pruebe**: pruebe siempre el complemento generado por la IA; a menudo es un proceso iterativo y creativo.
