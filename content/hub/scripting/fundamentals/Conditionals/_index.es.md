---
title: "Condicionales"
type: docs
weight: 2
---
Los condicionales son una parte fundamental de la programación, ya que permiten a los scripts tomar decisiones y controlar su flujo en función de criterios específicos. En Scheme, que se basa en el lenguaje de programación Scheme, los condicionales le permiten crear scripts dinámicos e inteligentes que se adaptan a entradas, entornos o acciones del usuario cambiantes.

### El papel de los condicionales en el esquema

Los condicionales cumplen varios propósitos clave en sus scripts:
- **Lógica de dirección:** Le permiten ejecutar diferentes fragmentos de código dependiendo de si ciertas condiciones son verdaderas o falsas.
- **Mejora de la flexibilidad:** Al responder dinámicamente a entradas o estados, los condicionales ayudan a su secuencia de comandos a manejar una variedad de escenarios.
- **Simplificación de la complejidad:** Dividen la toma de decisiones en estructuras manejables, lo que hace que el código sea más fácil de leer, depurar y mantener.

### Tipos de condicionales disponibles

Scheme proporciona varias construcciones condicionales, cada una adecuada a diferentes necesidades lógicas:
- **`if`:** Para tomar decisiones binarias simples, ejecutar un bloque de código si una condición es verdadera y otro si es falsa.
- **`cond`:** Una poderosa construcción de múltiples ramificaciones para manejar múltiples condiciones de una manera clara y estructurada.
- **`and` / `or`:** Operadores lógicos que evalúan combinaciones de condiciones, permitiendo una toma de decisiones más compleja.
- **`else`:** Un comodín que define el comportamiento de respaldo cuando no se cumple ninguna de las condiciones especificadas.

### Cómo funcionan los condicionales

Los condicionales suelen implicar:
1. **Evaluación de una condición:** Una expresión de prueba determina si una condición es verdadera o falsa.
2. **Ejecución de ramificación:** Según la evaluación, el script selecciona qué bloque de código ejecutar.
3. **Devolver un valor (opcional):** En algunos casos, los condicionales también pueden producir un valor que otras partes del script pueden usar.