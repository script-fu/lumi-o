---
title: "Condicionales"
type: docs
weight: 2
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 8e9a64dd1bc1445c996fe17ce6b666b8d597ab16040cf0ef0876232026ff11b2
url: "hub/scripting/fundamentals/Conditionals"
---
Los condicionales son un elemento fundamental de la programación: permiten que los scripts tomen decisiones y controlen su flujo según criterios concretos. En Scheme, basado en el lenguaje de programación Scheme, los condicionales le ayudan a crear scripts dinámicos e inteligentes que se adaptan a entradas, entornos o acciones del usuario que cambian.

### El papel de los condicionales en Scheme

Los condicionales cumplen varias funciones clave en sus scripts:
- **Dirigir la lógica:** Ejecutan distintos fragmentos de código según si ciertas condiciones son verdaderas o falsas.
- **Mayor flexibilidad:** Al responder dinámicamente a entradas o estados, ayudan a manejar una variedad de escenarios.
- **Simplificar la complejidad:** Descomponen la toma de decisiones en estructuras manejables, facilitando la lectura, depuración y mantenimiento del código.

### Tipos de condicionales disponibles

Scheme ofrece varios constructos condicionales, cada uno adaptado a distintas necesidades lógicas:
- **`if`:** Para decisiones binarias simples: un bloque si la condición es verdadera y otro si es falsa.
- **`cond`:** Un potente constructo de ramificación múltiple para manejar varias condiciones de forma clara y estructurada.
- **`and` / `or`:** Operadores lógicos que evalúan combinaciones de condiciones y permiten decisiones más complejas.
- **`else`:** Un caso comodín que define el comportamiento de respaldo cuando ninguna condición especificada se cumple.

### Cómo funcionan los condicionales

Los condicionales suelen implicar:
1. **Evaluar una condición:** Una expresión de prueba determina si una condición es verdadera o falsa.
2. **Ejecución ramificada:** Según la evaluación, el script elige qué bloque de código ejecutar.
3. **Devolver un valor (opcional):** En algunos casos, los condicionales también producen un valor que otras partes del script pueden usar.