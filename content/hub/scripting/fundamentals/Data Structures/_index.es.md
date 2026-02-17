---
title: "Estructuras de datos"
type: docs
weight: 3
---
En Scheme, las **estructuras de datos** son herramientas esenciales para organizar, almacenar y manipular datos. Permiten a los desarrolladores crear scripts eficientes, legibles y reutilizables. Al elegir la estructura de datos adecuada para un problema específico, puede optimizar tanto el rendimiento como la claridad de su código.

## Estructuras de datos clave en el esquema

Scheme proporciona varias estructuras de datos potentes y versátiles, cada una adecuada para tareas específicas. Las estructuras de datos primarias incluyen:

### Listas
Las listas son colecciones ordenadas de elementos que pueden crecer o reducirse dinámicamente. Son ideales para datos secuenciales o jerárquicos y se utilizan ampliamente en programación funcional.

Características clave:
- Dimensionado dinámicamente.
- Los elementos pueden ser de tipos mixtos.
- Comúnmente utilizado para algoritmos recursivos y para representar estructuras en forma de árbol.

Ejemplos de uso:
- Gestión de colecciones de artículos.
- Representar secuencias o jerarquías.

---

### Vectores
Los vectores son colecciones de elementos de tamaño fijo, indexados para un acceso rápido. Son más adecuados para escenarios donde el rendimiento y el acceso posicional son críticos.

Características clave:
- Tamaño fijo en la creación.
- Se accede a los elementos por su índice.
- Más rápido que las listas para determinadas operaciones como el acceso aleatorio.

Ejemplos de uso:
- Almacenamiento de configuraciones o datos de tamaño fijo.
- Búsquedas rápidas y actualizaciones según la posición.

---

### Elegir la estructura de datos adecuada

La decisión de utilizar una **lista** o un **vector** depende de las necesidades específicas de su script. Aquí hay algunas pautas:

| Característica | Listas | Vectores |
|---------------------------------|------------------------------|--------------------------------|
| **Flexibilidad de tamaño** | Dinámico | Fijo |
| **Velocidad de acceso** | Más lento (acceso secuencial) | Más rápido (acceso indexado) |
| **Facilidad de modificación**| Más fácil | Más difícil (requiere reasignación)|
| **Casos de uso** | Datos dinámicos, recursividad | Datos estáticos, búsquedas rápidas |

---