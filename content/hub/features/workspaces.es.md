---
title: "Espacios de trabajo"
type: docs
---
Un espacio de trabajo es una instantánea guardada de todo su entorno de interfaz de usuario: qué paneles están abiertos y dónde, las decoraciones del lienzo y el relleno para la vista Normal y de Pantalla completa, el tema activo y el conjunto de íconos, el diseño de la caja de herramientas, la paleta activa y la configuración de sus herramientas. Lumi te permite guardar tantos espacios de trabajo con nombre como quieras y cambiar entre ellos al instante: todas las imágenes abiertas se actualizan en su lugar, no es necesario reiniciar.

## Lo que ahorra un espacio de trabajo

Cada espacio de trabajo con nombre almacena lo siguiente de forma independiente:

| Componente | Qué cubre |
| :--- | :--- |
| **Diseño** | Posición y tamaño de la ventana, disposición del muelle (columnas del panel izquierdo y derecho, qué paneles están abiertos y en qué orden), modo de ventana única o múltiple, estado maximizado, visibilidad y posición de la barra de pestañas |
| **Opciones de herramientas** | La configuración actual para cada herramienta (tamaño del pincel, dureza, comportamiento de deformación, etc.) |
| **Dispositivos de entrada** | Configuración del dispositivo de entrada: curvas de presión, asignaciones de botones, asignaciones de ejes para lápiz óptico y otros dispositivos |
| **Decoraciones de lienzo** | Valores predeterminados por espacio de trabajo para reglas, barras de desplazamiento, guías, cuadrícula, resaltado de selección, límite de capa y límite del lienzo: establecidos a través de **Preferencias → Ventanas de imagen → Apariencia predeterminada** y **Apariencia de pantalla completa**, de forma independiente para la vista Normal y Pantalla completa |
| **Acolchado de lona** | Modo de relleno por espacio de trabajo y color para la vista Normal y Pantalla completa: se configura mediante **Preferencias → Ventanas de imagen → Apariencia predeterminada** |
| **Tema e íconos** | Tema activo, variante de color oscuro/claro, conjunto de iconos, anulación del tamaño de los iconos y escala de fuente |
| **Caja de herramientas** | Posición del widget FG/BG (arriba/abajo/izquierda/derecha), escala FG/BG, visibilidad de la mascota Wilber, encabezados de grupos de herramientas |

La **paleta** activa y la **herramienta preestablecida** también se registran por espacio de trabajo y se restauran cuando cambia.

> **Las decoraciones y el relleno del lienzo** están controlados por
> **Preferencias → Ventanas de imagen → Opciones avanzadas de ventana → Apariencia predeterminada** (Vista normal)
> y **Apariencia de pantalla completa** (vista de pantalla completa). Ajusta esas configuraciones a tu gusto,
> luego guarde el espacio de trabajo. Los elementos del menú **Ver** (reglas, guías, etc.) son locales para el
> ventana de imagen actual y no se guardan por espacio de trabajo.

### Actualizaciones en vivo en Switch

Cuando cambia de espacio de trabajo, todas las ventanas de imágenes abiertas se actualizan inmediatamente: reglas, guías, barras de desplazamiento, colores de relleno y cualquier otra configuración de visualización cambian sin necesidad de cerrar y volver a abrir las imágenes.

## Acceso

**Editar → Preferencias → Espacio de trabajo**

La sección superior de la página de preferencias del espacio de trabajo enumera todos sus espacios de trabajo guardados y proporciona controles para administrarlos.

## Creando un espacio de trabajo

Configure sus paneles, herramientas y paleta exactamente como los desee y luego:

1. Abra **Editar → Preferencias → Espacio de trabajo**.
2. Haga clic en **Guardar diseño como…**.
3. Ingrese un nombre y haga clic en **Guardar**.

El nuevo espacio de trabajo aparece en el menú desplegable **Diseño activo** y en el menú **Windows**.

## Cambiar espacios de trabajo

Hay dos formas de cambiar:

- **Menú de Windows**: los nombres de los diseños aparecen en **Windows → Diseño** para un acceso rápido desde el lienzo.
- **Preferencias → Espacio de trabajo**: seleccione un diseño del menú desplegable **Diseño activo** y haga clic en **Recargar diseño**.

El cambio es inmediato: Lumi reconstruye el diseño del panel, restaura las opciones de herramientas, recarga la configuración del dispositivo, actualiza las decoraciones del lienzo, el relleno, el tema y el diseño de la caja de herramientas, todo sin reiniciar.

## Gestión de espacios de trabajo

Desde **Editar → Preferencias → Espacio de trabajo**:| Acción | Efecto |
| :--- | :--- |
| **Guardar diseño** | Sobrescribe el espacio de trabajo actual con su configuración actual. |
| **Guardar diseño como…** | Crea un nuevo espacio de trabajo con nombre a partir de su configuración actual. |
| **Cambiar nombre de diseño…** | Cambia el nombre del espacio de trabajo seleccionado. |
| **Recargar diseño** | Aplica el espacio de trabajo seleccionado inmediatamente. |
| **Eliminar diseño…** | Elimina permanentemente el espacio de trabajo seleccionado y sus archivos. |

## Configuración de persistencia

La parte inferior de la página de preferencias del espacio de trabajo controla lo que Lumi guarda automáticamente:

- **Guardar posiciones de ventana al salir**: cuando está activado, las posiciones de acoplamiento y ventana se escriben en el disco cada vez que sale.
- **Abrir ventanas en el mismo monitor**: Vuelve a abrir cada ventana en el monitor en el que estaba durante la última sesión.
- **Guardar opciones de herramienta al salir**: guarda la configuración actual de la herramienta al salir.
- **Guardar configuración del dispositivo de entrada al salir**: guarda la configuración del lápiz y del dispositivo al salir.

Estas configuraciones se aplican por espacio de trabajo: cada diseño mantiene su propio estado guardado de forma independiente.

## Flujos de trabajo de ejemplo

Algunas formas en que los artistas pueden utilizar múltiples espacios de trabajo:

- **Pintura**: bases de pincel grandes, color de relleno cálido (establecido en Preferencias → Ventanas de imagen → Apariencia predeterminada), su variante de tema preferida
- **Entintado**: guías y límites del lienzo activados, barras de desplazamiento activadas (configuradas en Preferencias → Apariencia predeterminada), color de relleno neutro
- **Roughs**: muelles ocultos, sin reglas ni cuadrículas, relleno oscuro, tamaño de icono compacto para maximizar el espacio del lienzo
- **Enfoque de pantalla completa**: diferentes colores de relleno y configuraciones de decoración en la apariencia de pantalla completa versus la apariencia predeterminada, por lo que alternar entre pantalla completa brinda un entorno de trabajo genuinamente diferente.
- **Secuencias de comandos**: panel de secuencias de comandos abierto, aumento del tamaño de fuente para facilitar la lectura, conjunto de íconos diferente