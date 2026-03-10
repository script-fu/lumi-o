---
title: "Inicio rápido"
type: docs
---
Lumi aún no se ha lanzado, está disponible como versión de desarrollo.

Si ya estás en Linux y quieres ejecutar Lumi rápidamente, usa la última **AppImage** de desarrollo de los artefactos de GitLab:

https://gitlab.gnome.org/pixelmixer/lumi-dev/-/artifacts

1. Descargue el último zip de artefactos de AppImage de desarrollo.
2. Extraiga la cremallera.
3. Haga doble clic en el archivo `Lumi*.AppImage` para ejecutarlo.

AppImage ya debería poder ejecutarse. Si no es así, habilite **Permitir ejecutar archivo como programa** en los permisos del archivo, o use el método de terminal a continuación.

```bash
chmod +x Lumi*.AppImage
./Lumi*.AppImage
```

## Configuración de Wacom en Linux

Para la pintura digital en Lumi, lo mejor suele ser una **configuración de presión lineal** simple:

- Mantenga lineal la curva de presión del controlador de la tableta.
- Mantener lineales las curvas de presión/entrada en Lumi.
- Dale forma a la sensación con el propio pincel, ya que la dinámica del pincel puede ser no lineal.

Puede verificar y restablecer la curva del controlador de Linux con:

```bash
xsetwacom --get "Wacom Intuos Pro L Pen stylus" PressureCurve
xsetwacom --set "Wacom Intuos Pro L Pen stylus" PressureCurve 0 0 100 100
```

Consejos prácticos:

- Lumi actualmente bloquea la entrada problemática del pad/anillo táctil de Wacom para evitar fallos en X11. En su lugar, asigne botones de la tableta a un tamaño de pincel **relativo** arriba/abajo.
- Si arrastrar el tamaño de un pincel con `Alt` no funciona, es posible que su escritorio esté usando `Alt` para mover ventanas. Cambie ese acceso directo del administrador de ventanas a `Super` o desactívelo.

Si desea trabajar desde el código fuente, vaya a [Technical Guides](/hub/technical-guides/) y [Installation](/hub/technical-guides/Installation/).