---
title: "Démarrage rapide"
type: docs
---
Lumi n'est pas encore sorti, il est disponible en version de développement.

Si vous êtes déjà sous Linux et souhaitez exécuter Lumi rapidement, utilisez la dernière **AppImage de développement** des artefacts GitLab :

https://gitlab.gnome.org/pixelmixer/lumi-dev/-/artifacts

1. Téléchargez le dernier zip d'artefact AppImage de développement.
2. Extrayez le zip.
3. Double-cliquez sur le fichier `Lumi*.AppImage` pour l'exécuter.

L'AppImage devrait déjà être exécutable. Si ce n'est pas le cas, activez **Autoriser l'exécution du fichier en tant que programme** dans les autorisations du fichier ou utilisez la méthode de terminal ci-dessous.

```bash
chmod +x Lumi*.AppImage
./Lumi*.AppImage
```

## Configuration de Wacom sous Linux

Pour la peinture numérique dans Lumi, une simple **configuration de pression linéaire** est généralement la meilleure :

- Gardez la courbe de pression du pilote de la tablette linéaire.
- Conserver les courbes de pression/entrée dans Lumi linéaire.
- Façonnez la sensation avec le pinceau lui-même, car la dynamique du pinceau peut être non linéaire.

Vous pouvez vérifier et réinitialiser la courbe du pilote Linux avec :

```bash
xsetwacom --get "Wacom Intuos Pro L Pen stylus" PressureCurve
xsetwacom --set "Wacom Intuos Pro L Pen stylus" PressureCurve 0 0 100 100
```

Conseils pratiques :

- Lumi bloque actuellement l'entrée problématique du pad/anneau tactile Wacom pour éviter les problèmes X11. Mappez plutôt les boutons de la tablette sur une taille de pinceau **relative** vers le haut/bas.
- Si le glissement de la taille d'un pinceau avec `Alt` ne fonctionne pas, votre bureau utilise peut-être `Alt` pour déplacer les fenêtres. Remplacez ce raccourci du gestionnaire de fenêtres par `Super` ou désactivez-le.

Si vous souhaitez travailler à partir du code source, accédez à [Technical Guides](/hub/technical-guides/) et [Installation](/hub/technical-guides/Installation/).