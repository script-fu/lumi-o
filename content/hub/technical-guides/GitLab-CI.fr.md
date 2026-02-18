---
title: "GitLabCI"
type: docs
---
L'intégration continue (CI) est un moyen de tester, créer et valider automatiquement votre code chaque fois que des modifications sont apportées.

**GitLab** fournit des fonctionnalités CI/CD intégrées via son fichier `.gitlab-ci.yml`. Ce fichier, placé à la racine de votre référentiel, indique à GitLab comment construire et tester votre projet. Il définit des étapes et des scripts qui sont exécutés dans un environnement propre à chaque fois que des modifications sont appliquées.

Ce document décrit le fonctionnement du pipeline GitLab CI/CD de Lumi, y compris le rôle du fichier `.gitlab-ci.yml`, des scripts shell et des outils externes tels que Meson et Ninja.

Pour une documentation technique détaillée du processus de construction Lumi CI, voir [README-CI.md](https://gitlab.gnome.org/pixelmixer/lumi/-/blob/main/build/linux/appimage/README-CI.md) dans le référentiel.

## Bases de GitLab CI/CD

Le CI est contrôlé par un fichier nommé `.gitlab-ci.yml`. Ce fichier définit :

- **Étapes** : groupes de tâches ordonnés (par exemple, `build-this`, `build-that`, `package-up`)
- **Jobs** : tâches individuelles à exécuter à chaque étape
- **Scripts** : commandes Shell exécutées pour chaque tâche
- **Runners** : ordinateurs que GitLab utilise pour exécuter les tâches définies dans le pipeline.

Dans Lumi, les étapes du pipeline sont :

- `dependencies`
- `build lumi`
- `appimage`

## Builds basées sur des conteneurs

Le pipeline Lumi utilise la conteneurisation pour des builds cohérents :

1. **Création du conteneur de construction** : la première étape utilise Buildah pour créer une image Docker avec toutes les dépendances
2. **Utilisation du conteneur** : les étapes suivantes s'exécutent dans ce conteneur, garantissant un environnement cohérent
3. **Builds reproductibles** : l'isolation des conteneurs garantit les mêmes résultats sur différents exécuteurs

Cette approche garantit que les builds fonctionnent de la même manière sur n’importe quel exécuteur GitLab et fournit un environnement contrôlé pour les processus de build complexes.

### Sources de dépendance intégrées

L'image de dépendance CI de Lumi construit la pile forkée à partir de **sources intégrées dans le dépôt** (et non de clones externes) :

- `lumi-babl/` (BABL)
- `lumi-gegl/` (GEGL)
- `lumi-gtk3/` (GTK3)

Ces répertoires sont copiés dans le contexte de construction du conteneur et compilés dans le préfixe de dépendance (généralement `/opt/lumi-deps`). Cela maintient CI reproductible et garantit que la version AppImage utilise la même source de vérité que le développement local.

## Rôle des scripts Shell

Les tâches dans `.gitlab-ci.yml` invoquent généralement directement des commandes shell. Les opérations complexes sont souvent déplacées vers des scripts distincts stockés dans le référentiel.

Le Lumi CI utilise des scripts shell modulaires pour organiser la logique de construction :

**Exemple d'invocation de script :**
```yaml
script:
  - bash build/linux/appimage/lumi-goappimage.sh 2>&1 | tee appimage_creation.log
```

**Avantages de cette approche :**
- **Clean YAML** : maintient le fichier `.gitlab-ci.yml` concentré sur la structure du travail
- **Maintenabilité** : la logique complexe est plus facile à déboguer et à modifier dans les scripts shell
- **Réutilisabilité** : les scripts peuvent être utilisés dans différents contextes ou environnements
- **Modularité** : différents aspects de la construction peuvent être séparés en scripts ciblés

Cela maintient la configuration CI propre tout en permettant des processus de construction sophistiqués.

## Intégration avec les systèmes de build

Lumi utilise **Meson** et **Ninja** pour préparer puis créer le code.

Par exemple :

```
script:
  - meson setup _build-${CI_RUNNER_TAG} -Dprefix="${LUMI_PREFIX}"
  - ninja -C _build-${CI_RUNNER_TAG}
  - ninja -C _build-${CI_RUNNER_TAG} install
```

Ici :

- `meson setup` prépare le répertoire de build et génère `build.ninja`
- `ninja` exécute les commandes de build comme défini

## Structure du système de construction de mésons

Le système de construction **Meson** utilise un fichier racine `meson.build` placé dans le répertoire racine du projet. Ce fichier définit la configuration de build de niveau supérieur et le point d'entrée pour le processus de build.- La racine `meson.build` se trouve généralement dans le même répertoire que `.gitlab-ci.yml`
- À partir de là, il **cascade récursivement** dans des sous-répertoires, chacun pouvant avoir son propre fichier `meson.build`
- Ces fichiers de sous-répertoire définissent les cibles, les sources, les dépendances et les instructions de construction pertinentes pour ce répertoire.

## Variables d'environnement

Les variables clés du pipeline Lumi incluent :

```yaml
variables:
  DEBIAN_FRONTEND: "noninteractive"  # Prevents interactive prompts
  DEB_VERSION: "trixie"              # Debian version for consistency
  CI_RUNNER_TAG: "x86_64"            # Architecture specification
```

**Variables spécifiques à l'emploi :**
```yaml
build-lumi:
  variables:
    COMPILER: "clang"                                           # Compiler selection
    LINKER: "lld"                                               # Linker selection
    LUMI_PREFIX: "${CI_PROJECT_DIR}/_install-${CI_RUNNER_TAG}"  # Installation path
    DEPS_PREFIX: "/opt/lumi-deps"                               # Prebuilt dependency prefix
    MESON_OPTIONS: "-Dpkgconfig.relocatable=true -Drelocatable-bundle=yes"  # Build configuration
```

Ces variables contrôlent le comportement de construction et garantissent la cohérence entre les différentes étapes et coureurs.

## Exemple de structure

```
project-root/
├── .gitlab-ci.yml
├── meson.build              <-- Root Meson file
├── src/
│   ├── meson.build          <-- Subdirectory Meson file
│   └── some_source.c
├── data/
│   ├── meson.build
│   └── icons/
```

Dans cette structure :

- Le fichier racine `meson.build` configure l'environnement global de construction
- Le sous-répertoire `meson.build` gère les détails de compilation pour des composants ou des modules spécifiques
- Cette disposition hiérarchique maintient la logique de construction modulaire et maintenable

## Artefacts entre les étapes

Les artefacts sont des fichiers générés par les tâches nécessaires aux étapes suivantes :

```yaml
build-lumi:
  # ...job configuration...
  artifacts:
    paths:
      - "${LUMI_PREFIX}/"      # Installation files
      - _build-${CI_RUNNER_TAG}/meson-logs/meson-log.txt  # Build logs
```

## Étapes et dépendances du pipeline

Le pipeline Lumi se compose de trois étapes principales :

1. **Dépendances** : crée un environnement de construction conteneurisé avec tous les outils et bibliothèques requis
2. **Build Lumi** : compile Lumi à l'aide de Meson et Ninja dans l'environnement préparé
3. **AppImage** : regroupe l'application intégrée dans un format AppImage distribuable

**Dépendances d'étape :**
```yaml
build-lumi:
  needs: [deps-debian]  # Waits for dependency container

lumi-appimage:
  needs: [build-lumi] # Waits for application build
```

Chaque étape ne s'exécute qu'une fois ses dépendances terminées avec succès, garantissant ainsi un ordre de construction et une disponibilité des artefacts appropriés.

## Noms des tâches actuelles

Le Lumi `.gitlab-ci.yml` définit actuellement ces noms de tâches :

- `deps-debian`
- `build-lumi`
- `lumi-appimage`

## Résumé

- `.gitlab-ci.yml` définit la structure et la logique du pipeline
- Les travaux contiennent des commandes shell ou des scripts externes
- Des outils comme Meson et Ninja sont utilisés dans les tâches dans le cadre du processus de construction

Lumi utilise GitLab CI pour créer automatiquement son AppImage pour les plates-formes basées sur Debian. Le pipeline crée des dépendances, compile Lumi, puis empaquette une AppImage.

Pour plus de détails au niveau de la source, utilisez :

- `.gitlab-ci.yml` à la racine du référentiel Lumi
- `build/linux/appimage/lumi-goappimage.sh`
- `build/linux/appimage/README-CI.md`

Pour des détails techniques complets sur le processus de création de Lumi CI, y compris la configuration de l'environnement, l'architecture des scripts et le dépannage, reportez-vous à [README-CI.md](https://gitlab.gnome.org/pixelmixer/lumi/-/blob/main/build/linux/appimage/README-CI.md).