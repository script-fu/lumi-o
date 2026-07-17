---
title: "GitLab CI"
type: docs
url: "hub/technical-guides/GitLab-CI"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 9917cebc417adeeae24d91b05b919b679a397d5db652cf4442d4330c0f8eeea5
---

L'intégration continue (CI) permet de tester, construire et valider automatiquement votre code à chaque modification.

**GitLab** fournit des fonctionnalités CI/CD intégrées via son fichier `.gitlab-ci.yml`. Placé à la racine de votre dépôt, ce fichier indique à GitLab comment construire et tester votre projet. Il définit des étapes et des scripts exécutés dans un environnement propre à chaque push.

Ce document décrit le fonctionnement du pipeline GitLab CI/CD de Lumi, y compris le rôle du fichier `.gitlab-ci.yml`, des scripts shell et d'outils externes tels que Meson et Ninja.

Pour une documentation technique détaillée du processus de build CI de Lumi, consultez [README-CI.md](https://gitlab.gnome.org/pixelmixer/lumi/-/blob/main/build/linux/appimage/README-CI.md) dans le dépôt.

## Bases de GitLab CI/CD

Le CI est contrôlé par un fichier nommé `.gitlab-ci.yml`. Ce fichier définit :

- **Stages** : groupes ordonnés de jobs (par exemple, `build-this`, `build-that`, `package-up`)
- **Jobs** : tâches individuelles exécutées à chaque stage
- **Scripts** : commandes shell exécutées pour chaque job
- **Runners** : machines que GitLab utilise pour exécuter les jobs définis dans le pipeline

Dans Lumi, les stages du pipeline sont :

- `dependencies`
- `build lumi`
- `appimage`

## Builds basés sur des conteneurs

Le pipeline Lumi utilise la conteneurisation pour des builds cohérents :

1. **Création du conteneur de build** : la première stage utilise Buildah pour créer une image Docker avec toutes les dépendances
2. **Utilisation du conteneur** : les stages suivantes s'exécutent dans ce conteneur, garantissant un environnement cohérent
3. **Builds reproductibles** : l'isolation des conteneurs garantit les mêmes résultats sur différents runners

Cette approche garantit que les builds fonctionnent de la même manière sur n'importe quel runner GitLab et fournit un environnement contrôlé pour les processus de build complexes.

### Sources de dépendances intégrées

L'image de dépendances CI de Lumi construit la pile forkée à partir de **sources intégrées dans le dépôt** (et non de clones externes) :

- `lumi-babl/` (BABL)
- `lumi-gegl/` (GEGL)
- `lumi-gtk3/` (GTK3)

Ces répertoires sont copiés dans le contexte de build du conteneur et compilés dans le préfixe de dépendances (généralement `/opt/lumi-deps`). Cela maintient le CI reproductible et garantit que le build AppImage utilise la même source de vérité que le développement local.

## Rôle des scripts shell

Les jobs dans `.gitlab-ci.yml` invoquent généralement directement des commandes shell. Les opérations complexes sont souvent déplacées vers des scripts distincts stockés dans le dépôt.

Le CI de Lumi utilise des scripts shell modulaires pour organiser la logique de build :

**Exemple d'invocation de script :**
```yaml
script:
  - bash build/linux/appimage/lumi-goappimage.sh 2>&1 | tee appimage_creation.log
```

**Avantages de cette approche :**
- **YAML épuré** : le fichier `.gitlab-ci.yml` reste concentré sur la structure des jobs
- **Maintenabilité** : la logique complexe est plus facile à déboguer et à modifier dans les scripts shell
- **Réutilisabilité** : les scripts peuvent être utilisés dans différents contextes ou environnements
- **Modularité** : différents aspects du build peuvent être séparés en scripts ciblés

Cela maintient la configuration CI propre tout en permettant des processus de build sophistiqués.

## Intégration avec les systèmes de build

Lumi utilise **Meson** et **Ninja** pour préparer puis construire le code.

Par exemple :

```
script:
  - meson setup _build-${CI_RUNNER_TAG} -Dprefix="${LUMI_PREFIX}"
  - ninja -C _build-${CI_RUNNER_TAG}
  - ninja -C _build-${CI_RUNNER_TAG} install
```

Ici :

- `meson setup` prépare le répertoire de build et génère `build.ninja`
- `ninja` exécute les commandes de build comme défini

## Structure du système de build Meson

Le système de build **Meson** utilise un fichier racine `meson.build` placé dans le répertoire racine du projet. Ce fichier définit la configuration de build de niveau supérieur et le point d'entrée pour le processus de build.

- La racine `meson.build` se trouve généralement dans le même répertoire que `.gitlab-ci.yml`
- À partir de là, il **cascade récursivement** dans des sous-répertoires, chacun pouvant avoir son propre fichier `meson.build`
- Ces fichiers de sous-répertoire définissent les cibles, les sources, les dépendances et les instructions de build pertinentes pour ce répertoire

## Variables d'environnement

Les variables clés du pipeline Lumi incluent :

```yaml
variables:
  DEBIAN_FRONTEND: "noninteractive"  # Prevents interactive prompts
  DEB_VERSION: "trixie"              # Debian version for consistency
  CI_RUNNER_TAG: "x86_64"            # Architecture specification
```

**Variables spécifiques aux jobs :**
```yaml
build-lumi:
  variables:
    COMPILER: "clang"                                           # Compiler selection
    LINKER: "lld"                                               # Linker selection
    LUMI_PREFIX: "${CI_PROJECT_DIR}/_install-${CI_RUNNER_TAG}"  # Installation path
    DEPS_PREFIX: "/opt/lumi-deps"                               # Prebuilt dependency prefix
    MESON_OPTIONS: "-Dpkgconfig.relocatable=true -Drelocatable-bundle=yes"  # Build configuration
```

Ces variables contrôlent le comportement de build et garantissent la cohérence entre les différentes stages et runners.

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

- Le fichier racine `meson.build` configure l'environnement global de build
- Les fichiers `meson.build` des sous-répertoires gèrent les détails de compilation pour des composants ou modules spécifiques
- Cette disposition hiérarchique maintient la logique de build modulaire et maintenable

## Artefacts entre les stages

Les artefacts sont des fichiers générés par les jobs et nécessaires aux stages suivantes :

```yaml
build-lumi:
  # ...job configuration...
  artifacts:
    paths:
      - "${LUMI_PREFIX}/"      # Installation files
      - _build-${CI_RUNNER_TAG}/meson-logs/meson-log.txt  # Build logs
```

## Stages et dépendances du pipeline

Le pipeline Lumi se compose de trois stages principales :

1. **Dependencies** : crée un environnement de build conteneurisé avec tous les outils et bibliothèques requis
2. **Build Lumi** : compile Lumi à l'aide de Meson et Ninja dans l'environnement préparé
3. **AppImage** : regroupe l'application construite dans un format AppImage distribuable

**Dépendances entre stages :**
```yaml
build-lumi:
  needs: [deps-debian]  # Waits for dependency container

lumi-appimage:
  needs: [build-lumi] # Waits for application build
```

Chaque stage ne s'exécute qu'une fois ses dépendances terminées avec succès, garantissant un ordre de build correct et la disponibilité des artefacts.

## Noms des jobs actuels

Le `.gitlab-ci.yml` de Lumi définit actuellement ces noms de jobs :

- `deps-debian`
- `build-lumi`
- `lumi-appimage`

## Résumé

- `.gitlab-ci.yml` définit la structure et la logique du pipeline
- Les jobs contiennent des commandes shell ou des scripts externes
- Des outils comme Meson et Ninja sont utilisés dans les jobs dans le cadre du processus de build

Lumi utilise GitLab CI pour construire automatiquement son AppImage pour les plateformes basées sur Debian. Le pipeline crée les dépendances, compile Lumi, puis empaquette une AppImage.

Pour les détails au niveau source, consultez :

- `.gitlab-ci.yml` à la racine du dépôt Lumi
- `build/linux/appimage/lumi-goappimage.sh`
- `build/linux/appimage/README-CI.md`

Pour des détails techniques complets sur le processus de build CI de Lumi, y compris la configuration de l'environnement, l'architecture des scripts et le dépannage, consultez [README-CI.md](https://gitlab.gnome.org/pixelmixer/lumi/-/blob/main/build/linux/appimage/README-CI.md).
