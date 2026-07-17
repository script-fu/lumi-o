---
title: "GitLab CI"
type: docs
url: "hub/technical-guides/GitLab-CI"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 9917cebc417adeeae24d91b05b919b679a397d5db652cf4442d4330c0f8eeea5
---

Continuous Integration (CI) is een manier om uw code automatisch te testen, bouwen en valideren wanneer er wijzigingen worden aangebracht.

**GitLab** biedt ingebouwde CI/CD-functies via het bestand `.gitlab-ci.yml`. Dit bestand, geplaatst in de root van uw repository, vertelt GitLab hoe u uw project moet bouwen en testen. Het definieert stages en scripts die in een schone omgeving worden uitgevoerd telkens wanneer er wijzigingen worden gepusht.

Dit document beschrijft hoe de GitLab CI/CD-pipeline van Lumi werkt, inclusief de rol van het bestand `.gitlab-ci.yml`, shellscripts en externe tools zoals Meson en Ninja.

Voor gedetailleerde technische documentatie van het Lumi CI-buildproces, zie [README-CI.md](https://gitlab.gnome.org/pixelmixer/lumi/-/blob/main/build/linux/appimage/README-CI.md) in de repository.

## GitLab CI/CD-basisprincipes

De CI wordt beheerd door een bestand met de naam `.gitlab-ci.yml`. Dit bestand definieert:

- **Stages**: geordende groepen jobs (bijvoorbeeld `build-this`, `build-that`, `package-up`)
- **Jobs**: individuele taken die binnen elke stage worden uitgevoerd
- **Scripts**: shellopdrachten die voor elke job worden uitgevoerd
- **Runners**: computers die GitLab gebruikt om jobs uit te voeren die in de pipeline zijn gedefinieerd

In Lumi zijn de pipeline-stages:

- `dependencies`
- `build lumi`
- `appimage`

## Op containers gebaseerde builds

De Lumi-pipeline maakt gebruik van containerisatie voor consistente builds:

1. **De buildcontainer maken**: in de eerste stage wordt Buildah gebruikt om een Docker-image met alle dependencies te maken
2. **De container gebruiken**: volgende stages draaien in deze container, waardoor een consistente omgeving wordt gegarandeerd
3. **Reproduceerbare builds**: containerisolatie garandeert dezelfde resultaten op verschillende runners

Deze aanpak zorgt ervoor dat builds op elke GitLab-runner op dezelfde manier werken en biedt een gecontroleerde omgeving voor complexe buildprocessen.

### Geïntegreerde dependencybronnen

De CI-dependencyimage van Lumi bouwt de geforkte stack op vanuit **in-repo geïntegreerde bronnen** (geen externe clones):

- `lumi-babl/` (BABL)
- `lumi-gegl/` (GEGL)
- `lumi-gtk3/` (GTK3)

Deze mappen worden gekopieerd naar de containerbuildcontext en gecompileerd in het dependencyprefix (doorgaans `/opt/lumi-deps`). Hierdoor blijft CI reproduceerbaar en gebruikt de AppImage-build dezelfde bron van waarheid als lokale ontwikkeling.

## Rol van shellscripts

Jobs in `.gitlab-ci.yml` roepen doorgaans rechtstreeks shellopdrachten aan. Complexe bewerkingen worden vaak verplaatst naar aparte scripts die in de repository zijn opgeslagen.

De Lumi CI gebruikt modulaire shellscripts om de buildlogica te organiseren:

**Voorbeeld van scriptaanroep:**
```yaml
script:
  - bash build/linux/appimage/lumi-goappimage.sh 2>&1 | tee appimage_creation.log
```

**Voordelen van deze aanpak:**
- **Schoon YAML**: houdt het bestand `.gitlab-ci.yml` gericht op de jobstructuur
- **Onderhoudbaarheid**: complexe logica is gemakkelijker te debuggen en aan te passen in shellscripts
- **Herbruikbaarheid**: scripts kunnen in verschillende contexten of omgevingen worden gebruikt
- **Modulariteit**: verschillende aspecten van de build kunnen worden opgesplitst in gerichte scripts

Hierdoor blijft de CI-configuratie overzichtelijk terwijl geavanceerde buildprocessen mogelijk blijven.

## Integratie met buildsystemen

Lumi gebruikt **Meson** en **Ninja** om de code voor te bereiden en vervolgens te bouwen.

Bijvoorbeeld:

```
script:
  - meson setup _build-${CI_RUNNER_TAG} -Dprefix="${LUMI_PREFIX}"
  - ninja -C _build-${CI_RUNNER_TAG}
  - ninja -C _build-${CI_RUNNER_TAG} install
```

Hier:

- `meson setup` bereidt de buildmap voor en genereert `build.ninja`
- `ninja` voert de buildopdrachten uit zoals gedefinieerd

## Structuur van het Meson-buildsysteem

Het **Meson**-buildsysteem gebruikt een rootbestand `meson.build` in de rootmap van het project. Dit bestand definieert de buildconfiguratie op het hoogste niveau en het toegangspunt voor het buildproces.

- Het rootbestand `meson.build` bevindt zich doorgaans in dezelfde map als `.gitlab-ci.yml`
- Van daaruit **cascadeert het recursief** naar submappen, die elk hun eigen `meson.build`-bestand kunnen hebben
- Deze submapbestanden definiëren targets, bronnen, dependencies en buildinstructies die relevant zijn voor die map

## Omgevingsvariabelen

Belangrijke variabelen in de Lumi-pipeline zijn onder meer:

```yaml
variables:
  DEBIAN_FRONTEND: "noninteractive"  # Prevents interactive prompts
  DEB_VERSION: "trixie"              # Debian version for consistency
  CI_RUNNER_TAG: "x86_64"            # Architecture specification
```

**Jobspecifieke variabelen:**
```yaml
build-lumi:
  variables:
    COMPILER: "clang"                                           # Compiler selection
    LINKER: "lld"                                               # Linker selection
    LUMI_PREFIX: "${CI_PROJECT_DIR}/_install-${CI_RUNNER_TAG}"  # Installation path
    DEPS_PREFIX: "/opt/lumi-deps"                               # Prebuilt dependency prefix
    MESON_OPTIONS: "-Dpkgconfig.relocatable=true -Drelocatable-bundle=yes"  # Build configuration
```

Deze variabelen sturen het buildgedrag aan en zorgen voor consistentie tussen verschillende stages en runners.

## Voorbeeldstructuur

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

In deze structuur:

- Het rootbestand `meson.build` configureert de algehele buildomgeving
- Submapbestanden `meson.build` verwerken compilatiedetails voor specifieke componenten of modules
- Deze hiërarchische indeling houdt de buildlogica modulair en onderhoudbaar

## Artifacts tussen stages

Artifacts zijn bestanden die door jobs worden gegenereerd en nodig zijn in volgende stages:

```yaml
build-lumi:
  # ...job configuration...
  artifacts:
    paths:
      - "${LUMI_PREFIX}/"      # Installation files
      - _build-${CI_RUNNER_TAG}/meson-logs/meson-log.txt  # Build logs
```

## Pipeline-stages en dependencies

De Lumi-pipeline bestaat uit drie hoofdstages:

1. **Dependencies**: maakt een gecontaineriseerde buildomgeving met alle vereiste tools en bibliotheken
2. **Build Lumi**: compileert Lumi met Meson en Ninja in de voorbereide omgeving
3. **AppImage**: verpakt de gebouwde applicatie in een distribueerbaar AppImage-formaat

**Stage-dependencies:**
```yaml
build-lumi:
  needs: [deps-debian]  # Waits for dependency container

lumi-appimage:
  needs: [build-lumi] # Waits for application build
```

Elke stage draait pas nadat de dependencies succesvol zijn voltooid, waardoor de juiste buildvolgorde en beschikbaarheid van artifacts worden gegarandeerd.

## Huidige jobnamen

De Lumi `.gitlab-ci.yml` definieert momenteel deze jobnamen:

- `deps-debian`
- `build-lumi`
- `lumi-appimage`

## Samenvatting

- `.gitlab-ci.yml` definieert de structuur en logica van de pipeline
- Jobs bevatten shellopdrachten of externe scripts
- Tools zoals Meson en Ninja worden binnen jobs gebruikt als onderdeel van het buildproces

Lumi gebruikt GitLab CI om automatisch een AppImage te bouwen voor op Debian gebaseerde platforms. De pipeline bouwt dependencies, compileert Lumi en verpakt vervolgens een AppImage.

Gebruik voor details op bronniveau:

- `.gitlab-ci.yml` in de root van de Lumi-repository
- `build/linux/appimage/lumi-goappimage.sh`
- `build/linux/appimage/README-CI.md`

Voor uitgebreide technische details over het Lumi CI-buildproces, inclusief omgevingsconfiguratie, scriptarchitectuur en troubleshooting, raadpleeg [README-CI.md](https://gitlab.gnome.org/pixelmixer/lumi/-/blob/main/build/linux/appimage/README-CI.md).
