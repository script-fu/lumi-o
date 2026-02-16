---
title: "GitLab-CI"
type: docs
url: "hub/technical-guides/GitLab-CI"
---
Continuous Integration (CI) is een manier om uw code automatisch te testen, bouwen en valideren wanneer er wijzigingen worden aangebracht.

**GitLab** biedt ingebouwde CI/CD-functies via het bestand `.gitlab-ci.yml`. Dit bestand, geplaatst in de root van je repository, vertelt GitLab hoe je je project moet bouwen en testen. Het definieert fasen en scripts die in een schone omgeving worden uitgevoerd telkens wanneer er wijzigingen worden doorgevoerd.

Dit document schetst hoe de GitLab CI/CD-pijplijn van Lumi werkt, inclusief de rol van het bestand `.gitlab-ci.yml`, shell-scripts en externe tools zoals Meson en Ninja.

Voor gedetailleerde technische documentatie van het Lumi CI-bouwproces, zie [README-CI.md](https://gitlab.gnome.org/pixelmixer/lumi/-/blob/main/build/linux/appimage/README-CI.md) in de repository.

## GitLab CI/CD-basisprincipes

Het CI wordt beheerd door een bestand met de naam `.gitlab-ci.yml`. Dit bestand definieert:

- **Fasen**: bestelde groepen taken (bijvoorbeeld `build-this`, `build-that`, `package-up`)
- **Jobs**: individuele taken die binnen elke fase moeten worden uitgevoerd
- **Scripts**: Shell-opdrachten worden voor elke taak uitgevoerd
- **Runners**: computers die GitLab gebruikt om taken uit te voeren die in de pijplijn zijn gedefinieerd.

In Lumi zijn de pijplijnfasen:

- `dependencies`
- `build lumi`
- `appimage`

## Op containers gebaseerde builds

De Lumi-pijplijn maakt gebruik van containerisatie voor consistente builds:

1. **De build-container maken**: in de eerste fase wordt Buildah gebruikt om een Docker-image met alle afhankelijkheden te maken
2. **De container gebruiken**: de daaropvolgende fasen worden in deze container uitgevoerd, waardoor een consistente omgeving wordt gegarandeerd
3. **Reproduceerbare builds**: containerisolatie garandeert dezelfde resultaten voor verschillende runners

Deze aanpak zorgt ervoor dat builds op elke GitLab-runner op dezelfde manier werken en biedt een gecontroleerde omgeving voor complexe bouwprocessen.

## Rol van Shell-scripts

Taken in `.gitlab-ci.yml` roepen doorgaans rechtstreeks shell-opdrachten op. Complexe bewerkingen worden vaak verplaatst naar afzonderlijke scripts die in de repository zijn opgeslagen.

De Lumi CI gebruikt modulaire shell-scripts om de bouwlogica te organiseren:

**Voorbeeld van scriptaanroep:**
```yaml
script:
  - bash build/linux/appimage/lumi-goappimage.sh 2>&1 | tee appimage_creation.log
```

**Voordelen van deze aanpak:**
- **Schoon YAML**: Houdt het bestand `.gitlab-ci.yml` gericht op de taakstructuur
- **Onderhoudbaarheid**: complexe logica is gemakkelijker te debuggen en aan te passen in shell-scripts
- **Herbruikbaarheid**: Scripts kunnen in verschillende contexten of omgevingen worden gebruikt
- **Modulariteit**: verschillende aspecten van de build kunnen worden onderverdeeld in gerichte scripts

Hierdoor blijft de CI-configuratie schoon en zijn geavanceerde bouwprocessen mogelijk.

## Integratie met bouwsystemen

Lumi gebruikt **Meson** en **Ninja** om de code voor te bereiden en vervolgens te bouwen.

Bijvoorbeeld:

```
script:
  - meson setup _build-${CI_RUNNER_TAG} -Dprefix="${LUMI_PREFIX}"
  - ninja -C _build-${CI_RUNNER_TAG}
  - ninja -C _build-${CI_RUNNER_TAG} install
```

Hier:

- `meson setup` bereidt de build-directory voor en genereert `build.ninja`
- `ninja` voert de build-opdrachten uit zoals gedefinieerd

## Meson Build-systeemstructuur

Het **Meson**-buildsysteem gebruikt een root `meson.build`-bestand dat in de hoofdmap van het project is geplaatst. Dit bestand definieert de buildconfiguratie op het hoogste niveau en het toegangspunt voor het buildproces.

- De root `meson.build` bevindt zich doorgaans in dezelfde map als `.gitlab-ci.yml`
- Van daaruit gaat het **recursief** over in submappen, die elk hun eigen `meson.build` bestand kunnen hebben
- Deze submapbestanden definiëren doelen, bronnen, afhankelijkheden en bouwinstructies die relevant zijn voor die map

## Omgevingsvariabelen

Belangrijke variabelen in de Lumi-pijplijn zijn onder meer:

```yaml
variables:
  DEBIAN_FRONTEND: "noninteractive"  # Prevents interactive prompts
  DEB_VERSION: "trixie"              # Debian version for consistency
  CI_RUNNER_TAG: "x86_64"            # Architecture specification
```

**Taakspecifieke variabelen:**
```yaml
build-lumi:
  variables:
    COMPILER: "clang"                                           # Compiler selection
    LINKER: "lld"                                               # Linker selection
    LUMI_PREFIX: "${CI_PROJECT_DIR}/_install-${CI_RUNNER_TAG}"  # Installation path
    DEPS_PREFIX: "/opt/lumi-deps"                               # Prebuilt dependency prefix
    MESON_OPTIONS: "-Dpkgconfig.relocatable=true -Drelocatable-bundle=yes"  # Build configuration
```Deze variabelen bepalen het bouwgedrag en zorgen voor consistentie tussen verschillende fasen en lopers.

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

- Het hoofdbestand `meson.build` configureert de algehele bouwomgeving
- Submap `meson.build` bestanden verwerken compilatiegegevens voor specifieke componenten of modules
- Deze hiërarchische lay-out houdt de bouwlogica modulair en onderhoudbaar

## Artefacten tussen fasen

Artefacten zijn bestanden die worden gegenereerd door taken die nodig zijn in de volgende fasen:

```yaml
build-lumi:
  # ...job configuration...
  artifacts:
    paths:
      - "${LUMI_PREFIX}/"      # Installation files
      - _build-${CI_RUNNER_TAG}/meson-logs/meson-log.txt  # Build logs
```

## Pijplijnfasen en afhankelijkheden

De Lumi-pijplijn bestaat uit drie hoofdfasen:

1. **Afhankelijkheden**: Creëert een gecontaineriseerde bouwomgeving met alle vereiste tools en bibliotheken
2. **Bouw Lumi**: compileert Lumi met Meson en Ninja in de voorbereide omgeving
3. **AppImage**: verpakt de ingebouwde applicatie in een distribueerbaar AppImage-formaat

**Fase-afhankelijkheden:**
```yaml
build-lumi:
  needs: [deps-debian]  # Waits for dependency container

lumi-appimage:
  needs: [build-lumi] # Waits for application build
```

Elke fase wordt pas uitgevoerd nadat de afhankelijkheden met succes zijn voltooid, waardoor de juiste bouwvolgorde en beschikbaarheid van artefacten wordt gegarandeerd.

## Huidige taaknamen

De Lumi `.gitlab-ci.yml` definieert momenteel deze taaknamen:

- `deps-debian`
- `build-lumi`
- `lumi-appimage`

## Samenvatting

- `.gitlab-ci.yml` definieert de structuur en logica van de pijplijn
- Taken bevatten shell-opdrachten of externe scripts
- Tools zoals Meson en Ninja worden in banen gebruikt als onderdeel van het bouwproces

Lumi gebruikt GitLab CI om zijn AppImage automatisch te bouwen voor op Debian gebaseerde platforms. De pijplijn bouwt afhankelijkheden op, compileert Lumi en verpakt vervolgens een AppImage.

Gebruik voor details op bronniveau:

- `.gitlab-ci.yml` in de hoofdmap van de Lumi-repository
- `build/linux/appimage/lumi-goappimage.sh`
- `build/linux/appimage/README-CI.md`

Voor uitgebreide technische details over het Lumi CI-bouwproces, inclusief het instellen van de omgeving, scriptarchitectuur en probleemoplossing, raadpleegt u [README-CI.md](https://gitlab.gnome.org/pixelmixer/lumi/-/blob/main/build/linux/appimage/README-CI.md).