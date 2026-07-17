---
title: "GitLab CI"
type: docs
url: "hub/technical-guides/GitLab-CI"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 9917cebc417adeeae24d91b05b919b679a397d5db652cf4442d4330c0f8eeea5
---

Continuous Integration (CI) är ett sätt att automatiskt testa, bygga och validera din kod när ändringar görs.

**GitLab** tillhandahåller inbyggda CI/CD-funktioner via filen `.gitlab-ci.yml`. Den här filen, placerad i roten av ditt repository, talar om för GitLab hur projektet ska byggas och testas. Den definierar stages och scripts som körs i en ren miljö varje gång ändringar pushas.

Det här dokumentet beskriver hur Lumis GitLab CI/CD-pipeline fungerar, inklusive rollen för filen `.gitlab-ci.yml`, shellskript och externa verktyg som Meson och Ninja.

För detaljerad teknisk dokumentation av Lumis CI-byggprocess, se [README-CI.md](https://gitlab.gnome.org/pixelmixer/lumi/-/blob/main/build/linux/appimage/README-CI.md) i repositoryt.

## Grunderna i GitLab CI/CD

CI styrs av en fil som heter `.gitlab-ci.yml`. Den här filen definierar:

- **Stages**: ordnade grupper av jobs (t.ex. `build-this`, `build-that`, `package-up`)
- **Jobs**: enskilda uppgifter som körs inom varje stage
- **Scripts**: shellkommandon som körs för varje job
- **Runners**: datorer som GitLab använder för att köra jobs som definieras i pipelinen

I Lumi är pipeline-stages:

- `dependencies`
- `build lumi`
- `appimage`

## Containerbaserade builds

Lumi-pipelinen använder containerisering för konsekventa builds:

1. **Skapa build-containern**: det första staget använder Buildah för att skapa en Docker-image med alla beroenden
2. **Använda containern**: efterföljande stages körs i den här containern, vilket säkerställer en konsekvent miljö
3. **Reproducerbara builds**: containerisolering garanterar samma resultat på olika runners

Det här tillvägagångssättet säkerställer att builds fungerar på samma sätt på alla GitLab-runners och ger en kontrollerad miljö för komplexa byggprocesser.

### Integrerade beroendekällor

Lumis CI-beroendeimage bygger den forkade stacken från **integrerade källor i repositoryt** (inga externa kloner):

- `lumi-babl/` (BABL)
- `lumi-gegl/` (GEGL)
- `lumi-gtk3/` (GTK3)

De här katalogerna kopieras till containerbuild-kontexten och kompileras till beroendeprefixet (vanligtvis `/opt/lumi-deps`). Det här håller CI reproducerbart och säkerställer att AppImage-builden använder samma source of truth som lokal utveckling.

## Rollen för shellskript

Jobs i `.gitlab-ci.yml` anropar vanligtvis shellkommandon direkt. Komplexa operationer flyttas ofta till separata skript som lagras i repositoryt.

Lumi CI använder modulära shellskript för att organisera bygglogiken:

**Exempel på skriptanrop:**
```yaml
script:
  - bash build/linux/appimage/lumi-goappimage.sh 2>&1 | tee appimage_creation.log
```

**Fördelar med det här tillvägagångssättet:**
- **Rent YAML**: håller filen `.gitlab-ci.yml` fokuserad på jobstrukturen
- **Underhållbarhet**: komplex logik är lättare att felsöka och ändra i shellskript
- **Återanvändbarhet**: skript kan användas i olika sammanhang eller miljöer
- **Modularitet**: olika aspekter av builden kan separeras i fokuserade skript

Det här håller CI-konfigurationen ren samtidigt som sofistikerade byggprocesser är möjliga.

## Integration med byggsystem

Lumi använder **Meson** och **Ninja** för att förbereda och sedan bygga koden.

Till exempel:

```
script:
  - meson setup _build-${CI_RUNNER_TAG} -Dprefix="${LUMI_PREFIX}"
  - ninja -C _build-${CI_RUNNER_TAG}
  - ninja -C _build-${CI_RUNNER_TAG} install
```

Här:

- `meson setup` förbereder build-katalogen och genererar `build.ninja`
- `ninja` kör build-kommandona enligt definitionen

## Struktur för Meson-byggsystemet

**Meson**-byggsystemet använder en rotfil `meson.build` placerad i projektets rotkatalog. Den här filen definierar build-konfigurationen på högsta nivå och ingångspunkten för byggprocessen.

- Rotfilen `meson.build` finns vanligtvis i samma katalog som `.gitlab-ci.yml`
- Därifrån **kaskaderar den rekursivt** till underkataloger, som var och en kan ha sin egen `meson.build`-fil
- Dessa underkatalogsfiler definierar targets, källor, beroenden och bygginstruktioner som är relevanta för den katalogen

## Miljövariabler

Viktiga variabler i Lumi-pipelinen inkluderar:

```yaml
variables:
  DEBIAN_FRONTEND: "noninteractive"  # Prevents interactive prompts
  DEB_VERSION: "trixie"              # Debian version for consistency
  CI_RUNNER_TAG: "x86_64"            # Architecture specification
```

**Jobspecifika variabler:**
```yaml
build-lumi:
  variables:
    COMPILER: "clang"                                           # Compiler selection
    LINKER: "lld"                                               # Linker selection
    LUMI_PREFIX: "${CI_PROJECT_DIR}/_install-${CI_RUNNER_TAG}"  # Installation path
    DEPS_PREFIX: "/opt/lumi-deps"                               # Prebuilt dependency prefix
    MESON_OPTIONS: "-Dpkgconfig.relocatable=true -Drelocatable-bundle=yes"  # Build configuration
```

De här variablerna styr byggbeteendet och säkerställer konsekvens mellan olika stages och runners.

## Exempelstruktur

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

I den här strukturen:

- Rotfilen `meson.build` konfigurerar den övergripande build-miljön
- `meson.build`-filer i underkataloger hanterar kompileringsdetaljer för specifika komponenter eller moduler
- Den här hierarkiska layouten håller bygglogiken modulär och underhållbar

## Artifacts mellan stages

Artifacts är filer som genereras av jobs och behövs i efterföljande stages:

```yaml
build-lumi:
  # ...job configuration...
  artifacts:
    paths:
      - "${LUMI_PREFIX}/"      # Installation files
      - _build-${CI_RUNNER_TAG}/meson-logs/meson-log.txt  # Build logs
```

## Pipeline-stages och beroenden

Lumi-pipelinen består av tre huvudsakliga stages:

1. **Dependencies**: skapar en containeriserad build-miljö med alla nödvändiga verktyg och bibliotek
2. **Build Lumi**: kompilerar Lumi med Meson och Ninja i den förberedda miljön
3. **AppImage**: paketerar den byggda applikationen i ett distribuerbart AppImage-format

**Stage-beroenden:**
```yaml
build-lumi:
  needs: [deps-debian]  # Waits for dependency container

lumi-appimage:
  needs: [build-lumi] # Waits for application build
```

Varje stage körs först efter att dess beroenden har slutförts, vilket säkerställer rätt byggordning och tillgänglighet av artifacts.

## Aktuella jobbnamn

Lumi `.gitlab-ci.yml` definierar för närvarande dessa jobbnamn:

- `deps-debian`
- `build-lumi`
- `lumi-appimage`

## Sammanfattning

- `.gitlab-ci.yml` definierar pipelinens struktur och logik
- Jobs innehåller shellkommandon eller externa skript
- Verktyg som Meson och Ninja används inom jobs som en del av byggprocessen

Lumi använder GitLab CI för att automatiskt bygga sitt AppImage för Debian-baserade plattformar. Pipelinen bygger beroenden, kompilerar Lumi och paketerar sedan ett AppImage.

För detaljer på källnivå, använd:

- `.gitlab-ci.yml` i roten av Lumi-repositoryt
- `build/linux/appimage/lumi-goappimage.sh`
- `build/linux/appimage/README-CI.md`

För omfattande tekniska detaljer om Lumis CI-byggprocess, inklusive miljökonfiguration, skriptarkitektur och felsökning, se [README-CI.md](https://gitlab.gnome.org/pixelmixer/lumi/-/blob/main/build/linux/appimage/README-CI.md).
