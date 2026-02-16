---
title: "GitLab CI"
type: docs
url: "hub/technical-guides/folder/GitLab-CI"
---
Continuous Integration (CI) är ett sätt att automatiskt testa, bygga och validera din kod närhelst ändringar görs.

**GitLab** tillhandahåller inbyggda CI/CD-funktioner genom sin `.gitlab-ci.yml`-fil. Den här filen, placerad i roten av ditt arkiv, berättar för GitLab hur man bygger och testar ditt projekt. Den definierar stadier och skript som körs i en ren miljö varje gång ändringar skjuts fram.

Det här dokumentet beskriver hur Lumis GitLab CI/CD-pipeline fungerar, inklusive rollen för `.gitlab-ci.yml`-filen, skalskript och externa verktyg som Meson och Ninja.

För detaljerad teknisk dokumentation av Lumi CI-byggprocessen, se [README-CI.md](https://gitlab.gnome.org/pixelmixer/lumi/-/blob/main/build/linux/appimage/README-CI.md) i arkivet.

## Grunderna i GitLab CI/CD

CI:n styrs av en fil som heter `.gitlab-ci.yml`. Denna fil definierar:

- **Stapper**: Beställda grupper av jobb (t.ex. `build-this`, `build-that`, `package-up`)
- **Jobb**: Individuella uppgifter att köra inom varje steg
- **Skript**: Skalkommandon körs för varje jobb
- **Löpare**: Datorer som GitLab använder för att köra jobb definierade i pipeline.

I Lumi är pipelinestegen:

- `dependencies`
- `build lumi`
- `appimage`

## Behållarbaserade byggnader

Lumi pipeline använder containerisering för konsekventa konstruktioner:

1. **Skapa byggbehållaren**: Det första steget använder Buildah för att skapa en Docker-bild med alla beroenden
2. **Använda behållaren**: Efterföljande steg körs inuti denna behållare, vilket säkerställer en konsekvent miljö
3. **Reproducerbara byggnader**: Behållarisolering garanterar samma resultat för olika löpare

Detta tillvägagångssätt säkerställer att byggen fungerar på samma sätt över alla GitLab-löpare och ger en kontrollerad miljö för komplexa byggprocesser.

## Rollen för Shell-skript

Jobb i `.gitlab-ci.yml` anropar vanligtvis skalkommandon direkt. Komplexa operationer flyttas ofta till separata skript som lagras i förvaret.

Lumi CI använder modulära skalskript för att organisera bygglogik:

**Exempel på skriptanrop:**
```yaml
script:
  - bash build/linux/appimage/lumi-goappimage.sh 2>&1 | tee appimage_creation.log
```

**Fördelar med detta tillvägagångssätt:**
- **Clean YAML**: Håller `.gitlab-ci.yml`-filen fokuserad på jobbstruktur
- **Underhållbarhet**: Komplex logik är lättare att felsöka och ändra i skalskript
- **Återanvändbarhet**: Skript kan användas i olika sammanhang eller miljöer
- **Modularitet**: Olika aspekter av bygget kan delas upp i fokuserade skript

Detta håller CI-konfigurationen ren samtidigt som den tillåter sofistikerade byggprocesser.

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

- `meson setup` förbereder byggkatalogen och genererar `build.ninja`
- `ninja` kör byggkommandona enligt definitionen

## Meson Build System Struktur

Byggsystemet **Meson** använder en rot `meson.build`-fil placerad i projektets rotkatalog. Den här filen definierar byggkonfigurationen på toppnivån och startpunkten för byggprocessen.

- Roten `meson.build` finns vanligtvis i samma katalog som `.gitlab-ci.yml`
- Därifrån **kaskaderar den rekursivt** till underkataloger, som var och en kan ha sin egen `meson.build`-fil
- Dessa underkatalogfiler definierar mål, källor, beroenden och bygginstruktioner som är relevanta för den katalogen

## Miljövariabler

Nyckelvariabler i Lumi pipeline inkluderar:

```yaml
variables:
  DEBIAN_FRONTEND: "noninteractive"  # Prevents interactive prompts
  DEB_VERSION: "trixie"              # Debian version for consistency
  CI_RUNNER_TAG: "x86_64"            # Architecture specification
```

**Jobbspecifika variabler:**
```yaml
build-lumi:
  variables:
    COMPILER: "clang"                                           # Compiler selection
    LINKER: "lld"                                               # Linker selection
    LUMI_PREFIX: "${CI_PROJECT_DIR}/_install-${CI_RUNNER_TAG}"  # Installation path
    DEPS_PREFIX: "/opt/lumi-deps"                               # Prebuilt dependency prefix
    MESON_OPTIONS: "-Dpkgconfig.relocatable=true -Drelocatable-bundle=yes"  # Build configuration
```Dessa variabler styr byggbeteendet och säkerställer konsistens över olika etapper och löpare.

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

I denna struktur:

- Rotfilen `meson.build` konfigurerar den övergripande byggmiljön
- Underkatalog `meson.build`-filer hanterar kompileringsdetaljer för specifika komponenter eller moduler
- Den här hierarkiska layouten håller logiken modulär och underhållsbar

## Artefakter mellan stadier

Artefakter är filer som genereras av jobb som behövs i efterföljande steg:

```yaml
build-lumi:
  # ...job configuration...
  artifacts:
    paths:
      - "${LUMI_PREFIX}/"      # Installation files
      - _build-${CI_RUNNER_TAG}/meson-logs/meson-log.txt  # Build logs
```

## Pipelinestadier och beroenden

Lumi pipeline består av tre huvudsteg:

1. **Beroenden**: Skapar en containeriserad byggmiljö med alla nödvändiga verktyg och bibliotek
2. **Bygg Lumi**: Kompilerar Lumi med Meson och Ninja i den förberedda miljön
3. **AppImage**: Paketerar den inbyggda applikationen till ett distribuerbart AppImage-format

**Scenberoende:**
```yaml
build-lumi:
  needs: [deps-debian]  # Waits for dependency container

lumi-appimage:
  needs: [build-lumi] # Waits for application build
```

Varje steg körs först efter att dess beroenden har slutförts framgångsrikt, vilket säkerställer korrekt byggordning och artefakttillgänglighet.

## Aktuella jobbnamn

Lumi `.gitlab-ci.yml` definierar för närvarande dessa jobbnamn:

- `deps-debian`
- `build-lumi`
- `lumi-appimage`

## Sammanfattning

- `.gitlab-ci.yml` definierar strukturen och logiken för pipelinen
- Jobb innehåller skalkommandon eller externa skript
– Verktyg som Meson och Ninja används inuti jobb som en del av byggprocessen

Lumi använder GitLab CI för att automatiskt bygga sin AppImage för Debian-baserade plattformar. Pipelinen bygger beroenden, kompilerar Lumi och paketerar sedan en AppImage.

För detaljer på källnivå, använd:

- `.gitlab-ci.yml` i Lumi-förvarets rot
- `build/linux/appimage/lumi-goappimage.sh`
- `build/linux/appimage/README-CI.md`

För omfattande teknisk information om Lumi CI-byggprocessen, inklusive miljöinstallation, skriptarkitektur och felsökning, se [README-CI.md](https://gitlab.gnome.org/pixelmixer/lumi/-/blob/main/build/linux/appimage/README-CI.md).