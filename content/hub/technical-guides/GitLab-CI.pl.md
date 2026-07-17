---
title: "GitLab CI"
type: docs
url: "hub/technical-guides/GitLab-CI"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 9917cebc417adeeae24d91b05b919b679a397d5db652cf4442d4330c0f8eeea5
---

Ciągła integracja (CI) to sposób na automatyczne testowanie, kompilowanie i weryfikowanie kodu przy każdej wprowadzonej zmianie.

**GitLab** udostępnia wbudowane funkcje CI/CD poprzez plik `.gitlab-ci.yml`. Ten plik, umieszczony w katalogu głównym repozytorium, informuje GitLab, jak zbudować i przetestować projekt. Definiuje stage’y i skrypty uruchamiane w czystym środowisku za każdym razem, gdy wprowadzane są zmiany.

Ten dokument opisuje działanie pipeline’u CI/CD GitLab w projekcie Lumi, w tym rolę pliku `.gitlab-ci.yml`, skryptów powłoki oraz narzędzi zewnętrznych, takich jak Meson i Ninja.

Szczegółową dokumentację techniczną procesu kompilacji Lumi CI znajdziesz w [README-CI.md](https://gitlab.gnome.org/pixelmixer/lumi/-/blob/main/build/linux/appimage/README-CI.md) w repozytorium.

## Podstawy GitLab CI/CD

CI jest kontrolowane przez plik o nazwie `.gitlab-ci.yml`. Ten plik definiuje:

- **Stage’y**: uporządkowane grupy jobów (np. `build-this`, `build-that`, `package-up`)
- **Joby**: pojedyncze zadania do wykonania w każdym stage’u
- **Skrypty**: polecenia powłoki wykonywane dla każdego joba
- **Runners**: komputery, których GitLab używa do uruchamiania jobów zdefiniowanych w pipeline’ie

W Lumi stage’y pipeline’u to:

- `dependencies`
- `build lumi`
- `appimage`

## Kompilacje oparte na kontenerach

Pipeline Lumi wykorzystuje konteneryzację w celu zapewnienia spójnych kompilacji:

1. **Tworzenie kontenera kompilacji**: w pierwszym stage’u Buildah tworzy obraz Docker z wszystkimi zależnościami
2. **Korzystanie z kontenera**: kolejne stage’y działają w tym kontenerze, zapewniając spójne środowisko
3. **Powtarzalne kompilacje**: izolacja kontenera gwarantuje te same wyniki na różnych runnerach

To podejście zapewnia, że kompilacje działają tak samo na każdym runnerze GitLab i zapewnia kontrolowane środowisko dla złożonych procesów budowania.

### Zintegrowane źródła zależności

Obraz zależności CI Lumi buduje sforkowany stos z **zintegrowanych źródeł w repozytorium** (bez zewnętrznych klonów):

- `lumi-babl/` (BABL)
- `lumi-gegl/` (GEGL)
- `lumi-gtk3/` (GTK3)

Te katalogi są kopiowane do kontekstu budowania kontenera i kompilowane do prefiksu zależności (zazwyczaj `/opt/lumi-deps`). Dzięki temu CI pozostaje powtarzalne, a kompilacja AppImage korzysta z tego samego źródła prawdy co lokalny development.

## Rola skryptów powłoki

Joby w `.gitlab-ci.yml` zazwyczaj wywołują polecenia powłoki bezpośrednio. Złożone operacje są często przenoszone do osobnych skryptów przechowywanych w repozytorium.

Lumi CI wykorzystuje modułowe skrypty powłoki do organizacji logiki kompilacji:

**Przykład wywołania skryptu:**
```yaml
script:
  - bash build/linux/appimage/lumi-goappimage.sh 2>&1 | tee appimage_creation.log
```

**Korzyści z tego podejścia:**
- **Czysty YAML**: plik `.gitlab-ci.yml` pozostaje skupiony na strukturze jobów
- **Łatwość utrzymania**: złożoną logikę łatwiej debugować i modyfikować w skryptach powłoki
- **Ponowne wykorzystanie**: skrypty można używać w różnych kontekstach lub środowiskach
- **Modularność**: różne aspekty kompilacji można rozdzielić na wyspecjalizowane skrypty

Dzięki temu konfiguracja CI pozostaje przejrzysta, a jednocześnie umożliwia zaawansowane procesy budowania.

## Integracja z systemami kompilacji

Lumi używa **Meson** i **Ninja** do przygotowania, a następnie kompilacji kodu.

Na przykład:

```
script:
  - meson setup _build-${CI_RUNNER_TAG} -Dprefix="${LUMI_PREFIX}"
  - ninja -C _build-${CI_RUNNER_TAG}
  - ninja -C _build-${CI_RUNNER_TAG} install
```

Tutaj:

- `meson setup` przygotowuje katalog kompilacji i generuje `build.ninja`
- `ninja` uruchamia polecenia kompilacji zgodnie z definicją

## Struktura systemu kompilacji Meson

System kompilacji **Meson** używa głównego pliku `meson.build` umieszczonego w katalogu głównym projektu. Ten plik definiuje konfigurację kompilacji najwyższego poziomu i punkt wejścia procesu budowania.

- Główny plik `meson.build` zazwyczaj znajduje się w tym samym katalogu co `.gitlab-ci.yml`
- Stamtąd **kaskadowo przechodzi rekurencyjnie** do podkatalogów, z których każdy może mieć własny plik `meson.build`
- Te pliki podkatalogów definiują cele, źródła, zależności i instrukcje kompilacji odpowiednie dla danego katalogu

## Zmienne środowiskowe

Kluczowe zmienne w pipeline’ie Lumi obejmują:

```yaml
variables:
  DEBIAN_FRONTEND: "noninteractive"  # Prevents interactive prompts
  DEB_VERSION: "trixie"              # Debian version for consistency
  CI_RUNNER_TAG: "x86_64"            # Architecture specification
```

**Zmienne specyficzne dla joba:**
```yaml
build-lumi:
  variables:
    COMPILER: "clang"                                           # Compiler selection
    LINKER: "lld"                                               # Linker selection
    LUMI_PREFIX: "${CI_PROJECT_DIR}/_install-${CI_RUNNER_TAG}"  # Installation path
    DEPS_PREFIX: "/opt/lumi-deps"                               # Prebuilt dependency prefix
    MESON_OPTIONS: "-Dpkgconfig.relocatable=true -Drelocatable-bundle=yes"  # Build configuration
```

Te zmienne kontrolują zachowanie kompilacji i zapewniają spójność między różnymi stage’ami i runnerami.

## Przykładowa struktura

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

W tej strukturze:

- Główny plik `meson.build` konfiguruje ogólne środowisko kompilacji
- Pliki `meson.build` w podkatalogach obsługują szczegóły kompilacji poszczególnych komponentów lub modułów
- Ta hierarchiczna organizacja utrzymuje logikę kompilacji w modularnej i łatwej w utrzymaniu formie

## Artefakty między stage’ami

Artefakty to pliki generowane przez joby, które są potrzebne w kolejnych stage’ach:

```yaml
build-lumi:
  # ...job configuration...
  artifacts:
    paths:
      - "${LUMI_PREFIX}/"      # Installation files
      - _build-${CI_RUNNER_TAG}/meson-logs/meson-log.txt  # Build logs
```

## Stage’y pipeline’u i zależności

Pipeline Lumi składa się z trzech głównych stage’ów:

1. **Dependencies**: tworzy skonteneryzowane środowisko kompilacji ze wszystkimi wymaganymi narzędziami i bibliotekami
2. **Build Lumi**: kompiluje Lumi za pomocą Meson i Ninja w przygotowanym środowisku
3. **AppImage**: pakuje zbudowaną aplikację w dystrybuowalny format AppImage

**Zależności między stage’ami:**
```yaml
build-lumi:
  needs: [deps-debian]  # Waits for dependency container

lumi-appimage:
  needs: [build-lumi] # Waits for application build
```

Każdy stage uruchamia się dopiero po pomyślnym zakończeniu zależności, co zapewnia właściwą kolejność kompilacji i dostępność artefaktów.

## Aktualne nazwy jobów

Obecnie Lumi `.gitlab-ci.yml` definiuje te nazwy jobów:

- `deps-debian`
- `build-lumi`
- `lumi-appimage`

## Podsumowanie

- `.gitlab-ci.yml` definiuje strukturę i logikę pipeline’u
- Joby zawierają polecenia powłoki lub zewnętrzne skrypty
- Narzędzia takie jak Meson i Ninja są używane w jobach jako część procesu kompilacji

Lumi używa GitLab CI do automatycznego budowania AppImage dla platform opartych na Debianie. Pipeline buduje zależności, kompiluje Lumi, a następnie pakuje AppImage.

Szczegóły na poziomie kodu źródłowego:

- `.gitlab-ci.yml` w katalogu głównym repozytorium Lumi
- `build/linux/appimage/lumi-goappimage.sh`
- `build/linux/appimage/README-CI.md`

Pełne szczegóły techniczne procesu kompilacji Lumi CI, w tym konfiguracja środowiska, architektura skryptów i rozwiązywanie problemów, znajdziesz w [README-CI.md](https://gitlab.gnome.org/pixelmixer/lumi/-/blob/main/build/linux/appimage/README-CI.md).
