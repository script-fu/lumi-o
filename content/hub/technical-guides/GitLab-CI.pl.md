---
title: "GitLab CI"
type: docs
url: "hub/technical-guides/GitLab-CI"
---
Ciągła integracja (CI) to sposób na automatyczne testowanie, kompilowanie i sprawdzanie poprawności kodu po każdym wprowadzeniu zmian.

**GitLab** zapewnia wbudowane funkcje CI/CD poprzez plik `.gitlab-ci.yml`. Ten plik, umieszczony w katalogu głównym repozytorium, informuje GitLab, jak zbudować i przetestować projekt. Definiuje etapy i skrypty, które są uruchamiane w czystym środowisku za każdym razem, gdy wprowadzane są zmiany.

Ten dokument opisuje działanie potoku CI/CD GitLab Lumi, w tym rolę pliku `.gitlab-ci.yml`, skryptów powłoki i narzędzi zewnętrznych, takich jak Meson i Ninja.

Aby uzyskać szczegółową dokumentację techniczną procesu kompilacji Lumi CI, zobacz [README-CI.md](https://gitlab.gnome.org/pixelmixer/lumi/-/blob/main/build/linux/appimage/README-CI.md) w repozytorium.

## Podstawy GitLaba CI/CD

CI jest kontrolowany przez plik o nazwie `.gitlab-ci.yml`. Ten plik definiuje:

- **Etapy**: Uporządkowane grupy zadań (np. `build-this`, `build-that`, `package-up`)
- **Praca**: Indywidualne zadania do wykonania na każdym etapie
- **Skrypty**: Polecenia powłoki wykonywane dla każdego zadania
- **Runners**: Komputery używane przez GitLab do uruchamiania zadań zdefiniowanych w potoku.

W Lumi etapy rurociągu to:

- `dependencies`
- `build lumi`
- `appimage`

## Kompilacje oparte na kontenerach

Potok Lumi wykorzystuje konteneryzację w celu zapewnienia spójnych kompilacji:

1. **Tworzenie kontenera kompilacji**: Pierwszy etap wykorzystuje Buildah do utworzenia obrazu Dockera ze wszystkimi zależnościami
2. **Korzystanie z kontenera**: Kolejne etapy przebiegają wewnątrz tego kontenera, zapewniając spójne środowisko
3. **Powtarzalne kompilacje**: Izolacja kontenera gwarantuje takie same wyniki w różnych modułach uruchamiających

Takie podejście gwarantuje, że kompilacje będą działać w ten sam sposób w każdym programie uruchamiającym GitLab i zapewnia kontrolowane środowisko dla złożonych procesów kompilacji.

## Rola skryptów powłoki

Zadania w `.gitlab-ci.yml` zazwyczaj bezpośrednio wywołują polecenia powłoki. Złożone operacje są często przenoszone do oddzielnych skryptów przechowywanych w repozytorium.

Lumi CI wykorzystuje modułowe skrypty powłoki do organizowania logiki kompilacji:

**Przykład wywołania skryptu:**
```yaml
script:
  - bash build/linux/appimage/lumi-goappimage.sh 2>&1 | tee appimage_creation.log
```

**Zalety tego podejścia:**
- **Wyczyść YAML**: Utrzymuje plik `.gitlab-ci.yml` skupiony na strukturze zadania
- **Łatwość konserwacji**: Złożoną logikę łatwiej jest debugować i modyfikować w skryptach powłoki
- **Ponowne użycie**: Skrypty mogą być używane w różnych kontekstach i środowiskach
- **Modułowość**: Różne aspekty kompilacji można podzielić na skoncentrowane skrypty

Dzięki temu konfiguracja CI jest czysta, a jednocześnie umożliwia zaawansowane procesy kompilacji.

## Integracja z systemami kompilacji

Lumi używa **Mesona** i **Ninji** do przygotowania, a następnie zbudowania kodu.

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

## Meson Budowa struktury systemu

System kompilacji **Meson** wykorzystuje plik główny `meson.build` umieszczony w katalogu głównym projektu. Ten plik definiuje konfigurację kompilacji najwyższego poziomu i punkt wejścia dla procesu kompilacji.

- Katalog główny `meson.build` zazwyczaj znajduje się w tym samym katalogu co `.gitlab-ci.yml`
- Stamtąd **przechodzi rekurencyjnie** do podkatalogów, z których każdy może mieć własny plik `meson.build`
- Te pliki podkatalogów definiują cele, źródła, zależności i instrukcje kompilacji odpowiednie dla tego katalogu

## Zmienne środowiskowe

Kluczowe zmienne w rurociągu Lumi obejmują:

```yaml
variables:
  DEBIAN_FRONTEND: "noninteractive"  # Prevents interactive prompts
  DEB_VERSION: "trixie"              # Debian version for consistency
  CI_RUNNER_TAG: "x86_64"            # Architecture specification
```

**Zmienne specyficzne dla stanowiska:**
```yaml
build-lumi:
  variables:
    COMPILER: "clang"                                           # Compiler selection
    LINKER: "lld"                                               # Linker selection
    LUMI_PREFIX: "${CI_PROJECT_DIR}/_install-${CI_RUNNER_TAG}"  # Installation path
    DEPS_PREFIX: "/opt/lumi-deps"                               # Prebuilt dependency prefix
    MESON_OPTIONS: "-Dpkgconfig.relocatable=true -Drelocatable-bundle=yes"  # Build configuration
```Te zmienne kontrolują zachowanie kompilacji i zapewniają spójność na różnych etapach i biegaczach.

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
- Podkatalog plików `meson.build` obsługuje szczegóły kompilacji dla określonych komponentów lub modułów
- Ten hierarchiczny układ sprawia, że logika kompilacji jest modułowa i łatwa w utrzymaniu

## Artefakty między etapami

Artefakty to pliki generowane przez zadania, które są potrzebne w kolejnych etapach:

```yaml
build-lumi:
  # ...job configuration...
  artifacts:
    paths:
      - "${LUMI_PREFIX}/"      # Installation files
      - _build-${CI_RUNNER_TAG}/meson-logs/meson-log.txt  # Build logs
```

## Etapy potoku i zależności

Rurociąg Lumi składa się z trzech głównych etapów:

1. **Zależności**: Tworzy kontenerowe środowisko kompilacji ze wszystkimi wymaganymi narzędziami i bibliotekami
2. **Buduj Lumi**: Kompiluje Lumi przy użyciu Mesona i Ninja w przygotowanym środowisku
3. **AppImage**: Pakuje zbudowaną aplikację do rozpowszechnialnego formatu AppImage

**Zależności etapów:**
```yaml
build-lumi:
  needs: [deps-debian]  # Waits for dependency container

lumi-appimage:
  needs: [build-lumi] # Waits for application build
```

Każdy etap jest uruchamiany dopiero po pomyślnym zakończeniu jego zależności, zapewniając odpowiednią kolejność kompilacji i dostępność artefaktów.

## Nazwy bieżących stanowisk

Lumi `.gitlab-ci.yml` obecnie definiuje następujące nazwy zadań:

- `deps-debian`
- `build-lumi`
- `lumi-appimage`

## Podsumowanie

- `.gitlab-ci.yml` definiuje strukturę i logikę potoku
- Zadania zawierają polecenia powłoki lub skrypty zewnętrzne
- Narzędzia takie jak Meson i Ninja są używane w zadaniach w ramach procesu kompilacji

Lumi używa GitLab CI do automatycznego tworzenia AppImage dla platform opartych na Debianie. Potok buduje zależności, kompiluje Lumi, a następnie pakuje AppImage.

Aby uzyskać szczegółowe informacje na poziomie źródła, użyj:

- `.gitlab-ci.yml` w katalogu głównym repozytorium Lumi
- `build/linux/appimage/lumi-goappimage.sh`
- `build/linux/appimage/README-CI.md`

Szczegółowe informacje techniczne dotyczące procesu kompilacji Lumi CI, w tym konfiguracji środowiska, architektury skryptów i rozwiązywania problemów, można znaleźć w artykule [README-CI.md](https://gitlab.gnome.org/pixelmixer/lumi/-/blob/main/build/linux/appimage/README-CI.md).