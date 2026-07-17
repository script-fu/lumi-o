---
title: "AppImage"
type: docs
url: "hub/technical-guides/AppImage"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 939beb6f4f1657ab77f785d1753385360cca7920b6291dbcd09f4687bfdfc502
---

AppImage to jednoplikowy pakiet aplikacji Linux. Pobierasz jeden plik, oznaczasz go jako wykonywalny i uruchamiasz bez instalowania oprogramowania w całym systemie.

Oficjalna strona AppImage: https://appimage.org/

AppImage zapewnia przenośną wersję Lumi, która działa bez instalacji ani modyfikacji systemu. Jest idealny dla artystów, którzy chcą od razu korzystać z oprogramowania bez zarządzania zależnościami, kompilowania kodu źródłowego ani konfigurowania środowiska programistycznego.

Jako samodzielny plik wykonywalny AppImage można przechowywać w dowolnym miejscu systemu. Ułatwia to testowanie nowych wydań, przechowywanie wielu wersji lub przenoszenie oprogramowania między komputerami.

W procesie rozwoju Lumi AppImage pełni rolę przenośnej wersji testowej, która ściśle odpowiada wynikom ciągłej integracji. Umożliwia to niezawodne testowanie w spójnym środowisku, przy jednoczesnym skupieniu lokalnych kompilacji źródeł na pracy programistycznej.

Uwaga: CI buduje AppImage przy użyciu zintegrowanych źródeł zależności Lumi w repozytorium (BABL/GEGL/GTK3), więc stos zależności jest spójny z lokalnym przepływem pracy `lumi-build-script.sh`.

## Wersja release a AppImage deweloperski

- **Release AppImage**: jeszcze niedostępny (Lumi nie zostało jeszcze wydane).
- **Development AppImage (artefakt CI)**: generowany automatycznie z bieżących commitów deweloperskich do testów.

Ten przewodnik opisuje głównie przepływ pracy **development AppImage**.

Bieżąca strona artefaktów:

https://gitlab.gnome.org/pixelmixer/lumi-dev/-/artifacts

## Podstawy pobierania AppImage z CI

CI tworzy pliki zip z artefaktami (na przykład `lumi-appimage*.zip`).

Podstawowy przepływ ręczny:

1. Pobierz najnowszy plik zip z artefaktami CI.
2. Rozpakuj go.
3. Uruchom dołączony plik `Lumi*.AppImage`.

Poniższe skrypty to opcjonalne narzędzia pomocnicze automatyzujące te kroki.

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# Unpack latest downloaded CI zip from ~/Downloads
bash lumi-appimage-unpack-zip.sh

# Launch AppImage with terminal output
bash lumi-appimage-launch.sh
```

## Opcjonalne skrypty pomocnicze

- `lumi-appimage-unpack-zip.sh`
  - znajduje najnowszy `lumi-appimage*.zip` w `~/Downloads`
  - instaluje AppImage w `~/AppImage/Lumi/Lumi_CI.AppImage`
  - instaluje zasoby pulpitu w `~/.local/share/applications/lumi.desktop`

- `lumi-appimage-launch.sh`
  - uruchamia AppImage w terminalu
  - włącza wyjście w czasie wykonywania (`APPIMAGE_DEBUG=1`)

## Uwagi ogólne

- Jeśli uruchamiasz AppImage ręcznie (bez skryptów pomocniczych), najpierw nadaj mu uprawnienia do wykonywania:

```bash
chmod +x ~/AppImage/Lumi/Lumi_CI.AppImage
```

`lumi-appimage-unpack-zip.sh` stosuje uprawnienia wykonywania automatycznie.

- Jeśli Lumi już działa z innej kompilacji, zamknij je przed uruchomieniem AppImage.
