---
title: "Obraz aplikacji"
type: docs
url: "hub/technical-guides/AppImage"
---
AppImage to jednoplikowy pakiet aplikacji dla systemu Linux. Pobierasz jeden plik, oznaczasz go jako wykonywalny i uruchamiasz bez instalowania oprogramowania w całym systemie.

Oficjalna strona AppImage: https://appimage.org/

AppImage zapewnia przenośną wersję Lumi, która działa bez instalacji lub modyfikacji systemu. Jest idealny dla artystów, którzy chcą natychmiast korzystać z oprogramowania, bez zarządzania zależnościami, kompilowania kodu źródłowego lub konfigurowania środowiska programistycznego.

Jako samodzielny plik wykonywalny AppImage można przechowywać w dowolnym miejscu systemu. Ułatwia to testowanie nowych wydań, przechowywanie wielu wersji lub przenoszenie oprogramowania między komputerami.

W procesie rozwoju Lumi AppImage funkcjonuje jako przenośna wersja testowa, która ściśle odpowiada wynikom ciągłej integracji. Umożliwia to niezawodne testowanie w spójnym środowisku, przy jednoczesnym skupieniu kompilacji lokalnych źródeł na pracach programistycznych.

## Wersja a obraz aplikacji rozwojowej

- **Wydanie AppImage**: jeszcze niedostępne (Lumi nie zostało jeszcze wydane).
- **Obraz aplikacji deweloperskiej (artefakt CI)**: generowany automatycznie na podstawie bieżących zobowiązań programistycznych do testów.

Ten przewodnik opisuje głównie przepływ pracy **programowania AppImage**.

Bieżąca strona artefaktów:

https://gitlab.gnome.org/pixelmixer/lumi-dev/-/artifacts

## Podstawy pobierania obrazu aplikacji CI

CI tworzy pliki zip z artefaktami (na przykład `lumi-appimage*.zip`).

Podstawowy przepływ ręczny:

1. Pobierz najnowszy plik zip z artefaktami CI.
2. Wyodrębnij to.
3. Uruchom dołączony plik `Lumi*.AppImage`.

Poniższe skrypty są opcjonalnymi pomocnikami, które automatyzują te kroki.

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
  - instaluje AppImage na `~/AppImage/Lumi/Lumi_CI.AppImage`
  - instaluje zasoby pulpitu na `~/.local/share/applications/lumi.desktop`

- `lumi-appimage-launch.sh`
  - uruchamia AppImage w terminalu
  - włącza wyjście w czasie wykonywania (`APPIMAGE_DEBUG=1`)

## Wspólne notatki

- Jeśli uruchamiasz AppImage ręcznie (bez skryptów pomocniczych), najpierw uczyń go wykonywalnym:

```bash
chmod +x ~/AppImage/Lumi/Lumi_CI.AppImage
```

`lumi-appimage-unpack-zip.sh` już automatycznie stosuje uprawnienia do wykonywania.

- Jeśli Lumi działa już z innej kompilacji, zamknij ją przed uruchomieniem AppImage.