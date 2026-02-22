---
title: "Format pliku (.lum)"
type: docs
---
Lumi wykorzystuje otwarty format plików oparty na katalogach (`.lum`) zaprojektowany z myślą o wydajności, niezawodności i długoterminowej dostępności.

## Przegląd

Plik `.lum` jest w rzeczywistości katalogiem zawierającym:
- **Metadane** (warstwy, tryby mieszania, właściwości).
- **Bufory warstw** (indywidualne dane pikseli dla każdej warstwy).
- **Maski** (dane w skali szarości dla masek warstw).
- **Historia odzyskiwania** (migawki przyrostowe).

Taka struktura umożliwia szybkie zapisywanie, leniwe ładowanie dużych plików i przywracanie pracy nawet po awarii.

## Właściwości klucza

### Otwarte i czytelne

Format `.lum` wykorzystuje metadane XML i skompresowane bufory binarne. Strukturę warstw, właściwości i tryby mieszania można sprawdzić za pomocą zwykłego tekstu. Brak zastrzeżonego kodeka; dane pikseli są przechowywane w standardowym formacie bufora GEGL.

### Stopniowe oszczędzanie

Zapisywanie przyrostowe musi być włączone dla każdego projektu w oknie dialogowym **Zapisz jako** (pole wyboru **Zapis przyrostowy** i przycisk obrotowy **Maksymalna liczba zapisów**). Po włączeniu Ctrl+S zapisuje tylko zmodyfikowane warstwy, zamiast przepisywać cały projekt, drastycznie skracając czas oszczędzania. Ustawienie jest przechowywane w projekcie i obowiązuje pomiędzy sesjami.

### Leniwe ładowanie

Duże projekty otwierają się szybko. Piksele warstw są ładowane z dysku tylko wtedy, gdy:
- Warstwa staje się widoczna.
- Malujesz na warstwie.
- Warstwa zostanie wyeksportowana lub złożona.

Bardzo duże projekty (ponad 500 warstw, wiele gigabajtów danych) pozostają responsywne. Leniwe ładowanie jest domyślnie włączone i można je włączyć w **Edycja → Preferencje → Wydajność → Zasoby pamięci**.

### Autozapis

Lumi automatycznie zapisuje zmiany w **oddzielnej lokalizacji pamięci podręcznej** (`~/.cache/lumi/autosave/`) w regularnych odstępach czasu. Autozapisy są niezależne od pliku roboczego i nie modyfikują go. Interwał i lokalizację pamięci podręcznej można skonfigurować w **Edycja → Preferencje → Wydajność**.

## Dostęp

### Zapisz i zapisz jako

- **Plik** → **Zapisz** (Ctrl+S): Zapisz w bieżącym katalogu `.lum`.
- **Plik** → **Zapisz jako** (Shift+Ctrl+S): Zapisz do nowego pliku `.lum`. Okno dialogowe Zapisz jako zawiera opcje typu kompresji oraz przełącznik **Zapis przyrostowy** (z limitem **Maksymalna liczba zapisów**), umożliwiający włączenie lub wyłączenie zapisywania przyrostowego dla tego projektu.

Niezapisane zmiany są oznaczone gwiazdką (*) w tytule okna.

### Eksportuj

- **Plik** → **Eksportuj jako** (Shift+Ctrl+E): Eksportuj do formatu PNG, JPEG, TIFF lub innego.
- **Plik** → **Nadpisz** (Ctrl+E): Ponowny eksport do ostatnio eksportowanego pliku.

Eksportowanie spłaszcza widoczne warstwy i konwertuje przestrzeń kolorów widmowych na sRGB.

### Importuj

- **Plik** → **Otwórz** (Ctrl+O): Załaduj projekt `.lum`.
- **Plik** → **Otwórz jako warstwy** (Shift+Ctrl+O): Importuj pliki `.lum`, XCF lub PSD jako nowe warstwy.
- **Plik** → **Ostatnie pliki**: Szybki dostęp do ostatnio otwartych projektów.

Pliki PSD i XCF są podczas importu konwertowane do natywnego formatu Lumi.

## Zgodność importu i eksportu

### Obsługiwane formaty importu
- **.lum**: natywny format Lumi.
- **.xcf**: Natywny format GIMP (zachowane warstwy i podstawowe właściwości).
- **.psd**: format Photoshopa (z zachowaniem warstw i trybów mieszania).
- **PNG, JPEG, TIFF itp.**: Import spłaszczonych obrazów.

### Obsługiwane formaty eksportu
- **PNG**: bezstratny, z przezroczystością alfa.
- **JPEG**: Stratny, spłaszczony.
- **TIFF**: bezstratny lub skompresowany LZW.
- **XCF**: format zgodny z GIMP. Tylko eksport; warstwy i podstawowe właściwości zachowane.

## Odzyskiwanie projektuLumi utrzymuje automatyczne zapisywanie w tle i ręczne przyrostowe punkty kontrolne, oba dostępne z **Plik** → **Odzyskaj obraz**. Aby uzyskać szczegółowe informacje, zobacz stronę [File Recovery](../recovery).

## Organizacja

Plik `.lum` jest katalogiem o ustalonej strukturze:

```
my-painting.lum/
  ├── metadata.xml                       (image structure, layer tree, properties)
  ├── thumbnail-YYYYMMDD-HHMMSS.png      (last-saved thumbnail)
  ├── drawables/
  │   ├── layer-<name>.geglbuf           (pixel data per layer)
  │   └── mask-<name>.geglbuf            (mask data, shares layer name)
  ├── icc/                               (embedded colour profiles)
  ├── parasites/                         (per-image metadata)
  ├── paths/                             (vector paths as SVG)
  ├── configs/                           (non-destructive filter configurations)
  └── recovery/
      └── primary-01.lum/                (incremental save checkpoint)
          ├── metadata.xml
          ├── drawables/                 (only modified buffers)
          ├── delta-0001.lum/            (Ctrl+S checkpoint)
          └── delta-0002.lum/
```

Bufory warstw noszą nazwy warstw (`layer-Background.geglbuf`), a nie są numerowane sekwencyjnie. Spacje w nazwach warstw są zapisywane jako podkreślenia; warstwy grupowe otrzymują przyrostek `-GROUP`. Maski mają wspólną nazwę warstwy (`mask-Background.geglbuf`).

Każdy `recovery/primary-NN.lum/` to pełny zapis stanu bazowego. Kolejne naciśnięcia klawiszy Ctrl+S dołączają podkatalogi `delta-NNNN.lum/` zawierające tylko zmodyfikowane bufory od ostatniej linii bazowej, co pozwala na szybkie zapisywanie punktów kontrolnych niezależnie od rozmiaru projektu.

Autozapisy mają tę samą strukturę, ale są przechowywane oddzielnie w `~/.cache/lumi/autosave/`, pozostawiając plik roboczy nietknięty.
- **Bardzo duże projekty**: Projekt zawierający ponad 1000 warstw i terabajty danych najbardziej skorzysta na leniwym ładowaniu; jednakże ostateczny eksport do formatu obrazu płaskiego może zająć trochę czasu.
- **Dyski sieciowe**: Zapisywanie w katalogach podłączonych do sieci jest obsługiwane, ale jest wolniejsze niż pamięć lokalna ze względu na opóźnienia we/wy.