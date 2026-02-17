---
title: "Proces wsadowy"
type: docs
---
Praktyczny, kompleksowy przykład przetwarzania wielu plików za jednym razem.

## Gdzie mieszka

- [View the source](https://gitlab.gnome.org/pixelmixer/lumi-dev/-/blob/main/plug-ins/lumi/batch-process/batch-process.scm)

## Gdzie pojawia się w Lumi

- **Plik → Proces wsadowy**

## Co to pokazuje

- Parametry `SF-DIRNAME` dla katalogów źródłowych/docelowych
- Walidacja ścieżek GUI z rezerwami (`validate-path-and-dir`)
- Rekurencyjne skanowanie katalogów i iteracja
- Raportowanie postępów w przypadku długotrwałych operacji