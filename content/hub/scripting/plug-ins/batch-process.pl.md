---
title: "Proces wsadowy"
type: docs
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 141b5ee23e77ecfc8ef5e8112706cfaebc1a2a528518218296dffda5b9dee6d1
url: "hub/scripting/plug-ins/batch-process"
---
Praktyczny, kompleksowy przykład przetwarzania wielu plików za jednym razem.

## Kod źródłowy

- [Zobacz kod źródłowy](https://gitlab.gnome.org/pixelmixer/lumi-dev/-/blob/main/plug-ins/lumi/batch-process/batch-process.scm)

## Menu w Lumi

- **Plik → Proces wsadowy**

## Co pokazuje

- Parametry `SF-DIRNAME` dla katalogów źródłowych/docelowych
- Walidacja ścieżek GUI z rezerwami (`validate-path-and-dir`)
- Rekurencyjne skanowanie katalogów i iteracja
- Raportowanie postępów w przypadku długotrwałych operacji
