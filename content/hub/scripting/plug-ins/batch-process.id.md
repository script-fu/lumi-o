---
title: "Proses Batch"
type: docs
translation_provenance: ai-reviewed
translation_source_sha256: 141b5ee23e77ecfc8ef5e8112706cfaebc1a2a528518218296dffda5b9dee6d1
url: "hub/scripting/plug-ins/batch-process"
translation_lock: true
---
Contoh praktis dan menyeluruh untuk memproses banyak file sekaligus.

## Lokasi default

- [Lihat sumber](https://gitlab.gnome.org/pixelmixer/lumi-dev/-/blob/main/plug-ins/lumi/batch-process/batch-process.scm)

## Di mana muncul di Lumi

- **Berkas → Proses Batch**

## Apa yang ditunjukkannya

- `SF-DIRNAME` parameter untuk direktori sumber/tujuan
- Memvalidasi jalur GUI dengan fallback (`validate-path-and-dir`)
- Pemindaian dan iterasi direktori rekursif
- Pelaporan kemajuan untuk operasi jangka panjang