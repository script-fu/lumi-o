---
title: "Kapan"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_source_sha256: 8b6c15ef2763fe95100759e0e2e21f2bf43bf8424317be6d414f2b5260587714
translation_lock: true
url: "hub/scripting/fundamentals/Conditionals/conditionals-when"
---
Dalam Scheme, meskipun `if` elegan dan serbaguna, namun dapat membingungkan bila digunakan tanpa `else` yang eksplisit. Hal ini terutama berlaku ketika tujuannya adalah untuk mengeksekusi satu cabang kode hanya ketika kondisinya benar, tanpa tindakan alternatif untuk kasus `false`. Dalam skenario seperti itu, konstruksi `when` memberikan alternatif yang lebih jelas dan ringkas.

Bentuk dasar `when` terlihat seperti ini:

```scheme
(when test-is-true
  do-this
  do-that)
```

- Jika `test` bernilai benar (`#t`), semua ekspresi dalam isi konstruksi `when` dieksekusi secara berurutan.
- Jika `test` bernilai false (`#f`), tidak ada yang terjadi, dan tidak ada nilai yang dikembalikan.

### Contoh

```scheme
(when (< 0 1)
  (lumi-message "Condition is true!")
  (lumi-message "Executing additional actions."))
```

### Membandingkan `if` dan `when`

Untuk lebih memahami perbedaan antara `if` dan `when`, perhatikan contoh berikut di mana keduanya digunakan bersama-sama:

```scheme
(if (= 0 1)
  (lumi-message "This will not run")
  (when (< 0 1)
    (lumi-message "The 'when' condition is true!")
    (lumi-message "Executing multiple actions within 'when'.")))
```

#### Penjelasan:

1. **Kondisi `if`**:
   - Tes `(= 0 1)` memeriksa apakah 0 sama dengan 1.
   - Karena ini salah (`#f`), cabang `else` dari `if` dieksekusi.

2. **Konstruksi `when` di Cabang `else`**:
   - Tes `when` `(< 0 1)` memeriksa apakah 0 kurang dari 1.
   - Karena ini benar (`#t`), semua ekspresi dalam badan `when` dieksekusi secara berurutan:
     - Pertama, mencetak `"The 'when' condition is true!"`.
     - Kemudian, mencetak `"Executing multiple actions within 'when'."`.

#### Mengapa Menggunakan `when` Di Sini?

- Menggunakan `when` alih-alih `if` yang lain menyederhanakan logika ketika tidak diperlukan cabang `else` yang eksplisit untuk kondisi tersebut.
- `when` memperjelas bahwa hanya cabang sebenarnya yang relevan, sehingga mengurangi potensi kebingungan.

### Ringkasan

- Gunakan `if` ketika Anda membutuhkan cabang benar dan salah.
- Gunakan `when` ketika hanya ada satu cabang untuk kasus sebenarnya, terutama ketika beberapa tindakan perlu dijalankan.
- Menggabungkan `if` dan `when` dapat membantu menyusun persyaratan yang lebih kompleks dengan jelas dan ringkas.