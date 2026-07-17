#!/usr/bin/env python3
"""One-shot fixes for known MT corruption traps across translation files.

Run after manual review of diffs. Re-run lint_translation_traps.py to verify.

Usage:
  python3 scripts/fix_translation_traps.py
  python3 scripts/fix_translation_traps.py --dry-run
"""

from __future__ import annotations

import argparse
import re
from pathlib import Path

CONTENT = Path("content")

WRAPPING_CAR_HEADING = {
    "de": ("### Autoersatz", "### `car` ersetzen"),
    "es": ("### Reemplazo de automóvil", "### Sustitución de `car`"),
    "fr": ("### Remplacement de voiture", "### Remplacer `car`"),
    "it": ("### Sostituzione auto", "### Sostituire `car`"),
    "nl": ("### Autovervanging", "### `car` vervangen"),
    "pt": ("### Substituição de carro", "### Substituir `car`"),
    "pt-br": ("### Substituição de carro", "### Substituir `car`"),
    "ru": ("### Замена автомобиля", "### Замена `car`"),
    "sv": ("### Bilbyte", "### Ersätta `car`"),
    "ja": ("### 車の買い替え", "### `car` の置き換え"),
    "ko": ("### 자동차 교체", "### `car` 대체"),
    "zh-cn": ("### 汽车更换", "### 替换 `car`"),
    "zh-tw": ("### 汽車更換", "### 替換 `car`"),
    "vi": ("### Thay xe", "### Thay thế `car`"),
    "th": ("### เปลี่ยนรถ", "### แทนที่ `car`"),
    "id": ("### Pengganti Otomatis", "### Mengganti `car`"),
}

WRAPPING_RANDOM_SEED_HEADING = {
    "de": ("### Zufälliger Samen", "### Zufälliger Seed"),
    "fr": ("### Graine aléatoire", "### Seed aléatoire"),
    "id": ("### Benih Acak", "### Seed acak"),
    "uk": ("### Випадкове насіння", "### Випадковий seed"),
    "vi": ("### Hạt giống ngẫu nhiên", "### Seed ngẫu nhiên"),
    "th": ("### เมล็ดสุ่ม", "### Seed สุ่ม"),
}

WRAPPING_BODY_FIXES = [
    (re.compile(r"\*\*\*auto\*\*\*", re.I), "***car***"),
    (re.compile(r"im ersten Element kapseln", re.I), "in `first-item` kapseln"),
    (re.compile(r"die graine est générée", re.I), "le seed est généré"),
    (re.compile(r"wie der Samen erzeugt wird", re.I), "wie der Seed erzeugt wird"),
    (re.compile(r"generación de semillas", re.I), "generación del seed"),
]

ALIST_COMMENT_PAIR = {
    "ar": (
        ";; Alist manuell definieren",
        ";; تعريف alist يدويًا",
        ";; Programmatisch ein neues Paar hinzufügen",
        ";; إضافة زوج جديد برمجيًا",
    ),
    "es": (
        ";; Alist manuell definieren",
        ";; Definir manualmente una alista",
        ";; Programmatisch ein neues Paar hinzufügen",
        ";; Añadir programáticamente un nuevo par",
    ),
    "fr": (
        ";; Alist manuell definieren",
        ";; Définir manuellement une aliste",
        ";; Programmatisch ein neues Paar hinzufügen",
        ";; Ajouter programmatiquement une nouvelle paire",
    ),
    "it": (
        ";; Alist manuell definieren",
        ";; Definire manualmente un alist",
        ";; Programmatisch ein neues Paar hinzufügen",
        ";; Aggiungere programmaticamente una nuova coppia",
    ),
    "nl": (
        ";; Alist manuell definieren",
        ";; Handmatig een alist definiëren",
        ";; Programmatisch ein neues Paar hinzufügen",
        ";; Programmatisch een nieuw paar toevoegen",
    ),
    "pt": (
        ";; Alist manuell definieren",
        ";; Definir manualmente uma alista",
        ";; Programmatisch ein neues Paar hinzufügen",
        ";; Adicionar programaticamente um novo par",
    ),
    "pt-br": (
        ";; Alist manuell definieren",
        ";; Definir manualmente uma alista",
        ";; Programmatisch ein neues Paar hinzufügen",
        ";; Adicionar programaticamente um novo par",
    ),
    "pl": (
        ";; Alist manuell definieren",
        ";; Ręczne zdefiniowanie alisty",
        ";; Programmatisch ein neues Paar hinzufügen",
        ";; Programowe dodanie nowej pary",
    ),
    "ru": (
        ";; Alist manuell definieren",
        ";; Ручное определение alist",
        ";; Programmatisch ein neues Paar hinzufügen",
        ";; Программное добавление новой пары",
    ),
    "sv": (
        ";; Alist manuell definieren",
        ";; Definiera en alist manuellt",
        ";; Programmatisch ein neues Paar hinzufügen",
        ";; Lägg till ett nytt par programmatiskt",
    ),
    "ja": (
        ";; Alist manuell definieren",
        ";; alist を手動で定義",
        ";; Programmatisch ein neues Paar hinzufügen",
        ";; プログラムで新しいペアを追加",
    ),
    "ko": (
        ";; Alist manuell definieren",
        ";; alist 수동 정의",
        ";; Programmatisch ein neues Paar hinzufügen",
        ";; 프로그램 방식으로 새 쌍 추가",
    ),
    "zh-cn": (
        ";; Alist manuell definieren",
        ";; 手动定义 alist",
        ";; Programmatisch ein neues Paar hinzufügen",
        ";; 以编程方式添加新对",
    ),
    "zh-tw": (
        ";; Alist manuell definieren",
        ";; 手動定義 alist",
        ";; Programmatisch ein neues Paar hinzufügen",
        ";; 以程式方式新增配對",
    ),
    "th": (
        ";; Alist manuell definieren",
        ";; กำหนด alist ด้วยตนเอง",
        ";; Programmatisch ein neues Paar hinzufügen",
        ";; เพิ่มคู่ใหม่แบบโปรแกรม",
    ),
    "uk": (
        ";; Alist manuell definieren",
        ";; Вручну визначити alist",
        ";; Programmatisch ein neues Paar hinzufügen",
        ";; Програмно додати нову пару",
    ),
    "vi": (
        ";; Alist manuell definieren",
        ";; Định nghĩa alist thủ công",
        ";; Programmatisch ein neues Paar hinzufügen",
        ";; Thêm cặp mới theo chương trình",
    ),
    "id": (
        ";; Alist manuell definieren",
        ";; Definisikan alist secara manual",
        ";; Programmatisch ein neues Paar hinzufügen",
        ";; Tambahkan pasangan baru secara programatis",
    ),
}

MESSAGE_CONSOLE = {
    "de": "Nachrichtenkonsole",
    "es": "consola de mensajes",
    "fr": "console de messages",
    "it": "console dei messaggi",
    "nl": "berichtenconsole",
    "pt": "consola de mensagens",
    "pt-br": "console de mensagens",
    "ru": "консоль сообщений",
    "sv": "meddelandekonsol",
    "pl": "konsola komunikatów",
    "uk": "консоль повідомлень",
    "ja": "メッセージコンソール",
    "ko": "메시지 콘솔",
    "zh-cn": "消息控制台",
    "zh-tw": "訊息主控台",
    "vi": "bảng điều khiển thông báo",
    "th": "คอนโซลข้อความ",
    "id": "konsol pesan",
    "ar": "وحدة تحكم الرسائل",
}

ERROR_CONSOLE = {
    "de": "Fehlerkonsole",
    "es": "consola de errores",
    "fr": "console d'erreurs",
    "it": "console degli errori",
    "nl": "foutenconsole",
    "pt": "consola de erros",
    "pt-br": "console de erros",
    "ru": "консоль ошибок",
    "sv": "felkonsol",
    "pl": "konsola błędów",
    "uk": "консоль помилок",
    "ja": "エラーコンソール",
    "ko": "오류 콘솔",
    "zh-cn": "错误控制台",
    "zh-tw": "錯誤主控台",
    "vi": "bảng điều khiển lỗi",
    "th": "คอนโซลข้อผิดพลาด",
    "id": "konsol kesalahan",
    "ar": "وحدة تحكم الأخطاء",
}

MOUSE_TOUCHPAD = {
    "nl": "Muis en touchpad → Grootte aanwijzer",
    "ru": "Мышь и сенсорная панель → Размер указателя",
    "sv": "Mus och pekplatta → Storlek på pekare",
    "pt-br": "Mouse e touchpad → Tamanho do ponteiro",
    "th": "เมาส์และทัชแพด → ปรับขนาดตัวชี้",
}

LUMI_O = re.compile(r"(?<![a-z-])Lumi-o(?![a-z])")
FM_RE = re.compile(r"^\ufeff?\s*---\r?\n[\s\S]*?\r?\n---\r?\n?", re.MULTILINE)

DOWNLOAD_FIXES: dict[str, list[tuple[str, str]]] = {
    "de": [
        ("2. Ziehen Sie den Reißverschluss heraus.", "2. Entpacken Sie die ZIP-Datei."),
    ],
    "es": [
        ("2. Extraiga la cremallera.", "2. Extraiga el archivo zip."),
    ],
    "nl": [
        (
            "Als je al Linux gebruikt en Lumi snel wilt gebruiken, gebruik dan de nieuwste **ontwikkeling AppImage** van GitLab-artefacten:",
            "Als je al Linux gebruikt en Lumi snel wilt gebruiken, gebruik dan de nieuwste **ontwikkel-AppImage** van GitLab-artefacten:",
        ),
        ("2. Pak de ritssluiting uit.", "2. Pak het zip-bestand uit."),
    ],
    "sv": [
        (
            "Om du redan är på Linux och vill köra Lumi snabbt, använd den senaste **utvecklingen AppImage** från GitLab-artefakter:",
            "Om du redan är på Linux och vill köra Lumi snabbt, använd den senaste **utvecklings-AppImage** från GitLab-artefakter:",
        ),
        ("2. Dra ut blixtlåset.", "2. Packa upp zip-filen."),
    ],
    "ru": [
        ("2. Распакуйте молнию.", "2. Распакуйте zip-архив."),
    ],
    "zh-cn": [
        ("1.下载最新开发AppImage神器zip。", "1. 下载最新的开发版 AppImage 构件 zip。"),
        ("2. 拉开拉链。", "2. 解压 zip 文件。"),
    ],
    "zh-tw": [
        ("1.下載最新開發AppImage神器zip。", "1. 下載最新的開發版 AppImage 構件 zip。"),
        ("2. 拉開拉鍊。", "2. 解壓 zip 檔案。"),
    ],
}

AR_UI_FIXES = [
    (
        "**Lumi > Edit > Preferences > Folders > Plug-ins**",
        "**Lumi > تحرير > التفضيلات > المجلدات > Plug-ins**",
    ),
    (
        "**Lumi -> Edit -> Preferences -> Folders -> Plug-ins**",
        "**Lumi -> تحرير -> التفضيلات -> المجلدات -> Plug-ins**",
    ),
    ("**Properties > Permissions**", "**الخصائص > الأذونات**"),
    (
        "**Properties -> Permissions -> Allow executing file as program**",
        "**الخصائص -> الأذونات -> السماح بتنفيذ الملف كبرنامج**",
    ),
    (
        "**Allow executing file as program**",
        "**السماح بتنفيذ الملف كبرنامج**",
    ),
]

ID_UI_FIXES = [
    ("**Lumi > Edit > Preferensi > Folder > Plug-in**", "**Lumi > Sunting > Preferensi > Folder > Plug-in**"),
    (
        "**Lumi -> Edit -> Preferensi -> Folder -> Plug-in**",
        "**Lumi -> Sunting -> Preferensi -> Folder -> Plug-in**",
    ),
]

TH_UI_FIXES = [
    ("**Properties > Permissions**", "**คุณสมบัติ > สิทธิ์**"),
    (
        "**Allow executing file as program**",
        "**อนุญาตให้เรียกใช้ไฟล์ในรูปแบบโปรแกรม**",
    ),
]

PT_ALIST_COMMENT = (
    ";; Define manualmente uma alist",
    ";; Definir manualmente uma alist",
)

WACOM_DRIVER_FIXES: dict[str, list[tuple[str, str]]] = {
    "fr": [
        (
            "en gardant le conducteur neutre",
            "en gardant le pilote neutre",
        ),
    ],
    "es": [
        (
            "Al mantener al conductor neutral",
            "al mantener el controlador neutro",
        ),
    ],
    "it": [
        (
            "Mantenendo il conducente neutrale",
            "mantenendo neutro il driver",
        ),
    ],
    "nl": [
        (
            "door de bestuurder neutraal te houden",
            "door de driver neutraal te houden",
        ),
        ("mondiale curve van Lumi", "globale curve van Lumi"),
    ],
    "sv": [
        (
            "genom att hålla föraren neutral",
            "genom att hålla drivrutinen neutral",
        ),
    ],
    "ru": [
        (
            "сохраняя нейтральность водителя",
            "сохраняя нейтральные настройки драйвера",
        ),
        ("кривой Люми", "кривой Lumi"),
    ],
    "zh-cn": [
        ("通过保持驾驶员中立", "通过保持驱动程序为中性设置"),
    ],
    "zh-tw": [
        ("透過保持駕駛員中立", "透過保持驅動程式為中性設定"),
    ],
    "th": [
        (
            "ด้วยการรักษาคนขับให้เป็นกลาง",
            "ด้วยการคงค่าไดรเวอร์ให้เป็นกลาง",
        ),
        ("เส้นโค้งทั่วโลกของ Lumi", "เส้นโค้งโดยรวมของ Lumi"),
    ],
}

LIST_REF_GLUE_FIXES = [
    (
        "- `(reverse (list 1 2 3))` retorna `(3 2 1)`#### Usando `list-ref`",
        "- `(reverse (list 1 2 3))` retorna `(3 2 1)`\n\n#### Usando `list-ref`",
    ),
    (
        "- `(reverse (list 1 2 3))` trả về `(3 2 1)`#### Sử dụng `list-ref`",
        "- `(reverse (list 1 2 3))` trả về `(3 2 1)`\n\n#### Sử dụng `list-ref`",
    ),
]

PROCEDURE_BROWSER_ZH_TW_FIX = (
    "程式瀏覽器中的 #### (lumi-message)\n\n搜尋`lumi-message`",
    "#### 程式瀏覽器中的 `(lumi-message)`\n\n搜尋 `lumi-message`",
)

RECURSION_LABELS = {
    "de": (
        ("**Base Condition:**", "**Basisbedingung:**"),
        ("**Base Result:**", "**Basisergebnis:**"),
        ("**Recursive Call:**", "**Rekursiver Aufruf:**"),
    ),
    "es": (
        ("**Base Condition:**", "**Condición base:**"),
        ("**Base Result:**", "**Resultado base:**"),
        ("**Recursive Call:**", "**Llamada recursiva:**"),
    ),
    "nl": (
        ("**Base Condition:**", "**Basisconditie:**"),
        ("**Base Result:**", "**Basisresultaat:**"),
        ("**Recursive Call:**", "**Recursieve aanroep:**"),
    ),
    "pt-br": (
        ("**Base Condition:**", "**Condição base:**"),
        ("**Base Result:**", "**Resultado base:**"),
        ("**Recursive Call:**", "**Chamada recursiva:**"),
    ),
}

ITERATION_LABELS = {
    "nl": (
        ("**Procedure:**", "**Functie:**"),
        ("**List:**", "**Lijst:**"),
    ),
    "pt-br": (
        ("**Procedure:**", "**Procedimento:**"),
        ("**List:**", "**Lista:**"),
    ),
}

WHEN_IT = """---
title: "when"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 8b6c15ef2763fe95100759e0e2e21f2bf43bf8424317be6d414f2b5260587714
url: "hub/scripting/fundamentals/Conditionals/conditionals-when"
---
In Scheme `if` è versatile, ma senza un ramo `else` esplicito può confondere — soprattutto quando deve essere eseguito solo il ramo vero, senza alternativa per il caso falso. In questi casi, `when` è più chiaro e conciso.

La forma base di `when`:

```scheme
(when test-is-true
  do-this
  do-that)
```

- Se `test` è vero (`#t`), tutte le espressioni nel corpo di `when` vengono eseguite in sequenza.
- Se `test` è falso (`#f`), non succede nulla e non viene restituito alcun valore.

### Esempio

```scheme
(when (< 0 1)
  (lumi-message "Condition is true!")
  (lumi-message "Executing additional actions."))
```

### Confronto tra `if` e `when`

Entrambi nello stesso esempio:

```scheme
(if (= 0 1)
  (lumi-message "This will not run")
  (when (< 0 1)
    (lumi-message "The 'when' condition is true!")
    (lumi-message "Executing multiple actions within 'when'.")))
```

#### Spiegazione

1. **Condizione `if`:**
   - Il test `(= 0 1)` verifica se 0 è uguale a 1.
   - Poiché è falso (`#f`), viene eseguito il ramo `else`.

2. **`when` nel ramo `else`:**
   - Il test `(< 0 1)` verifica se 0 è minore di 1.
   - Poiché è vero (`#t`), tutte le espressioni nel corpo di `when` vengono eseguite in sequenza.

#### Perché `when`?

- Evita un `else` vuoto o fittizio.
- Chiarisce che conta solo il ramo vero.

### Riepilogo

- Usa `if` quando servono entrambi i rami, vero e falso.
- Usa `when` quando c'è solo il ramo vero, soprattutto per più azioni.
- Combinare `if` e `when` aiuta a strutturare condizioni complesse in modo chiaro e conciso.
"""

WHEN_NL = """---
title: "when"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 8b6c15ef2763fe95100759e0e2e21f2bf43bf8424317be6d414f2b5260587714
url: "hub/scripting/fundamentals/Conditionals/conditionals-when"
---
In Scheme is `if` veelzijdig, maar zonder expliciete `else`-tak wordt het snel verwarrend — vooral wanneer alleen de ware tak moet worden uitgevoerd en er geen alternatief is voor het false-geval. In zo'n situatie is `when` duidelijker en compacter.

De basisvorm van `when`:

```scheme
(when test-is-true
  do-this
  do-that)
```

- Als `test` waar is (`#t`), worden alle expressies in de body van `when` achtereenvolgens uitgevoerd.
- Als `test` onwaar is (`#f`), gebeurt er niets en wordt geen waarde teruggegeven.

### Voorbeeld

```scheme
(when (< 0 1)
  (lumi-message "Condition is true!")
  (lumi-message "Executing additional actions."))
```

### `if` en `when` vergeleken

Beide in hetzelfde voorbeeld:

```scheme
(if (= 0 1)
  (lumi-message "This will not run")
  (when (< 0 1)
    (lumi-message "The 'when' condition is true!")
    (lumi-message "Executing additional actions within 'when'.")))
```

#### Uitleg

1. **`if`-voorwaarde:**
   - De test `(= 0 1)` controleert of 0 gelijk is aan 1.
   - Omdat dit onwaar is (`#f`), wordt de `else`-tak uitgevoerd.

2. **`when` in de `else`-tak:**
   - De test `(< 0 1)` controleert of 0 kleiner is dan 1.
   - Omdat dit waar is (`#t`), worden alle expressies in de body van `when` achtereenvolgens uitgevoerd.

#### Waarom `when`?

- Geen lege of dummy-`else` nodig.
- Maakt duidelijk dat alleen de ware tak relevant is.

### Samenvatting

- Gebruik `if` wanneer zowel de ware als de onware tak nodig is.
- Gebruik `when` wanneer er alleen een ware tak is, vooral bij meerdere acties.
- `if` en `when` combineren helpt complexe voorwaarden overzichtelijk te structureren.
"""

WHEN_PT_BR = """---
title: "when"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 8b6c15ef2763fe95100759e0e2e21f2bf43bf8424317be6d414f2b5260587714
url: "hub/scripting/fundamentals/Conditionals/conditionals-when"
---
No Scheme, `if` é versátil, mas sem um ramo `else` explícito pode confundir — especialmente quando só o ramo verdadeiro deve ser executado, sem ação alternativa para o caso falso. Nesses cenários, `when` é mais claro e conciso.

A forma básica de `when`:

```scheme
(when test-is-true
  do-this
  do-that)
```

- Se `test` for verdadeiro (`#t`), todas as expressões no corpo de `when` são executadas em sequência.
- Se `test` for falso (`#f`), nada acontece e nenhum valor é retornado.

### Exemplo

```scheme
(when (< 0 1)
  (lumi-message "Condition is true!")
  (lumi-message "Executing additional actions."))
```

### Comparando `if` e `when`

Os dois juntos no mesmo exemplo:

```scheme
(if (= 0 1)
  (lumi-message "This will not run")
  (when (< 0 1)
    (lumi-message "The 'when' condition is true!")
    (lumi-message "Executing additional actions within 'when'.")))
```

#### Explicação

1. **Condição do `if`:**
   - O teste `(= 0 1)` verifica se 0 é igual a 1.
   - Como é falso (`#f`), o ramo `else` é executado.

2. **`when` no ramo `else`:**
   - O teste `(< 0 1)` verifica se 0 é menor que 1.
   - Como é verdadeiro (`#t`), todas as expressões no corpo de `when` são executadas em sequência.

#### Por que `when`?

- Evita um `else` vazio ou fictício.
- Deixa claro que só o ramo verdadeiro importa.

### Resumo

- Use `if` quando ambos os ramos importam.
- Use `when` quando há apenas o ramo verdadeiro, especialmente com várias ações.
- Combinar `if` e `when` ajuda a estruturar condições complexas de forma clara.
"""


def locale_of(path: Path) -> str | None:
    parts = path.name.rsplit(".", 2)
    if len(parts) < 3 or parts[-1] != "md":
        return None
    return parts[-2]


def strip_front_matter(text: str) -> str:
    return FM_RE.sub("", text, count=1)


def source_for_target(target: Path, loc: str) -> Path | None:
    source = target.with_name(target.name[: -(len(loc) + 4)] + ".md")
    return source if source.exists() else None


def fix_file(path: Path, dry_run: bool) -> bool:
    loc = locale_of(path)
    if not loc:
        return False
    text = path.read_text(encoding="utf-8")
    original = text

    # Zero-width space — all locale files
    text = text.replace("\u200b", "")

    if path.name.startswith("wrapping.") and "Wrapping" in str(path):
        if loc in WRAPPING_CAR_HEADING:
            old, new = WRAPPING_CAR_HEADING[loc]
            text = text.replace(old, new)
        if loc in WRAPPING_RANDOM_SEED_HEADING:
            old, new = WRAPPING_RANDOM_SEED_HEADING[loc]
            text = text.replace(old, new)
        for pat, repl in WRAPPING_BODY_FIXES:
            text = pat.sub(repl, text)

    if path.name.startswith("alists.") and loc != "de" and loc in ALIST_COMMENT_PAIR:
        g_old, g_new, p_old, p_new = ALIST_COMMENT_PAIR[loc]
        text = text.replace(g_old, g_new).replace(p_old, p_new)

    if loc in MESSAGE_CONSOLE:
        text = text.replace("Message console", MESSAGE_CONSOLE[loc])
    if loc in ERROR_CONSOLE:
        text = text.replace("Error Console", ERROR_CONSOLE[loc])

    if loc in MOUSE_TOUCHPAD and "Installing-Debian" in str(path):
        text = text.replace(
            "Mouse and Touchpad → Pointer Size adjustment",
            MOUSE_TOUCHPAD[loc],
        )

    if loc in RECURSION_LABELS and path.name.startswith("recursion."):
        for old, new in RECURSION_LABELS[loc]:
            text = text.replace(old, new)

    if loc in ITERATION_LABELS and path.name in (f"map.{loc}.md", f"for-each.{loc}.md"):
        for old, new in ITERATION_LABELS[loc]:
            text = text.replace(old, new)

    if loc and "lumi-o" not in path.as_posix():
        source = source_for_target(path, loc)
        if source and LUMI_O.search(text):
            src_body = strip_front_matter(source.read_text(encoding="utf-8"))
            if not LUMI_O.search(src_body):
                fm_match = FM_RE.match(text)
                if fm_match:
                    front = fm_match.group(0)
                    body = text[len(front) :]
                    body = LUMI_O.sub("Lumi", body)
                    text = front + body
                else:
                    text = LUMI_O.sub("Lumi", text)

    if path.name.startswith("Download-and-Install.") and loc in DOWNLOAD_FIXES:
        for old, new in DOWNLOAD_FIXES[loc]:
            text = text.replace(old, new)

    if loc == "ar":
        for old, new in AR_UI_FIXES:
            text = text.replace(old, new)

    if loc == "id":
        for old, new in ID_UI_FIXES:
            text = text.replace(old, new)

    if loc == "th":
        for old, new in TH_UI_FIXES:
            text = text.replace(old, new)

    if path.name.startswith("alists.") and loc == "pt":
        old, new = PT_ALIST_COMMENT
        text = text.replace(old, new)

    if path.name.startswith("Wacom-Configuration.") and loc in WACOM_DRIVER_FIXES:
        for old, new in WACOM_DRIVER_FIXES[loc]:
            text = text.replace(old, new)

    if path.name.startswith("lists."):
        for old, new in LIST_REF_GLUE_FIXES:
            text = text.replace(old, new)

    if path.name == "the-procedure-browser.zh-tw.md":
        old, new = PROCEDURE_BROWSER_ZH_TW_FIX
        text = text.replace(old, new)

    if text != original:
        if not dry_run:
            path.write_text(text, encoding="utf-8")
        return True
    return False


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--dry-run", action="store_true")
    args = parser.parse_args()

    changed = []
    for path in sorted(CONTENT.rglob("*.*.md")):
        if fix_file(path, args.dry_run):
            changed.append(path)

    # Full rewrites for wrong-locale when pages
    when_writes = {
        CONTENT / "hub/scripting/fundamentals/Conditionals/conditionals-when.it.md": WHEN_IT,
        CONTENT / "hub/scripting/fundamentals/Conditionals/conditionals-when.nl.md": WHEN_NL,
        CONTENT / "hub/scripting/fundamentals/Conditionals/conditionals-when.pt-br.md": WHEN_PT_BR,
    }
    for path, body in when_writes.items():
        if not args.dry_run:
            path.write_text(body, encoding="utf-8")
        changed.append(path)

    print(f"{'Would change' if args.dry_run else 'Changed'} {len(changed)} files")
    for p in changed:
        print(f"  {p.relative_to(CONTENT.parent)}")


if __name__ == "__main__":
    main()
