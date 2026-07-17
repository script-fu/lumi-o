#!/usr/bin/env python3
"""Generate _batch_fundamentals_2_prose.json — all Conditionals & Iteration pages."""

from __future__ import annotations

import importlib.util
import json
import textwrap
from pathlib import Path

SCRIPT_DIR = Path(__file__).resolve().parent
OUT = SCRIPT_DIR / "_batch_fundamentals_2_prose.json"
LANGS = ["de", "es", "fr", "it", "ja", "ko", "nl", "pl", "pt-br", "ru", "sv", "th", "zh-cn", "zh-tw"]

spec = importlib.util.spec_from_file_location("data", SCRIPT_DIR / "_batch_fundamentals_2_data.py")
data = importlib.util.module_from_spec(spec)
spec.loader.exec_module(data)  # type: ignore[union-attr]


def t(table: dict[str, str], lang: str, fb: str = "fr") -> str:
    return table.get(lang, table[fb])


def j(*parts: str) -> str:
    return "\n".join(parts)


# --- shared code blocks ---
MAP_FORM = "```scheme\n(map procedure list)\n```"
MAP_DOUBLE = textwrap.dedent("""\
```scheme
(define (double x)
  (* x 2))

(map double (list 1 2 3 4))
```""")
MAP_MULTI = textwrap.dedent("""\
```scheme
(define (sum x y)
  (+ x y))

(map sum (list 1 2 3) (list 4 5 6))
```""")

FOREACH_FORM = "```scheme\n(for-each procedure list)\n```"
FOREACH_PRINT = textwrap.dedent("""\
```scheme
(define (print-item x)
  (lumi-message (number->string x)))

(for-each print-item (list 1 2 3 4))
```""")
FOREACH_MULTI = textwrap.dedent("""\
```scheme
(define (sum-and-print x y)
  (lumi-message (number->string (+ x y))))

(for-each sum-and-print (list 1 2 3) (list 4 5 6))
```""")

DO_FORM = textwrap.dedent("""\
```scheme
(do ((var1 init1 update1)
     (var2 init2 update2)
     (var3 init3 update3))
    (termination-condition result)
  body)
```""")
DO_SUM = textwrap.dedent("""\
```scheme
(do ((i 1 (+ i 1))      ; Initialize i to 1, increment by 1
     (sum 0 (+ sum i))) ; Initialize sum to 0, add i to sum
    ((> i 5) sum)       ; Terminate when i > 5, return sum
  (lumi-message (number->string sum))) ; Print sum at each step
```""")

WHEN_FORM = textwrap.dedent("""\
```scheme
(when test-is-true
  do-this
  do-that)
```""")
WHEN_EX = textwrap.dedent("""\
```scheme
(when (< 0 1)
  (lumi-message "Condition is true!")
  (lumi-message "Executing additional actions."))
```""")
WHEN_CONTRAST = textwrap.dedent("""\
```scheme
(if (= 0 1)
  (lumi-message "This will not run")
  (when (< 0 1)
    (lumi-message "The 'when' condition is true!")
    (lumi-message "Executing multiple actions within 'when'.")))
```""")

IF_SIMPLE = textwrap.dedent("""\
```scheme
(if test-is-true
  do-this)
```""")
IF_EX1 = textwrap.dedent("""\
```scheme
(if (< 0 1)
  (lumi-message "True!"))
```""")
IF_ELSE = textwrap.dedent("""\
```scheme
(if test
  do-this
  else-do-this)
```""")
IF_ELSE2 = textwrap.dedent("""\
```scheme
(if test
  consequent
  alternative)
```""")
IF_RET = textwrap.dedent("""\
```scheme
(if (< 0 1)
  1
  0)
```""")
IF_BEGIN = textwrap.dedent("""\
```scheme
(if (= 0 1)
  (begin
    (lumi-message "This won't run")
    1)
  (begin
    (lumi-message "False condition met, calculating...")
    (* 3 4)))
```""")
IF_LET = textwrap.dedent("""\
```scheme
(if (= 1 1)
  (let (x -1)
    (lumi-message "True condition met, calculating...")
    (* x 10))
  (let (y 4)
    (lumi-message "This won't run")
    (* 3 y)))
```""")

COND_SYNTAX = textwrap.dedent("""\
```scheme
(cond
  (test-1 consequent-1)
  (test-2 consequent-2)
  ...
  (else fallback-consequent))
```""")
COND_EX1 = textwrap.dedent("""\
```scheme
(cond
  ((< 3 2) "This won't run")
  ((= 3 3) "This will run")
  (else "Fallback"))
```""")
COND_EX2 = textwrap.dedent("""\
```scheme
(cond
  ((< 5 3)
    (begin
      (lumi-message "This won't run")
      (* 2 3)))
  ((> 5 3)
    (begin
      (lumi-message "Condition met")
      (* 5 5)))
  (else
    (begin
      (lumi-message "Fallback")
      0)))
```""")
COND_EX4 = textwrap.dedent("""\
```scheme
(cond
  ((< 5 3) "This won't run")
  ((= 5 3) "This won't run either")
  (else "Fallback value"))
```""")

REC_STRUCT = textwrap.dedent("""\
```scheme
(define (function-name args)
  (if (base-condition)
    base-result
    (recursive-call)))
```""")
REC_SUM = textwrap.dedent("""\
```scheme
(define (sum-to-n n)
  (if (= n 0)                  ; Base case: stop when n is 0
    0                          ; Base result: sum is 0
    (+ n (sum-to-n (- n 1))))) ; Recursive call: sum current n with result of smaller problem
```""")
REC_PRINT = textwrap.dedent("""\
```scheme
(define (print-elements lst)
  (if (null? lst)
    (lumi-message "done")
    (begin
      (lumi-message (number->string (car lst))) ; Print the first element
      (print-elements (cdr lst)))))             ; Process the rest of the list
```""")
REC_USAGE = textwrap.dedent("""\
```scheme
(print-elements (list 1 2 3))
```""")


def cond_ex3(lang: str) -> str:
    comments = {
        "de": (";; Fall 1: Wenn 0 kleiner als -1 ist", ";; Fall 2: Wenn 0 größer als -1 ist", ";; Standardfall: Wenn keine der obigen Bedingungen zutrifft"),
        "fr": (";; Cas 1 : si 0 est inférieur à -1", ";; Cas 2 : si 0 est supérieur à -1", ";; Cas par défaut : si aucune condition ci-dessus n'est remplie"),
        "es": (";; Caso 1: si 0 es menor que -1", ";; Caso 2: si 0 es mayor que -1", ";; Caso por defecto: si ninguna condición anterior se cumple"),
        "it": (";; Caso 1: se 0 è minore di -1", ";; Caso 2: se 0 è maggiore di -1", ";; Caso predefinito: se nessuna condizione sopra è soddisfatta"),
        "ja": (";; ケース 1: 0 が -1 より小さい場合", ";; ケース 2: 0 が -1 より大きい場合", ";; デフォルト: 上記の条件がいずれも満たされない場合"),
        "ko": (";; 경우 1: 0이 -1보다 작을 때", ";; 경우 2: 0이 -1보다 클 때", ";; 기본: 위 조건이 모두 거짓일 때"),
        "nl": (";; Geval 1: als 0 kleiner is dan -1", ";; Geval 2: als 0 groter is dan -1", ";; Standaard: als geen van bovenstaande voorwaarden geldt"),
        "pl": (";; Przypadek 1: gdy 0 jest mniejsze niż -1", ";; Przypadek 2: gdy 0 jest większe niż -1", ";; Domyślnie: gdy żaden z powyższych warunków nie jest spełniony"),
        "pt-br": (";; Caso 1: se 0 for menor que -1", ";; Caso 2: se 0 for maior que -1", ";; Padrão: se nenhuma condição acima for atendida"),
        "ru": (";; Случай 1: если 0 меньше -1", ";; Случай 2: если 0 больше -1", ";; По умолчанию: если ни одно из условий выше не выполнено"),
        "sv": (";; Fall 1: om 0 är mindre än -1", ";; Fall 2: om 0 är större än -1", ";; Standard: om inget av villkoren ovan uppfylls"),
        "th": (";; กรณี 1: ถ้า 0 น้อยกว่า -1", ";; กรณี 2: ถ้า 0 มากกว่า -1", ";; ค่าเริ่มต้น: ถ้าไม่มีเงื่อนไขข้างต้นเป็นจริง"),
        "zh-cn": (";; 情况 1：若 0 小于 -1", ";; 情况 2：若 0 大于 -1", ";; 默认：若以上条件均不满足"),
        "zh-tw": (";; 情況 1：若 0 小於 -1", ";; 情況 2：若 0 大於 -1", ";; 預設：若以上條件均不滿足"),
    }
    c1, c2, c3 = t(comments, lang).split("\n") if False else comments.get(lang, comments["fr"])
    c1, c2, c3 = comments.get(lang, comments["fr"])
    return textwrap.dedent(f"""\
```scheme
(cond
  {c1}
  ((< 0 -1)
    (let ((x 10))
      (* x x)))

  {c2}
  ((> 0 -1)
    (let ((y 20))
      (lumi-message "Positive condition met")
      (+ y y)))

  {c3}
  (else
    (let ((z 0))
      z)))
```""")


def build_map(lang: str) -> dict[str, str]:
    intro = {
        "de": "Die Funktion `map` in Scheme wendet eine Prozedur auf jedes Element einer Liste (oder mehrerer Listen) an und **gibt eine neue Liste** mit den Ergebnissen zurück. Ideal für Datentransformationen.",
        "es": "La función `map` en Scheme aplica un procedimiento a cada elemento de una lista (o varias listas) y **devuelve una nueva lista** con los resultados. Ideal para transformar datos.",
        "fr": "La fonction `map` en Scheme applique une procédure à chaque élément d'une liste (ou de plusieurs listes) et **renvoie une nouvelle liste** contenant les résultats. Idéale pour transformer des données.",
        "it": "La funzione `map` in Scheme applica una procedura a ogni elemento di una lista (o più liste) e **restituisce una nuova lista** con i risultati. È ideale per trasformare i dati.",
        "ja": "Scheme の `map` 関数は、リスト（または複数のリスト）の各要素に手続きを適用し、結果を含む**新しいリスト**を返します。データ変換に最適です。",
        "ko": "Scheme의 `map` 함수는 리스트(또는 여러 리스트)의 각 요소에 프로시저를 적용하고 결과가 담긴 **새 리스트**를 반환합니다. 데이터 변환에 이상적입니다.",
        "nl": "De functie `map` in Scheme past een procedure toe op elk element van een lijst (of meerdere lijsten) en **geeft een nieuwe lijst** met de resultaten terug. Ideaal voor datatransformaties.",
        "pl": "Funkcja `map` w Scheme stosuje procedurę do każdego elementu listy (lub wielu list) i **zwraca nową listę** z wynikami. Idealna do transformacji danych.",
        "pt-br": "A função `map` em Scheme aplica um procedimento a cada elemento de uma lista (ou várias listas) e **retorna uma nova lista** com os resultados. É ideal para transformar dados.",
        "ru": "Функция `map` в Scheme применяет процедуру к каждому элементу списка (или нескольких списков) и **возвращает новый список** с результатами. Идеальна для преобразования данных.",
        "sv": "Funktionen `map` i Scheme tillämpar en procedur på varje element i en lista (eller flera listor) och **returnerar en ny lista** med resultaten. Idealisk för datatransformation.",
        "th": "ฟังก์ชัน `map` ใน Scheme ใช้ procedure กับแต่ละองค์ประกอบของ list (หรือหลาย list) และ**คืน list ใหม่**ที่มีผลลัพธ์ เหมาะสำหรับการแปลงข้อมูล",
        "zh-cn": "Scheme 中的 `map` 函数对列表（或多个列表）的每个元素应用一个过程，并**返回包含结果的新列表**。非常适合数据转换。",
        "zh-tw": "Scheme 中的 `map` 函數對列表（或多個列表）的每個元素套用一個程序，並**回傳包含結果的新列表**。非常適合資料轉換。",
    }
    body = j(
        t(intro, lang),
        "", t({"de": "Die einfachste Form von `map` sieht so aus:", "fr": "La forme la plus simple de `map` ressemble à ceci :", "es": "La forma más simple de `map` es:"}, lang), "",
        MAP_FORM, "",
        t({"de": "- **Prozedur:** Funktion für jedes Listenelement.\n- **Liste:** Zu transformierende Liste.",
           "fr": "- **Procédure :** Fonction appliquée à chaque élément.\n- **Liste :** Liste à transformer.",
           "es": "- **Procedimiento:** Función por elemento.\n- **Lista:** Lista a transformar."}, lang),
        "", "---", "",
        t({"de": "### Beispiel: Elemente verdoppeln", "fr": "### Exemple : doubler chaque élément", "es": "### Ejemplo: duplicar cada elemento"}, lang), "",
        MAP_DOUBLE, "",
        t({"de": "- `double` wird auf `(1 2 3 4)` angewendet.\n- Ergebnis: neue Liste mit verdoppelten Werten.",
           "fr": "- `double` est appliqué à `(1 2 3 4)`.\n- Résultat : nouvelle liste avec valeurs doublées.",
           "es": "- `double` se aplica a `(1 2 3 4)`.\n- Resultado: nueva lista duplicada."}, lang),
        "", t({"de": "**Ausgabe**: `(2 4 6 8)`", "fr": "**Sortie** : `(2 4 6 8)`", "es": "**Salida**: `(2 4 6 8)`"}, lang),
        "", "---", "", t({"de": "### So funktioniert es", "fr": "### Comment ça marche", "es": "### Cómo funciona"}, lang), "",
        t({"de": "1. **Neue Liste:** `map` sammelt Ergebnisse in einer neuen Liste.\n2. **Transformation:** Für Datentransformation, nicht Nebeneffekte.",
           "fr": "1. **Nouvelle liste :** `map` collecte les résultats.\n2. **Transformation :** Plutôt que des effets de bord.",
           "es": "1. **Nueva lista:** `map` recopila resultados.\n2. **Transformación:** Más que efectos secundarios."}, lang),
        "", "---", "", t({"de": "#### Mehrere Listen", "fr": "#### Plusieurs listes", "es": "#### Varias listas"}, lang), "",
        t({"de": "Mit mehreren Listen verarbeitet `map` Elemente paarweise.", "fr": "Avec plusieurs listes, `map` traite les éléments correspondants.", "es": "Con varias listas, `map` procesa elementos correspondientes."}, lang), "",
        MAP_MULTI, "",
        t({"de": "**Ausgabe**: `(5 7 9)`", "fr": "**Sortie** : `(5 7 9)`", "es": "**Salida**: `(5 7 9)`"}, lang),
        "", "---", "", t({"de": "### Zusammenfassung", "fr": "### Résumé", "es": "### Resumen"}, lang), "",
        t({"de": "- `map` transformiert Listen elementweise.\n- Im Gegensatz zu `for-each` **erzeugt `map` eine neue Liste**.\n- Mehrere Listen werden paarweise verarbeitet.\n\nMit `map` erstellen Sie transformierte Datensätze, während Originallisten unverändert bleiben.",
           "fr": "- `map` transforme des listes élément par élément.\n- Contrairement à `for-each`, `map` **produit une nouvelle liste**.\n- Plusieurs listes sont traitées par paires.\n\nAvec `map`, créez des versions transformées tout en conservant les listes d'origine.",
           "es": "- `map` transforma listas elemento a elemento.\n- A diferencia de `for-each`, `map` **produce una nueva lista**.\n- Varias listas se procesan por pares.\n\nCon `map`, cree versiones transformadas manteniendo las listas originales."}, lang),
    )
    return {"title": "map", "body": body}


# NOTE: Additional page builders (for-each, do, when, if, cond, recursion, iter_index)
# are loaded from companion module when present.
try:
    from _batch_fundamentals_2_paragraphs import EXTRA_BUILDERS  # noqa: E402

    BUILDERS = {
        "content/hub/scripting/fundamentals/Conditionals/_index.md": lambda lang: {"title": data.cond_index(lang)[0], "body": data.cond_index(lang)[1]},
        "content/hub/scripting/fundamentals/Iteration/map.md": build_map,
    }
    BUILDERS.update(EXTRA_BUILDERS)
except ImportError:
    BUILDERS = {
        "content/hub/scripting/fundamentals/Conditionals/_index.md": lambda lang: {"title": data.cond_index(lang)[0], "body": data.cond_index(lang)[1]},
        "content/hub/scripting/fundamentals/Iteration/map.md": build_map,
    }


def main() -> None:
    prose: dict[str, dict[str, dict[str, str]]] = {}
    for rel, builder in BUILDERS.items():
        prose[rel] = {lang: builder(lang) for lang in LANGS}
    OUT.write_text(json.dumps(prose, ensure_ascii=False, indent=2), encoding="utf-8")
    print(f"Wrote {OUT} ({len(prose)} pages, {sum(len(v) for v in prose.values())} entries)")


if __name__ == "__main__":
    main()
