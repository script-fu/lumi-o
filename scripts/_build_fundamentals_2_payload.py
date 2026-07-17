#!/usr/bin/env python3
"""Build _batch_fundamentals_2_payload.json with all page translations."""

from __future__ import annotations

import json
import textwrap
from pathlib import Path

OUT = Path(__file__).resolve().parent / "_batch_fundamentals_2_payload.json"
LANGS = ["de", "es", "fr", "it", "ja", "ko", "nl", "pl", "pt-br", "ru", "sv", "th", "zh-cn", "zh-tw"]

# --- shared code (exact English source) ---
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

COND_EX3 = {
    "de": textwrap.dedent("""\
```scheme
(cond
  ;; Fall 1: Wenn 0 kleiner als -1 ist
  ((< 0 -1)
    (let ((x 10))
      (* x x)))

  ;; Fall 2: Wenn 0 größer als -1 ist
  ((> 0 -1)
    (let ((y 20))
      (lumi-message "Positive condition met")
      (+ y y)))

  ;; Standardfall: Wenn keine der obigen Bedingungen zutrifft
  (else
    (let ((z 0))
      z)))
```"""),
    "fr": textwrap.dedent("""\
```scheme
(cond
  ;; Cas 1 : si 0 est inférieur à -1
  ((< 0 -1)
    (let ((x 10))
      (* x x)))

  ;; Cas 2 : si 0 est supérieur à -1
  ((> 0 -1)
    (let ((y 20))
      (lumi-message "Positive condition met")
      (+ y y)))

  ;; Cas par défaut : si aucune condition ci-dessus n'est remplie
  (else
    (let ((z 0))
      z)))
```"""),
}

COND_EX3_DEFAULT = textwrap.dedent("""\
```scheme
(cond
  ;; Case 1: If 0 is less than -1
  ((< 0 -1)
    (let ((x 10))
      (* x x)))

  ;; Case 2: If 0 is greater than -1
  ((> 0 -1)
    (let ((y 20))
      (lumi-message "Positive condition met")
      (+ y y)))

  ;; Default case: If none of the above conditions are met
  (else
    (let ((z 0))
      z)))
```""")

COND_EX4 = textwrap.dedent("""\
```scheme
(cond
  ((< 5 3) "This won't run")
  ((= 5 3) "This won't run either")
  (else "Fallback value"))
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

WHEN_FORM = IF_SIMPLE.replace("if test-is-true", "when test-is-true").replace("do-this)", "do-this\n  do-that)")

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

FOREACH_FORM = textwrap.dedent("""\
```scheme
(for-each procedure list)
```""")

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

MAP_FORM = FOREACH_FORM.replace("for-each", "map")
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

WHEN_FORM = textwrap.dedent("""\
```scheme
(when test-is-true
  do-this
  do-that)
```""")


def j(*parts: str) -> str:
    return "\n".join(parts)


def t(lang: str, table: dict[str, str], fallback: str = "fr") -> str:
    return table.get(lang, table[fallback])


# --- Page builders returning {lang: {title, body}} ---

def page_iter_index() -> dict[str, dict[str, str]]:
    titles = {
        "de": "Iteration", "es": "Iteración", "fr": "Itération", "it": "Iterazione",
        "ja": "反復", "ko": "반복", "nl": "Iteratie", "pl": "Iteracja",
        "pt-br": "Iteração", "ru": "Итерация", "sv": "Iteration",
        "th": "การวนซ้ำ", "zh-cn": "迭代", "zh-tw": "迭代",
    }
    intro = {
        "de": "Iteration ist ein Grundpfeiler der Programmierung: Sie ermöglicht Skripten, Aktionen zu wiederholen und Datensammlungen effizient zu verarbeiten. In Scheme, basierend auf der Programmiersprache Scheme, bietet Iteration die Werkzeuge, um repetitive Aufgaben zu automatisieren, Datenstrukturen zu bearbeiten und ausgefeilte Ausführungsmuster zu erzeugen.",
        "fr": "L'itération est une pierre angulaire de la programmation : elle permet aux scripts de répéter des actions et de traiter efficacement des collections de données. En Scheme, basé sur le langage Scheme, l'itération fournit les outils pour automatiser les tâches répétitives, manipuler des structures de données et créer des schémas d'exécution sophistiqués.",
        "es": "La iteración es un pilar de la programación: permite que los scripts repitan acciones y procesen colecciones de datos con eficiencia. En Scheme, basado en el lenguaje Scheme, la iteración ofrece las herramientas para automatizar tareas repetitivas, manipular estructuras de datos y crear patrones de ejecución sofisticados.",
        "it": "L'iterazione è un pilastro della programmazione: consente agli script di ripetere azioni e processare raccolte di dati in modo efficiente. In Scheme, basato sul linguaggio Scheme, l'iterazione fornisce gli strumenti per automatizzare compiti ripetitivi, manipolare strutture dati e creare pattern di esecuzione sofisticati.",
        "ja": "反復はプログラミングの基盤です。スクリプトが処理を繰り返し、データ集合を効率的に扱えるようにします。Scheme 言語に基づく Scheme では、反復により反復タスクの自動化、データ構造の操作、洗練された実行パターンの作成が可能になります。",
        "ko": "반복은 프로그래밍의 초석입니다. 스크립트가 작업을 반복하고 데이터 집합을 효율적으로 처리할 수 있게 합니다. Scheme 프로그래밍 언어에 기반한 Scheme에서 반복은 반복 작업 자동화, 데이터 구조 조작, 정교한 실행 패턴 생성 도구를 제공합니다.",
        "nl": "Iteratie is een hoeksteen van programmeren: het laat scripts acties herhalen en gegevensverzamelingen efficiënt verwerken. In Scheme, gebaseerd op de Scheme-taal, biedt iteratie de tools om repetitieve taken te automatiseren, datastructuren te manipuleren en verfijnde uitvoeringspatronen te creëren.",
        "pl": "Iteracja to fundament programowania: pozwala skryptom powtarzać działania i efektywnie przetwarzać zbiory danych. W Scheme, opartym na języku Scheme, iteracja dostarcza narzędzia do automatyzacji powtarzalnych zadań, manipulacji strukturami danych i tworzenia wyrafinowanych wzorców wykonania.",
        "pt-br": "Iteração é um pilar da programação: permite que scripts repitam ações e processem coleções de dados com eficiência. Em Scheme, baseado na linguagem Scheme, a iteração oferece ferramentas para automatizar tarefas repetitivas, manipular estruturas de dados e criar padrões de execução sofisticados.",
        "ru": "Итерация — краеугольный камень программирования: она позволяет сценариям повторять действия и эффективно обрабатывать коллекции данных. В Scheme, основанном на языке Scheme, итерация даёт инструменты для автоматизации повторяющихся задач, работы со структурами данных и создания сложных шаблонов выполнения.",
        "sv": "Iteration är en hörnsten i programmering: den låter skript upprepa åtgärder och bearbeta datasamlingar effektivt. I Scheme, baserat på programmespråket Scheme, ger iteration verktyg för att automatisera repetitiva uppgifter, manipulera datastrukturer och skapa sofistikerade körningsmönster.",
        "th": "การวนซ้ำเป็นหัวใจของการเขียนโปรแกรม: ช่วยให้สคริปต์ทำงานซ้ำและประมวลผลชุดข้อมูลได้อย่างมีประสิทธิภาพ ใน Scheme ซึ่งอิงจากภาษา Scheme การวนซ้ำให้เครื่องมือสำหรับ automatize งานซ้ำ จัดการโครงสร้างข้อมูล และสร้างรูปแบบการทำงานที่ซับซ้อน",
        "zh-cn": "迭代是编程的基石：它使脚本能够重复操作并高效处理数据集合。在基于 Scheme 编程语言的 Scheme 中，迭代提供了自动化重复任务、操作数据结构和创建复杂执行模式的工具。",
        "zh-tw": "迭代是程式設計的基石：它使腳本能夠重複操作並高效處理資料集合。在基於 Scheme 程式語言的 Scheme 中，迭代提供了自動化重複任務、操作資料結構和建立複雜執行模式的工具。",
    }
    role_h = {
        "de": "### Die Rolle der Iteration in Scheme", "fr": "### Le rôle de l'itération en Scheme",
        "es": "### El papel de la iteración en Scheme", "it": "### Il ruolo dell'iterazione in Scheme",
        "ja": "### Scheme における反復の役割", "ko": "### Scheme에서 반복의 역할",
        "nl": "### De rol van iteratie in Scheme", "pl": "### Rola iteracji w Scheme",
        "pt-br": "### O papel da iteração em Scheme", "ru": "### Роль итерации в Scheme",
        "sv": "### Iterationens roll i Scheme", "th": "### บทบาทของการวนซ้ำใน Scheme",
        "zh-cn": "### 迭代在 Scheme 中的作用", "zh-tw": "### 迭代在 Scheme 中的角色",
    }
    out = {}
    for lang in LANGS:
        body = j(
            t(lang, intro),
            "", t(lang, role_h), "",
            t(lang, {
                "de": "Iteration erfüllt in Ihren Skripten mehrere wesentliche Aufgaben:",
                "fr": "L'itération remplit plusieurs fonctions essentielles dans vos scripts :",
                "es": "La iteración cumple varias funciones esenciales en sus scripts:",
            }),
            t(lang, {
                "de": "- **Wiederholung automatisieren:** Dieselbe Aktion oder Aktionsfolge mehrfach ausführen, ohne Code zu duplizieren.\n- **Effizienz steigern:** Datenstrukturen iterativ verarbeiten, um große Operationen systematisch abzuwickeln.\n- **Code straffen:** Redundanz vermeiden und Code übersichtlicher, lesbarer und wartbarer machen.",
                "fr": "- **Automatiser la répétition :** Exécuter la même action ou la même séquence d'actions plusieurs fois sans dupliquer le code.\n- **Gagner en efficacité :** Traiter les structures de données de façon itérative pour gérer des opérations à grande échelle.\n- **Alléger le code :** Éliminer la redondance et rendre le code plus concis, lisible et maintenable.",
                "es": "- **Automatizar la repetición:** Realizar la misma acción o secuencia de acciones varias veces sin duplicar código.\n- **Mejorar la eficiencia:** Procesar estructuras de datos de forma iterativa para operaciones a gran escala.\n- **Agilizar el código:** Eliminar redundancia y hacer el código más conciso, legible y mantenible.",
            }),
            "", t(lang, {
                "de": "### Verfügbare Iterationskonstrukte", "fr": "### Types d'itération disponibles",
                "es": "### Tipos de iteración disponibles",
            }), "",
            t(lang, {
                "de": "Scheme bietet mehrere Konstrukte für Iteration, jeweils für bestimmte Anforderungen:\n- **map:** Wendet eine Funktion auf jedes Listenelement an und gibt eine neue Liste mit den Ergebnissen zurück.\n- **for-each:** Ähnlich wie `map`, führt aber eine Funktion auf jedem Element aus, ohne ein Ergebnis zurückzugeben.\n- **do:** Allgemeine Schleife für vielfältige iterative Prozesse.\n- **recursion:** Leistungsstarke Technik, bei der Funktionen sich selbst aufrufen, um Probleme schrittweise zu lösen.",
                "fr": "Scheme propose plusieurs constructions d'itération, chacune adaptée à des besoins précis :\n- **map :** Applique une fonction à chaque élément d'une liste et renvoie une nouvelle liste de résultats.\n- **for-each :** Semblable à `map`, mais exécute une fonction sur chaque élément sans renvoyer de résultat.\n- **do :** Boucle générale pour une large variété de processus itératifs.\n- **recursion :** Technique puissante où les fonctions s'appellent elles-mêmes pour résoudre un problème par étapes.",
                "es": "Scheme ofrece varios constructos de iteración, cada uno adaptado a necesidades específicas:\n- **map:** Aplica una función a cada elemento de una lista y devuelve una nueva lista con los resultados.\n- **for-each:** Similar a `map`, pero ejecuta una función en cada elemento sin devolver un resultado.\n- **do:** Bucle de propósito general para procesos iterativos variados.\n- **recursion:** Técnica potente en la que las funciones se llaman a sí mismas para resolver problemas incrementalmente.",
            }),
            "", t(lang, {
                "de": "### So funktioniert Iteration", "fr": "### Comment fonctionne l'itération",
                "es": "### Cómo funciona la iteración",
            }), "",
            t(lang, {
                "de": "Iteration umfasst typischerweise:\n1. **Wiederholung definieren:** Festlegen, welche Aktion wiederholt wird und welche Daten oder welcher Bereich verarbeitet wird.\n2. **Sequenziell ausführen:** Die Aktion für jedes Element, jeden Schritt oder jede Bedingung wiederholen, bis die Iteration abgeschlossen ist.\n3. **Ergebnis zurückgeben (optional):** Je nach Konstrukt kann Iteration ein Ergebnis liefern oder Zustand ändern.\n\nDiese Konstrukte helfen Ihnen, anpassungsfähige, effiziente und elegante Skripte für komplexe Aufgaben zu schreiben.",
                "fr": "L'itération implique généralement :\n1. **Définir une répétition :** Spécifier l'action à répéter et les données ou la plage à traiter.\n2. **Exécuter en séquence :** Répéter l'action pour chaque élément, pas ou condition jusqu'à completion.\n3. **Renvoyer un résultat (facultatif) :** Selon la construction, l'itération peut produire un résultat ou modifier un état.\n\nCes constructions vous aident à écrire des scripts adaptables, efficaces et élégants pour des tâches complexes.",
                "es": "La iteración suele implicar:\n1. **Definir una repetición:** Especificar la acción a repetir y los datos o el rango a procesar.\n2. **Ejecutar en secuencia:** Repetir la acción para cada elemento, paso o condición hasta completar.\n3. **Devolver un resultado (opcional):** Según el constructo, la iteración puede devolver un resultado o modificar el estado.\n\nEstos constructos le ayudan a escribir scripts adaptables, eficientes y elegantes para tareas complejas.",
            }),
        )
        out[lang] = {"title": titles[lang], "body": body}
    return out


PAYLOAD: dict[str, dict[str, dict[str, str]]] = {
    "content/hub/scripting/fundamentals/Iteration/_index.md": page_iter_index(),
}

OUT.write_text(json.dumps(PAYLOAD, ensure_ascii=False, indent=2), encoding="utf-8")
print(f"Wrote {OUT} with {sum(len(v) for v in PAYLOAD.values())} entries (partial — extend script)")
