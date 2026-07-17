#!/usr/bin/env python3
"""Generate complete prose JSON and run batch pipeline."""

from __future__ import annotations

import importlib.util
import json
import subprocess
import sys
import textwrap
from pathlib import Path

SCRIPT_DIR = Path(__file__).resolve().parent
LANGS = ["de", "es", "fr", "it", "ja", "ko", "nl", "pl", "pt-br", "ru", "sv", "th", "zh-cn", "zh-tw"]

spec = importlib.util.spec_from_file_location("data", SCRIPT_DIR / "_batch_fundamentals_2_data.py")
data = importlib.util.module_from_spec(spec)
spec.loader.exec_module(data)  # type: ignore[union-attr]

# Import shared builders/code from generate module
sys.path.insert(0, str(SCRIPT_DIR))
import _generate_f2_prose as g  # noqa: E402


def meta(title: str, body: str) -> dict[str, str]:
    return {"title": title, "body": body}


def build_for_each(lang: str) -> dict[str, str]:
    body = g.j(
        g.t({"de": "Die Funktion `for-each` in Scheme wendet eine Prozedur auf jedes Element einer Liste (oder mehrerer Listen) an. Im Gegensatz zu `map`, das eine neue Liste zurückgibt, dient `for-each` **Nebeneffekten** wie Ausgabe oder Variablenänderung.",
             "fr": "La fonction `for-each` en Scheme applique une procédure à chaque élément d'une liste (ou de plusieurs listes). Contrairement à `map`, qui renvoie une nouvelle liste, `for-each` sert aux **effets de bord** : affichage, journalisation ou modification de variables.",
             "es": "La función `for-each` en Scheme aplica un procedimiento a cada elemento de una lista (o varias listas). A diferencia de `map`, que devuelve una nueva lista, `for-each` se usa por sus **efectos secundarios**: imprimir, registrar o modificar variables."}, lang),
        "", g.t({"de": "Die einfachste Form von `for-each`:", "fr": "La forme la plus simple de `for-each` :", "es": "La forma más simple de `for-each`:"}, lang), "",
        g.FOREACH_FORM, "",
        g.t({"de": "- **Prozedur:** Funktion pro Element.\n- **Liste:** Zu verarbeitende Liste.", "fr": "- **Procédure :** Fonction par élément.\n- **Liste :** Liste à parcourir.", "es": "- **Procedimiento:** Función por elemento.\n- **Lista:** Lista a procesar."}, lang),
        "", "---", "", g.t({"de": "### Beispiel: Liste ausgeben", "fr": "### Exemple : afficher une liste", "es": "### Ejemplo: imprimir una lista"}, lang), "",
        g.FOREACH_PRINT, "",
        g.t({"de": "- `print-item` wird auf `(1 2 3 4)` angewendet.\n- Jede Zahl wird nacheinander ausgegeben.", "fr": "- `print-item` est appliqué à `(1 2 3 4)`.\n- Chaque nombre est affiché successivement.", "es": "- `print-item` se aplica a `(1 2 3 4)`.\n- Cada número se imprime en secuencia."}, lang),
        "", g.t({"de": "**Ausgabe**: `1 2 3 4`", "fr": "**Sortie** : `1 2 3 4`", "es": "**Salida**: `1 2 3 4`"}, lang),
        "", "---", "", g.t({"de": "### So funktioniert es", "fr": "### Comment ça marche", "es": "### Cómo funciona"}, lang), "",
        g.t({"de": "1. **Über jedes Element iterieren:** Die Prozedur wird der Reihe nach ausgeführt.\n2. **Nebeneffekte:** Drucken, Protokollieren oder externe Variablen ändern — ohne neue Liste.", "fr": "1. **Parcourir chaque élément :** La procédure s'exécute dans l'ordre.\n2. **Effets de bord :** Affichage, journalisation ou modification — sans nouvelle liste.", "es": "1. **Iterar cada elemento:** El procedimiento se ejecuta en orden.\n2. **Efectos secundarios:** Imprimir o modificar estado — sin nueva lista."}, lang),
        "", "---", "", g.t({"de": "#### Mehrere Listen", "fr": "#### Plusieurs listes", "es": "#### Varias listas"}, lang), "",
        g.t({"de": "Mit mehreren Listen verarbeitet `for-each` entsprechende Elemente paarweise.", "fr": "Avec plusieurs listes, `for-each` traite les éléments correspondants.", "es": "Con varias listas, `for-each` procesa elementos correspondientes."}, lang), "",
        g.FOREACH_MULTI, "",
        g.t({"de": "**Ausgabe**: `5 7 9`", "fr": "**Sortie** : `5 7 9`", "es": "**Salida**: `5 7 9`"}, lang),
        "", "---", "", g.t({"de": "### Zusammenfassung", "fr": "### Résumé", "es": "### Resumen"}, lang), "",
        g.t({"de": "- `for-each` eignet sich für Nebeneffekte pro Listenelement.\n- Im Gegensatz zu `map` **keine neue Liste**.\n- Mehrere Listen werden gleichzeitig verarbeitet.\n\nNutzen Sie `for-each`, wenn Aktionen wichtiger sind als Datentransformation.", "fr": "- `for-each` convient aux effets de bord sur chaque élément.\n- Contrairement à `map`, **pas de nouvelle liste**.\n- Plusieurs listes simultanément.\n\nUtilisez `for-each` lorsque l'action prime sur la transformation.", "es": "- `for-each` sirve para efectos secundarios por elemento.\n- A diferencia de `map`, **sin nueva lista**.\n- Varias listas a la vez.\n\nUse `for-each` cuando la acción importa más que transformar datos."}, lang),
    )
    return meta("for-each", body)


def build_do(lang: str) -> dict[str, str]:
    body = g.j(
        g.t({"de": "Die Funktion `do` in Scheme ist eine Schleife mit Initialisierung, Aktualisierung und Abbruchbedingung. Nützlich, wenn eine Sequenz eine bestimmte Anzahl von Malen oder bis zu einer Bedingung laufen soll.",
             "fr": "La fonction `do` en Scheme est une boucle avec initialisation, mise à jour et condition d'arrêt. Utile pour exécuter une séquence un nombre défini de fois ou jusqu'à une condition.",
             "es": "La función `do` en Scheme es un bucle con inicialización, actualización y condición de terminación. Útil para ejecutar una secuencia un número de veces o hasta cumplir una condición."}, lang),
        "", g.t({"de": "Die allgemeine Form von `do`:", "fr": "La forme générale de `do` :", "es": "La forma general de `do`:"}, lang), "",
        g.DO_FORM, "",
        g.t({"de": "- **Variable:** Schleifenvariable(n).\n- **Initial-value:** Startwert.\n- **Update-expression:** Aktualisierung pro Iteration.\n- **Termination-condition:** Abbruchbedingung.\n- **Result-expression:** Rückgabewert beim Abbruch.\n- **Body:** Code pro Iteration.",
             "fr": "- **Variable :** variable(s) de boucle.\n- **Initial-value :** valeur initiale.\n- **Update-expression :** mise à jour par itération.\n- **Termination-condition :** condition d'arrêt.\n- **Result-expression :** valeur renvoyée à l'arrêt.\n- **Body :** code exécuté à chaque tour.", "es": "- **Variable:** variable(s) del bucle.\n- **Initial-value:** valor inicial.\n- **Update-expression:** actualización por iteración.\n- **Termination-condition:** condición de parada.\n- **Result-expression:** valor al terminar.\n- **Body:** código por iteración."}, lang),
        "", "---", "", g.t({"de": "### Beispiel: Summe von 1 bis 5", "fr": "### Exemple : somme de 1 à 5", "es": "### Ejemplo: suma del 1 al 5"}, lang), "",
        g.DO_SUM, "",
        g.t({"de": "- `i` startet bei 1 und wird inkrementiert.\n- `sum` akkumuliert die Summe.\n- Abbruch bei `i > 5`, Rückgabe von `sum`.", "fr": "- `i` commence à 1 et s'incrémente.\n- `sum` accumule la somme.\n- Arrêt quand `i > 5`, retour de `sum`.", "es": "- `i` empieza en 1 e incrementa.\n- `sum` acumula la suma.\n- Parada cuando `i > 5`, retorno de `sum`."}, lang),
        "", g.t({"de": "**Ausgabe**: `15`", "fr": "**Sortie** : `15`", "es": "**Salida**: `15`"}, lang),
        "", "---", "", g.t({"de": "### So funktioniert es", "fr": "### Comment ça marche", "es": "### Cómo funciona"}, lang), "",
        g.t({"de": "1. **Initialisierung:** Startwerte zuweisen.\n2. **Abbruchprüfung:** Zu Beginn jeder Iteration.\n3. **Iteration:** Body ausführen, Variablen aktualisieren.", "fr": "1. **Initialisation :** valeurs de départ.\n2. **Test d'arrêt :** au début de chaque tour.\n3. **Itération :** exécuter le corps, mettre à jour les variables.", "es": "1. **Inicialización:** valores iniciales.\n2. **Comprobación de parada:** al inicio de cada vuelta.\n3. **Iteración:** ejecutar cuerpo, actualizar variables."}, lang),
        "", "---", "", g.t({"de": "### Zusammenfassung", "fr": "### Résumé", "es": "### Resumen"}, lang), "",
        g.t({"de": "- `do` bietet flexible Schleifen mit mehreren Variablen.\n- Nützlich bei Zustandsänderungen über Iterationen.\n- Die Abbruchbedingung bestimmt Ende und Ergebnis.\n\n`do` verbindet **gebundene Variablen** (wie `let`) mit **iterativer Steuerung**.", "fr": "- `do` offre des boucles flexibles à plusieurs variables.\n- Utile quand l'état évolue à chaque tour.\n- La condition d'arrêt fixe la fin et le résultat.\n\n`do` combine **liaisons** (comme `let`) et **contrôle itératif**.", "es": "- `do` ofrece bucles flexibles con varias variables.\n- Útil cuando el estado cambia en cada vuelta.\n- La condición de parada fija el fin y el resultado.\n\n`do` combina **enlaces** (como `let`) y **control iterativo**."}, lang),
    )
    return meta("do", body)


def build_when(lang: str) -> dict[str, str]:
    body = g.j(
        g.t({"de": "In Scheme ist `if` vielseitig, wird aber ohne explizites `else` schnell unübersichtlich — besonders wenn nur der wahre Zweig ausgeführt werden soll. Dann ist `when` klarer und kompakter.",
             "fr": "En Scheme, `if` est polyvalent, mais sans `else` explicite il devient vite confus — surtout quand seule la branche vraie doit s'exécuter. Dans ce cas, `when` est plus clair et concis.",
             "es": "En Scheme, `if` es versátil, pero sin un `else` explícito puede confundir — sobre todo cuando solo debe ejecutarse la rama verdadera. Entonces `when` es más claro y conciso."}, lang),
        "", g.t({"de": "Die Grundform von `when`:", "fr": "La forme de base de `when` :", "es": "La forma básica de `when`:"}, lang), "",
        g.WHEN_FORM, "",
        g.t({"de": "- Bei `#t` werden alle Ausdrücke im Body nacheinander ausgeführt.\n- Bei `#f` passiert nichts; es wird kein Wert zurückgegeben.", "fr": "- Si `#t`, toutes les expressions du corps s'exécutent en séquence.\n- Si `#f`, rien ne se passe ; aucune valeur n'est renvoyée.", "es": "- Si `#t`, todas las expresiones del cuerpo se ejecutan en secuencia.\n- Si `#f`, no ocurre nada; no se devuelve valor."}, lang),
        "", g.t({"de": "### Beispiel", "fr": "### Exemple", "es": "### Ejemplo"}, lang), "",
        g.WHEN_EX, "",
        g.t({"de": "### `if` und `when` im Vergleich", "fr": "### Comparer `if` et `when`", "es": "### Comparar `if` y `when`"}, lang), "",
        g.t({"de": "Beide zusammen im selben Beispiel:", "fr": "Les deux ensemble dans le même exemple :", "es": "Ambos juntos en el mismo ejemplo:"}, lang), "",
        g.WHEN_CONTRAST, "",
        g.t({"de": "#### Erklärung", "fr": "#### Explication", "es": "#### Explicación"}, lang), "",
        g.t({"de": "1. **`if`:** `(= 0 1)` ist falsch, daher der `else`-Zweig.\n2. **`when` im `else`:** `(< 0 1)` ist wahr; beide `lumi-message`-Aufrufe laufen.\n\n#### Warum `when`?\n\n- Kein leerer oder Dummy-`else` nötig.\n- Macht deutlich, dass nur der wahre Zweig zählt.", "fr": "1. **`if` :** `(= 0 1)` est faux, donc branche `else`.\n2. **`when` dans le `else` :** `(< 0 1)` est vrai ; les deux `lumi-message` s'exécutent.\n\n#### Pourquoi `when` ?\n\n- Pas de `else` vide ou factice.\n- Montre que seule la branche vraie compte.", "es": "1. **`if`:** `(= 0 1)` es falso, rama `else`.\n2. **`when` en el `else`:** `(< 0 1)` es verdadero; ambos `lumi-message` se ejecutan.\n\n#### ¿Por qué `when`?\n\n- Sin `else` vacío o ficticio.\n- Deja claro que solo importa la rama verdadera."}, lang),
        "", g.t({"de": "### Zusammenfassung", "fr": "### Résumé", "es": "### Resumen"}, lang), "",
        g.t({"de": "- **`if`:** wenn beide Zweige gebraucht werden.\n- **`when`:** nur wahrer Zweig, ggf. mehrere Aktionen.\n- Kombination strukturiert komplexe Bedingungen klar.", "fr": "- **`if` :** quand les deux branches comptent.\n- **`when` :** branche vraie seule, plusieurs actions possibles.\n- Les combiner structure clairement des conditions complexes.", "es": "- **`if`:** cuando importan ambas ramas.\n- **`when`:** solo rama verdadera, varias acciones.\n- Combinarlos estructura condiciones complejas con claridad."}, lang),
    )
    return meta("when", body)


def build_if(lang: str) -> dict[str, str]:
    body = g.j(
        g.t({"de": "In seiner einfachsten Form wertet `if` in Scheme einen Test aus und führt je nach Ergebnis einen von zwei Codeblöcken aus:", "fr": "Dans sa forme la plus simple, `if` en Scheme évalue un test et, selon le résultat, exécute l'un de deux blocs de code :", "es": "En su forma más simple, `if` en Scheme evalúa una prueba y, según el resultado, ejecuta uno de dos bloques de código:"}, lang), "",
        g.IF_SIMPLE, "",
        g.t({"de": "- Bei `#t` wird der **consequent** ausgeführt (Wert zurückgeben oder Nebeneffekte).", "fr": "- Si `#t`, le **consequent** s'exécute (valeur ou effets de bord).", "es": "- Si `#t`, se ejecuta el **consequent** (valor o efectos secundarios)."}, lang),
        "", g.t({"de": "### Beispiel", "fr": "### Exemple", "es": "### Ejemplo"}, lang), "", g.IF_EX1, "",
        g.t({"de": "- Test: `(< 0 1)` ist wahr.\n- `(lumi-message \"True!\")` wird ausgeführt.", "fr": "- Test : `(< 0 1)` est vrai.\n- `(lumi-message \"True!\")` s'exécute.", "es": "- Prueba: `(< 0 1)` es verdadera.\n- Se ejecuta `(lumi-message \"True!\")`."}, lang),
        "", g.t({"de": "### Else-Zweig: `if-else`", "fr": "### Branche else : `if-else`", "es": "### Rama else: `if-else`"}, lang), "",
        g.IF_ELSE, "", g.IF_ELSE2, "",
        g.t({"de": "### So funktioniert es", "fr": "### Comment ça marche", "es": "### Cómo funciona"}, lang), "",
        g.t({"de": "1. **Test** zuerst auswerten.\n2. Bei `#t` **consequent**, bei `#f` **alternative**.\n\nBeide Blöcke können jeden gültigen Scheme-Ausdruck enthalten.", "fr": "1. **Tester** d'abord.\n2. Si `#t` **consequent**, si `#f` **alternative**.\n\nLes deux blocs peuvent contenir toute expression Scheme valide.", "es": "1. **Evaluar** la prueba primero.\n2. Si `#t` **consequent**, si `#f` **alternative**.\n\nAmbos bloques pueden ser cualquier expresión Scheme válida."}, lang),
        "", g.t({"de": "#### Beispiel 1: Wert zurückgeben", "fr": "#### Exemple 1 : renvoyer une valeur", "es": "#### Ejemplo 1: devolver un valor"}, lang), "", g.IF_RET, "",
        g.t({"de": "Ergebnis: **1**", "fr": "Résultat : **1**", "es": "Resultado: **1**"}, lang),
        "", g.t({"de": "#### Beispiel 2: `begin`-Block", "fr": "#### Exemple 2 : bloc `begin`", "es": "#### Ejemplo 2: bloque `begin`"}, lang), "", g.IF_BEGIN, "",
        g.t({"de": "Ergebnis: **Gibt \"False condition met, calculating...\" aus und liefert 12.**", "fr": "Résultat : **Affiche « False condition met, calculating... » et renvoie 12.**", "es": "Resultado: **Imprime \"False condition met, calculating...\" y devuelve 12.**"}, lang),
        "", g.t({"de": "#### Beispiel 3: `let`-Ausdruck", "fr": "#### Exemple 3 : expression `let`", "es": "#### Ejemplo 3: expresión `let`"}, lang), "", g.IF_LET, "",
        g.t({"de": "Ergebnis: **Gibt \"True condition met, calculating...\" aus und liefert -10.**", "fr": "Résultat : **Affiche « True condition met, calculating... » et renvoie -10.**", "es": "Resultado: **Imprime \"True condition met, calculating...\" y devuelve -10.**"}, lang),
        "", g.t({"de": "### Zusammenfassung", "fr": "### Résumé", "es": "### Resumen"}, lang), "",
        g.t({"de": "- `if` wertet Tests aus und führt passende Blöcke aus.\n- Einfache Ausdrücke oder `begin`/`let`-Gruppen möglich.\n- Ohne explizites `else` nur **consequent** bei wahrem Test.", "fr": "- `if` évalue un test et exécute le bloc adapté.\n- Expressions simples ou groupes `begin`/`let`.\n- Sans `else` explicite, seul le **consequent** si vrai.", "es": "- `if` evalúa pruebas y ejecuta el bloque adecuado.\n- Expresiones simples o grupos `begin`/`let`.\n- Sin `else` explícito, solo **consequent** si es verdadero."}, lang),
    )
    return meta("if", body)


def build_cond(lang: str) -> dict[str, str]:
    body = g.j(
        g.t({"de": "In Scheme wählt das bedingte `cond` anhand mehrerer Tests einen von mehreren Codeblöcken — wie ein mehrgliedriges `if`, geprüft in Reihenfolge bis zum ersten Treffer.", "fr": "En Scheme, le conditionnel `cond` sélectionne l'un de plusieurs blocs à exécuter selon plusieurs tests — comme un `if` à branches multiples, évalué dans l'ordre jusqu'au premier succès.", "es": "En Scheme, el condicional `cond` elige uno de varios bloques según múltiples pruebas — como un `if` multirrama evaluado en orden hasta el primer acierto."}, lang),
        "", g.t({"de": "### Syntax", "fr": "### Syntaxe", "es": "### Sintaxis"}, lang), "", g.COND_SYNTAX, "",
        g.t({"de": "- Tests in Schreibreihenfolge.\n- Erster `#t`-Test: **consequent** läuft, `cond` stoppt.\n- `else` optional als Fallback.", "fr": "- Tests dans l'ordre d'écriture.\n- Premier `#t` : **consequent** exécuté, `cond` s'arrête.\n- `else` optionnel en repli.", "es": "- Pruebas en orden.\n- Primer `#t`: **consequent** ejecutado, `cond` para.\n- `else` opcional como respaldo."}, lang),
        "", g.t({"de": "### So funktioniert es", "fr": "### Comment ça marche", "es": "### Cómo funciona"}, lang), "",
        g.t({"de": "1. **Jede Bedingung testen** in Reihenfolge.\n2. **Passenden consequent ausführen**; sonst `else` falls vorhanden.", "fr": "1. **Tester chaque condition** dans l'ordre.\n2. **Exécuter le consequent** correspondant ; sinon `else` si présent.", "es": "1. **Probar cada condición** en orden.\n2. **Ejecutar el consequent** correspondiente; si no, `else` si existe."}, lang),
        "", g.t({"de": "### Beispiele", "fr": "### Exemples", "es": "### Ejemplos"}, lang),
        "", g.t({"de": "#### Beispiel 1: Einzeilige consequents", "fr": "#### Exemple 1 : conséquents sur une expression", "es": "#### Ejemplo 1: consecuentes de una expresión"}, lang), "", g.COND_EX1, "",
        g.t({"de": "Ergebnis: **\"This will run\"**", "fr": "Résultat : **\"This will run\"**", "es": "Resultado: **\"This will run\"**"}, lang),
        "", g.t({"de": "#### Beispiel 2: Mehrere Aktionen mit `begin`", "fr": "#### Exemple 2 : actions multiples avec `begin`", "es": "#### Ejemplo 2: varias acciones con `begin`"}, lang), "", g.COND_EX2, "",
        g.t({"de": "Ergebnis: **Gibt \"Condition met\" aus und liefert 25.**", "fr": "Résultat : **Affiche « Condition met » et renvoie 25.**", "es": "Resultado: **Imprime \"Condition met\" y devuelve 25.**"}, lang),
        "", g.t({"de": "#### Beispiel 3: `let` im consequent", "fr": "#### Exemple 3 : bloc `let` dans un conséquent", "es": "#### Ejemplo 3: bloque `let` en el consecuente"}, lang), "", g.cond_ex3(lang), "",
        g.t({"de": "Ergebnis: **Gibt \"Positive condition met\" aus und liefert 40.**", "fr": "Résultat : **Affiche « Positive condition met » et renvoie 40.**", "es": "Resultado: **Imprime \"Positive condition met\" y devuelve 40.**"}, lang),
        "", g.t({"de": "#### Beispiel 4: Fallback mit `else`", "fr": "#### Exemple 4 : repli avec `else`", "es": "#### Ejemplo 4: respaldo con `else`"}, lang), "", g.COND_EX4, "",
        g.t({"de": "Ergebnis: **\"Fallback value\"**", "fr": "Résultat : **\"Fallback value\"**", "es": "Resultado: **\"Fallback value\"**"}, lang),
        "", g.t({"de": "### Zusammenfassung", "fr": "### Résumé", "es": "### Resumen"}, lang), "",
        g.t({"de": "- `cond` für mehrere Bedingungen klar und kompakt.\n- consequents einzeln oder mit `begin` gruppiert.\n- `let` für lokale Variablen; `else` als Fallback empfohlen.", "fr": "- `cond` pour plusieurs conditions clairement.\n- Conséquents simples ou groupés avec `begin`.\n- `let` pour variables locales ; `else` recommandé en repli.", "es": "- `cond` para varias condiciones con claridad.\n- Consecuentes simples o con `begin`.\n- `let` para variables locales; `else` recomendado."}, lang),
    )
    return meta("cond", body)


def build_recursion(lang: str) -> dict[str, str]:
    titles = {"de": "Einfache Rekursion", "fr": "Récursion simple", "es": "Recursión simple", "it": "Ricorsione semplice", "ja": "単純な再帰", "ko": "단순 재귀", "nl": "Eenvoudige recursie", "pl": "Prosta rekursja", "pt-br": "Recursão simples", "ru": "Простая рекурсия", "sv": "Enkel rekursion", "th": "การเรียกซ้ำแบบง่าย", "zh-cn": "简单递归", "zh-tw": "簡單遞迴"}
    body = g.j(
        g.t({"de": "Rekursion in Scheme bedeutet, dass eine Funktion sich selbst aufruft, um kleinere Teilprobleme zu lösen. **Einfache Rekursion** hat einen Basisfall zum Stoppen und einen rekursiven Fall zur Problemverkleinerung.", "fr": "En Scheme, la récursion signifie qu'une fonction s'appelle elle-même pour résoudre des sous-problèmes. Une **récursion simple** a un cas de base pour s'arrêter et un cas récursif qui réduit le problème.", "es": "En Scheme, la recursión significa que una función se llama a sí misma para resolver subproblemas. Una **recursión simple** tiene caso base para detenerse y caso recursivo que reduce el problema."}, lang),
        "", g.t({"de": "Allgemeine Struktur:", "fr": "Structure générale :", "es": "Estructura general:"}, lang), "", g.REC_STRUCT, "",
        g.t({"de": "- **Base Condition:** stoppt die Rekursion.\n- **Base Result:** Wert im Basisfall.\n- **Recursive Call:** Aufruf mit angepassten Argumenten.", "fr": "- **Base Condition :** arrête la récursion.\n- **Base Result :** valeur au cas de base.\n- **Recursive Call :** appel avec arguments réduits.", "es": "- **Base Condition:** detiene la recursión.\n- **Base Result:** valor en caso base.\n- **Recursive Call:** llamada con argumentos reducidos."}, lang),
        "", "---", "", g.t({"de": "### Beispiel: Summe 1 bis n", "fr": "### Exemple : somme de 1 à n", "es": "### Ejemplo: suma de 1 a n"}, lang), "", g.REC_SUM, "",
        g.t({"de": "#### Zerlegen und wieder zusammensetzen", "fr": "#### Décomposer et recomposer", "es": "#### Descomponer y recombinar"}, lang), "",
        g.t({"de": "Rekursion zerlegt das Problem; jeder Aufruf bearbeitet ein Stück. Am Basisfall setzt sich das Ergebnis wieder zusammen.", "fr": "La récursion décompose le problème ; chaque appel traite une partie. Au cas de base, le résultat se recompose.", "es": "La recursión descompone el problema; cada llamada trata una parte. En el caso base, el resultado se recomponen."}, lang),
        "", g.t({"de": "#### Schritt für Schritt: sum-to-n 3", "fr": "#### Pas à pas : sum-to-n 3", "es": "#### Paso a paso: sum-to-n 3"}, lang), "",
        g.t({"de": "1. *sum-to-n 3* → *(+ 3 (sum-to-n 2))*\n2. *sum-to-n 2* → *(+ 2 (sum-to-n 1))*\n3. *sum-to-n 1* → *(+ 1 (sum-to-n 0))*\n4. *sum-to-n 0* → *0*", "fr": "1. *sum-to-n 3* → *(+ 3 (sum-to-n 2))*\n2. *sum-to-n 2* → *(+ 2 (sum-to-n 1))*\n3. *sum-to-n 1* → *(+ 1 (sum-to-n 0))*\n4. *sum-to-n 0* → *0*", "es": "1. *sum-to-n 3* → *(+ 3 (sum-to-n 2))*\n2. *sum-to-n 2* → *(+ 2 (sum-to-n 1))*\n3. *sum-to-n 1* → *(+ 1 (sum-to-n 0))*\n4. *sum-to-n 0* → *0*"}, lang),
        "", g.t({"de": "#### Ergebnis zusammensetzen", "fr": "#### Recomposer le résultat", "es": "#### Recombinar el resultado"}, lang), "",
        g.t({"de": "1. *sum-to-n 0* → *0*\n2. *sum-to-n 1* → *1*\n3. *sum-to-n 2* → *3*\n4. *sum-to-n 3* → *6*", "fr": "1. *sum-to-n 0* → *0*\n2. *sum-to-n 1* → *1*\n3. *sum-to-n 2* → *3*\n4. *sum-to-n 3* → *6*", "es": "1. *sum-to-n 0* → *0*\n2. *sum-to-n 1* → *1*\n3. *sum-to-n 2* → *3*\n4. *sum-to-n 3* → *6*"}, lang),
        "", "---", "", g.t({"de": "### Beispiel: Listenelemente ausgeben", "fr": "### Exemple : afficher chaque élément", "es": "### Ejemplo: imprimir cada elemento"}, lang), "", g.REC_PRINT, "",
        g.t({"de": "- **Basisfall:** leere Liste → `\"done\"`.\n- **Rekursiv:** `car` ausgeben, Rest mit `cdr` verarbeiten.", "fr": "- **Cas de base :** liste vide → `\"done\"`.\n- **Récursif :** afficher `car`, traiter le reste avec `cdr`.", "es": "- **Caso base:** lista vacía → `\"done\"`.\n- **Recursivo:** imprimir `car`, procesar resto con `cdr`."}, lang),
        "", g.t({"de": "#### Verwendung", "fr": "#### Utilisation", "es": "#### Uso"}, lang), "", g.REC_USAGE, "",
        g.t({"de": "Ausgabe: *\"1\"*, *\"2\"*, *\"3\"* — Ergebnis: *\"done\"*", "fr": "Sortie : *\"1\"*, *\"2\"*, *\"3\"* — résultat : *\"done\"*", "es": "Salida: *\"1\"*, *\"2\"*, *\"3\"* — resultado: *\"done\"*"}, lang),
        "", g.t({"de": "### Zusammenfassung", "fr": "### Résumé", "es": "### Resumen"}, lang), "",
        g.t({"de": "- Basisfall stoppt; rekursiver Fall verkleinert das Problem.\n- Jeder Aufruf nähert sich dem Basisfall.\n- Immer einen Basisfall definieren — sonst endlose Rekursion.", "fr": "- Cas de base pour arrêter ; cas récursif pour réduire.\n- Chaque appel progresse vers le cas de base.\n- Toujours un cas de base — sinon récursion infinie.", "es": "- Caso base para parar; recursivo para reducir.\n- Cada llamada avanza hacia el caso base.\n- Siempre un caso base — o recursión infinita."}, lang),
    )
    return meta(titles[lang], body)


def build_iter_index(lang: str) -> dict[str, str]:
    titles = {"de": "Iteration", "es": "Iteración", "fr": "Itération", "it": "Iterazione", "ja": "反復", "ko": "반복", "nl": "Iteratie", "pl": "Iteracja", "pt-br": "Iteração", "ru": "Итерация", "sv": "Iteration", "th": "การวนซ้ำ", "zh-cn": "迭代", "zh-tw": "迭代"}
    body = g.j(
        g.t({"de": "Iteration ist ein Grundpfeiler der Programmierung: Sie ermöglicht Skripten, Aktionen zu wiederholen und Datensammlungen effizient zu verarbeiten. In Scheme bietet Iteration Werkzeuge, um repetitive Aufgaben zu automatisieren, Datenstrukturen zu bearbeiten und ausgefeilte Ausführungsmuster zu erzeugen.", "fr": "L'itération est une pierre angulaire de la programmation : elle permet aux scripts de répéter des actions et de traiter efficacement des collections de données. En Scheme, l'itération fournit les outils pour automatiser les tâches répétitives, manipuler des structures de données et créer des schémas d'exécution sophistiqués.", "es": "La iteración es un pilar de la programación: permite repetir acciones y procesar colecciones de datos con eficiencia. En Scheme, la iteración ofrece herramientas para automatizar tareas repetitivas, manipular estructuras de datos y crear patrones de ejecución sofisticados."}, lang),
        "", g.t({"de": "### Die Rolle der Iteration in Scheme", "fr": "### Le rôle de l'itération en Scheme", "es": "### El papel de la iteración en Scheme"}, lang), "",
        g.t({"de": "- **Wiederholung automatisieren:** Aktionen mehrfach ausführen, ohne Code zu duplizieren.\n- **Effizienz steigern:** Große Operationen systematisch abwickeln.\n- **Code straffen:** Redundanz vermeiden und Lesbarkeit verbessern.", "fr": "- **Automatiser la répétition :** Exécuter des actions plusieurs fois sans dupliquer le code.\n- **Gagner en efficacité :** Traiter des opérations à grande échelle.\n- **Alléger le code :** Éliminer la redondance et améliorer la lisibilité.", "es": "- **Automatizar la repetición:** Ejecutar acciones varias veces sin duplicar código.\n- **Mejorar la eficiencia:** Procesar operaciones a gran escala.\n- **Agilizar el código:** Eliminar redundancia y mejorar la legibilidad."}, lang),
        "", g.t({"de": "### Verfügbare Iterationskonstrukte", "fr": "### Types d'itération disponibles", "es": "### Tipos de iteración disponibles"}, lang), "",
        g.t({"de": "- **map:** Funktion auf jedes Element anwenden, neue Liste zurückgeben.\n- **for-each:** Wie `map`, aber für Nebeneffekte ohne Rückgabe.\n- **do:** Allgemeine Schleife.\n- **recursion:** Funktionen rufen sich selbst auf.", "fr": "- **map :** Applique une fonction à chaque élément, renvoie une nouvelle liste.\n- **for-each :** Comme `map`, mais pour des effets de bord sans valeur de retour.\n- **do :** Boucle générale.\n- **recursion :** Les fonctions s'appellent elles-mêmes.", "es": "- **map:** Aplica una función a cada elemento, devuelve nueva lista.\n- **for-each:** Como `map`, pero para efectos secundarios sin retorno.\n- **do:** Bucle general.\n- **recursion:** Las funciones se llaman a sí mismas."}, lang),
        "", g.t({"de": "### So funktioniert Iteration", "fr": "### Comment fonctionne l'itération", "es": "### Cómo funciona la iteración"}, lang), "",
        g.t({"de": "1. **Wiederholung definieren**\n2. **Sequenziell ausführen**\n3. **Ergebnis zurückgeben (optional)**\n\nDiese Konstrukte helfen, anpassungsfähige und effiziente Skripte zu schreiben.", "fr": "1. **Définir une répétition**\n2. **Exécuter en séquence**\n3. **Renvoyer un résultat (facultatif)**\n\nCes constructions aident à écrire des scripts adaptables et efficaces.", "es": "1. **Definir repetición**\n2. **Ejecutar en secuencia**\n3. **Devolver resultado (opcional)**\n\nEstos constructos ayudan a escribir scripts adaptables y eficientes."}, lang),
    )
    return meta(titles[lang], body)


BUILDERS = {
    "content/hub/scripting/fundamentals/Conditionals/_index.md": lambda lang: meta(*data.cond_index(lang)),
    "content/hub/scripting/fundamentals/Conditionals/conditionals-cond.md": build_cond,
    "content/hub/scripting/fundamentals/Conditionals/conditionals-if.md": build_if,
    "content/hub/scripting/fundamentals/Conditionals/conditionals-when.md": build_when,
    "content/hub/scripting/fundamentals/Iteration/_index.md": build_iter_index,
    "content/hub/scripting/fundamentals/Iteration/do.md": build_do,
    "content/hub/scripting/fundamentals/Iteration/for-each.md": build_for_each,
    "content/hub/scripting/fundamentals/Iteration/map.md": g.build_map,
    "content/hub/scripting/fundamentals/Iteration/recursion.md": build_recursion,
}

prose = {rel: {lang: builder(lang) for lang in LANGS} for rel, builder in BUILDERS.items()}
(SCRIPT_DIR / "_batch_fundamentals_2_prose.json").write_text(json.dumps(prose, ensure_ascii=False, indent=2), encoding="utf-8")
print(f"prose: {sum(len(v) for v in prose.values())} entries")

for cmd in ["_mk_f2_pages.py", "_batch_fundamentals_2.py"]:
    r = subprocess.run([sys.executable, str(SCRIPT_DIR / cmd)], cwd=SCRIPT_DIR, capture_output=True, text=True)
    print(r.stdout.strip() or r.stderr.strip())
    if r.returncode:
        sys.exit(r.returncode)
