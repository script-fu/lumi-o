---
title: "Hallo Welt!"
type: docs
weight: 1
---
Dieses Tutorial führt Sie durch die minimale Struktur eines Scheme-Plug-Ins. Einige Zeilen sind „Boilerplate“: Sie sind erforderlich, damit Lumi die Datei laden kann, auch wenn Sie sie noch nicht vollständig verstehen.

```bash
# !/usr/bin/env lumi-scheme-interpreter-0.1
```

Auf einem hohen Niveau werden Sie:

1. Definieren Sie eine Funktion
2. Registrieren Sie es, damit es in der Verfahrensdatenbank angezeigt wird
3. (Optional) Fügen Sie einen Menüeintrag hinzu
4. Installieren Sie die Datei in einem Plug-In-Ordner

### Definieren Sie eine Funktion

Eine Funktion, auch _Prozedur_ genannt, ist ein Codeblock mit einem Namen und Zweck, sie nimmt eine Eingabe entgegen und erzeugt eine Ausgabe.

**Eingabe** > **_Funktion_** > **Ausgabe**

### Registrieren Sie die Funktion

Bei der Registrierung wird der Funktionsname in eine Liste eingetragen, damit Lumi davon erfährt.

```scheme
(scheme-register-procedure "scheme-hello-world"...
```

### Link zum Menü

Dadurch erfahren Sie, wo Lumi Ihre Funktion im Menüsystem finden kann.

```scheme
(scheme-menu-register "scheme-hello-world" "<Image>/Funky")
```

Dadurch wird das Menü „Funky“ in der Hauptmenüleiste angezeigt. Ändern Sie den Pfad, um das Plug-in an einer anderen Stelle abzulegen. Der Pfad `<Image>/Funky` bedeutet, dass das Plug-in in der Menükategorie **Bild** angezeigt wird. Sie können `<Image>` in `<Tools>`, `<Filters>` usw. ändern, je nachdem, wo das Plug-in angezeigt werden soll.

### Kommentare

In Scheme, der Basissprache von Scheme, werden Kommentare im Allgemeinen erstellt, indem einer hilfreichen Textzeile `;;` vorangestellt wird. Die Verwendung von Kommentaren hängt von Ihren Kenntnissen als Programmierer ab – wenn Sie gelegentlich programmieren, sind mehr Kommentare hilfreich. Wenn Sie ständig programmieren, ist der Code genauso einfach zu lesen wie der Kommentar. Außerdem neigt der Code bei funktionaler Programmierung dazu, so beschreibend zu werden, dass er sich wie ein Skript liest.

### Syntax

Code enthält in der Regel nur wenige Regeln für die Platzierung von Elementen in einer Zeile, damit wir die Zeile leicht lesen können. Beispielsweise kann ein Satz nach einem Komma oder Punkt ein Leerzeichen enthalten. Es hilft der Lesbarkeit.

Code kann Dinge auf ähnliche Weise anordnen, was auf den ersten Blick seltsam aussehen könnte:

```scheme
(define (function-name input-a
                       input-b
                       input-c))
```

## Beispielcode

Hier ist das vollständige Beispiel. Den meisten Lumi-Prozeduren wird `lumi-` vorangestellt. Beispielsweise gibt `lumi-message` eine Zeichenfolge an den konfigurierten Nachrichtenhandler aus.

```scheme
# !/usr/bin/env lumi-scheme-interpreter-0.1

(define (scheme-hello-world)

  ;; Set the message handler to output the message to a GUI dialog box
  (lumi-message-set-handler 0)
  (lumi-message "Hello world!\n")

  ;; Set the message handler to output the message to the Error Console
  (lumi-message-set-handler 2)
  (lumi-message "Hello world!\n")

  ;; Send the message to the terminal, the OS window that launched Lumi
  (display "Hello world!\n"))


(scheme-register-procedure "scheme-hello-world"
  "Hello world!"
  "A Scheme procedure plug-in"
  "Mark Sweeney"
  "Under GNU GENERAL PUBLIC LICENSE Version 3"
  "2024")

(scheme-menu-register
  "scheme-hello-world"
  "<Image>/Funky")
```

### Installieren Sie das Plug-in

1. Gehen Sie zu **Lumi -> Bearbeiten -> Einstellungen -> Ordner -> Plug-ins**.
2. Fügen Sie Ihren [repo](/hub/scripting/tools/git) Plug-Ins-Ordner zur Liste hinzu.
3. Erstellen Sie einen Ordner für das Plug-in und speichern Sie den obigen Beispielcode als `hello-world.scm`:
  - `your-plug-ins-repo/hello-world/hello-world.scm`
4. Klicken Sie mit der rechten Maustaste auf die Datei `hello-world.scm`.
5. Gehen Sie zu **Eigenschaften -> Berechtigungen -> Ausführung der Datei als Programm zulassen**.
6. Starten Sie Lumi neu.

### Probieren Sie das Plug-in aus

Das Plug-in sollte nun im Menü „Funky“ im Hauptfenster von Lumi erscheinen. Klicken Sie darauf und es sollte „Hallo Welt!“ angezeigt werden. Nachricht. Versuchen Sie, den Code zu ändern, z. B. den Nachrichtentext, und speichern Sie die Datei. Wenn Sie das Plug-in erneut ausführen, werden Ihre Änderungen übernommen, ohne dass Lumi neu gestartet werden muss.

Experimentieren Sie, indem Sie den Menüpfad ändern. Beispielsweise fügt `"<Image>/File"` es in das Menü „Datei“ ein und `"<Image>/File/Funky"` erstellt einen neuen Abschnitt im Menü „Datei“. Dies ist eine großartige Möglichkeit, die Anzeige Ihres Plug-Ins anzupassen und Ihre Tools zu organisieren.