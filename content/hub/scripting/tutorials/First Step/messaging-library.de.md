---
title: "Messaging-Bibliothek"
type: docs
weight: 6
---
Was als einzelne Funktion zum Senden von Nachrichten begann, hat sich im Laufe der Zeit zu einer Sammlung verwandter Funktionen entwickelt. Diese Funktionen bilden nun die Grundlage einer **Messaging-Bibliothek**, die für die Ausgabe an verschiedene Ziele wie die GUI, die Fehlerkonsole und das Terminal konzipiert ist.

### Warum eine Messaging-Bibliothek?

Da unsere Anforderungen wachsen, erfordert die Verarbeitung von Nachrichten über mehrere Ausgänge hinweg einen modulareren und erweiterbareren Ansatz. Anstatt dass eine einzelne Funktion alles erledigt, haben wir den Prozess in wiederverwendbare Komponenten unterteilt, um eine größere Flexibilität zu ermöglichen. Diese Bibliothek kann nun als allgemeines Messaging-Tool verwendet werden, von dem andere Plug-Ins oder Funktionen ausleihen können.

### Was macht die Messaging-Bibliothek?

Die Messaging-Bibliothek umfasst derzeit die folgenden Funktionen:

- **send-to-gui**: Sendet Nachrichten an das Lumi GUI-Dialogfeld.
- **send-to-error-console**: Sendet Nachrichten an die Lumi Error Console.
- **send-to-terminal**: Sendet Nachrichten an das Terminalfenster.
- **Nachricht senden**: Eine Dispatcher-Funktion, die Nachrichten an die entsprechende Ausgabe weiterleitet.
- **validate-message**: Stellt sicher, dass die Nachricht und die Ausgabe vor dem Senden gültig sind.

### Erweiterung der Bibliothek

Die **Messaging-Bibliothek** kann problemlos erweitert werden, um zusätzliche Ausgaben zu unterstützen. Zum Beispiel:

- **Send-to-File**: Nachrichten in einer Protokolldatei speichern.
- **Send-to-Logger**: Integration mit einem externen Protokollierungssystem.
- **send-to-notification**: Nachrichten als Systembenachrichtigungen anzeigen.

Durch die Befolgung des gleichen Musters aus modularem Aufbau und wiederverwendbaren Funktionen kann diese Bibliothek zu einem umfassenden Tool für die Bewältigung aller Arten von Messaging-Aufgaben werden.

## Vorteile einer Messaging-Bibliothek

- **Wiederverwendbarkeit**: Die Funktionen können über verschiedene Plug-Ins oder Projekte hinweg wiederverwendet werden.
- **Modularität**: Jede Funktion übernimmt eine bestimmte Aufgabe, wodurch der Code einfacher zu warten und zu erweitern ist.
- **Konsistenz**: Die Verwendung derselben Validierungs- und Nachrichtenverarbeitungsfunktionen stellt ein konsistentes Verhalten in der gesamten Anwendung sicher.

Die **Messaging-Bibliothek** ist der Beginn eines umfassenderen Frameworks, das die Verwaltung von Nachrichten in Ihrem Projekt vereinfachen könnte. Wenn die Bibliothek wächst, können neue Plug-Ins problemlos darauf zugreifen, um Nachrichten dorthin zu senden, wo sie benötigt werden.

Wir können die Dateistruktur anpassen:

```plaintext
/home/your-username/code/
  ├── script-fu/
      ├── library/
      │     └── send-message.scm -> messaging.scm
      └── plug-ins/
            └── hello-world/
                  └── hello-world.scm
```

Und denken Sie daran, das `load` im Haupt-Plug-in anzupassen:

```scheme
# !/usr/bin/env lumi-scheme-interpreter-0.1

(load "/home/mark/code/github/script-plugins/funky-library/messaging.scm")

(define (scheme-hello-world)
  (let ((message "Hello world!\n"))
    (send-message message 'gui)
    (send-message message 'error-console)
    (send-message message 'terminal)))

(scheme-register-procedure "scheme-hello-world"
  "Hello world!"
  "A Scheme procedure plug-in refactored"
  "Mark Sweeney"
  "Under GNU GENERAL PUBLIC LICENSE Version 3"
  "2024")

(scheme-menu-register
  "scheme-hello-world"
  "<Image>/Funky")
```