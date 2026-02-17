---
title: "Der Verfahrensbrowser"
type: docs
weight: 1
---
Mit dem **Lumi Procedure Browser** können Sie die verfügbaren Prozeduren (integriert und Plug-in bereitgestellt) durchsuchen und deren Parameter und Rückgabewerte überprüfen.

### Wo finde ich den Lumi Procedure Browser?

Sie können über das Menü **Hilfe** auf den Verfahrensbrowser in Lumi zugreifen:

- **Hilfe** -> **Prozedur-Browser**

### Was der Prozedurbrowser macht

Der Prozedurenbrowser listet alle internen Prozeduren von Lumi sowie die durch Plug-ins hinzugefügten Prozeduren auf, einschließlich der gerade installierten Prozeduren. Jeder Prozedureintrag enthält nützliche Informationen, darunter:

– Der Prozedurname.
- Eine Beschreibung dessen, was es tut.
- Die akzeptierten Parameter (Eingabewerte).
- Die Rückgabewerte (Ausgabe).

Suchen Sie nach Schlüsselwort oder Verfahrensname, wenn Sie eine Anrufsignatur überprüfen oder den genauen Verfahrensnamen bestätigen müssen.

#### (Lumi-Nachricht) im Verfahrensbrowser

Suchen Sie nach `lumi-message`, um seine Parameter und Rückgabewerte anzuzeigen.

### Finden Sie Ihr Plug-in

Sobald Sie das Programm „Hello World!“ installiert haben. Plug-in finden Sie im Verfahrensbrowser aufgelistet. Suchen Sie einfach nach dem Funktionsnamen, den Sie bei Lumi registriert haben, in diesem Fall „scheme-hello-world“. Der Eintrag zeigt die Parameter und alle mit dem Plug-in verbundenen Rückgabewerte zusammen mit einer kurzen Beschreibung an. Außerdem sehen Sie im Abschnitt **Zusätzliche Informationen**, wo einige der Textzeilen angezeigt werden, die Sie während des Registrierungsprozesses als Eingabeparameter eingegeben haben.

```scheme
(scheme-register-procedure "scheme-hello-world"   ;; Procedure name
  "Hello world!"                                        ;; Menu item name
  "A Scheme procedure plug-in"                       ;; Tool tip and description
  "Your Name"                                           ;; Author
  "Under GNU GENERAL PUBLIC LICENSE Version 3"          ;; License
  "2024")                                               ;; Copyright Date
```

Dadurch können Sie leicht überprüfen, ob Ihr Plug-in ordnungsgemäß registriert ist, und Sie können schnell überprüfen, wie es mit anderen Verfahren in Lumi interagiert. Der Prozedurbrowser ist ein leistungsstarkes Tool zum Debuggen und Erweitern Ihrer Plug-Ins, indem Sie alle verfügbaren Prozeduren in Lumi erkunden.