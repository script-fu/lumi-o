---
title: "Verfahrensbrowser"
type: docs
---
Der Prozedurbrowser ist das wichtigste Referenzwerkzeug zum Entdecken der Hunderten von Funktionen, die in der prozeduralen Datenbank (PDB) von Lumi verfügbar sind. Da jedes Tool, jeder Filter und jedes Skript in Lumi im PDB registriert sein muss, damit es aufgerufen werden kann, ist dieser Browser praktisch ein vollständiger PDB-Explorer.

## Öffnen des Verfahrensbrowsers

Gehen Sie zu **Hilfe → Programmierung → Prozedurbrowser**.

Sie können auch über **Durchsuchen** in der Scheme-Konsole darauf zugreifen.

## Was es zeigt

Der Prozedurenbrowser kann alle derzeit im PDB registrierten Prozeduren auflisten, unabhängig von ihrer Herkunft. Standardmäßig wird nach „intern“ gesucht, um die intern registrierten Kernprozeduren anzuzeigen.

- **Interne Prozeduren**: Kernfunktionen für Bildbearbeitung, Ebenenverwaltung und Werkzeugsteuerung.
- **Externe Plug-Ins**: Prozeduren, die von kompilierten C/C++-Plug-Ins oder dauerhaften Erweiterungen bereitgestellt werden.

## Suchen und Filtern

- **Suchfeld**: Filtert Verfahren nach Name, Beschreibung oder Autor. Wenn Sie das Suchfeld leeren, werden alle verfügbaren Verfahren angezeigt.
- **Suchtyp**: Mit der Such-Dropdown-Liste können Sie nach bestimmten Feldern filtern. Wenn Sie **nach Typ** festlegen und nach „intern“ suchen, wird die Liste eingegrenzt und zeigt nur die intern registrierten Kernprozeduren an.
- **Detaillierte Ansicht**: Wenn Sie auf eine Prozedur klicken, werden deren Parameter, Rückgabewerte, Autor, Datum und eine Beschreibung ihrer Funktion angezeigt.

Dies ist wichtig, um den genauen Namen und die Argumentsignatur einer Funktion zu finden, die Sie von Ihrem Skript aus aufrufen möchten.