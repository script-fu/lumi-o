---
title: "Lumi-o"
type: docs
url: "hub/about/lumi-o"
weight: 1
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 060cd1cd6261e7278b4bc6e6c7330a374ecb20b5368faedde03a6e15da424d12
---
Lumi ist eine schnelle, effiziente Anwendung ausschließlich für Linux zur Erstellung von Rasterbildern, die offen entwickelt wird. Designentscheidungen, Architekturdokumentation und Entwicklungsgeschichte sind öffentlich, damit Nutzer nachvollziehen können, wie sich die Software weiterentwickelt.

Lumi ist auf digitale Malerei und Illustration ausgerichtet, bleibt aber fähig zu strukturierter Bildbearbeitung und fotografischer Korrektur. Korrekturanpassungen, Objektiv-Effekte, tonbasierte Farbkorrektur und zerstörungsfreie Filterebenen unterstützen eine breite Palette an Bearbeitungs-Workflows.

Lumi basiert auf der Idee, dass sich Malsoftware wie ein verlässliches Studio-Werkzeug verhalten sollte: vorhersehbar, transparent und auf das Erzeugen von Bildern fokussiert.

## Zweck

Lumi unterstützt eine strukturierte, zerstörungsfreie Methode der Bilderstellung — ob gemalt, gezeichnet oder fotografisch. Es ist eine fokussierte, eigenständige Alternative sowohl zu allgemeinen Bildbearbeitungsprogrammen als auch zu dedizierter Malsoftware: frei und Open Source, ohne Abonnements, Lock-in, Cloud-Abhängigkeit oder KI-Bildgenerierung.

Zuverlässigkeit und langfristiger Zugriff sind Kernmerkmale. Das offene, verzeichnisbasierte Dateiformat bleibt ohne proprietäre Software lesbar; XCF- und PSD-Import sowie -Export werden unterstützt.

## Künstlerischer Hintergrund

Lumi wird von einem unabhängigen Künstler entwickelt, mit Erfahrung in Pixelkunst, traditionellem Zeichnen und Malen, Spieleentwicklung, technischer Kunst, Illustration und 3D-Animation. Dieser Hintergrund prägt den Ansatz zu Farbe, Linienführung, Ebenen, Leistung, Datenwiederherstellung, Skripting und Benutzererfahrung.

## Philosophie

Lumi verbindet ein pigmentbasiertes Farbsystem mit einem reaktionsschnellen, zerstörungsfreien, ebenenbasierten Workflow. Sein Farbsystem ist eine bewusste Abkehr von herkömmlichen HSV-Schiebereglern und beliebigen RGB-Auswahlfeldern.

- **Pigmentzentrierte Farbe**: Reale Pigmentprofile (Colour-Index-Codes) werden spektral gemischt, sodass sich Paletten eher wie echte Farbe verhalten.
- **Palettengesteuerter Workflow**: Gespeicherte, umschaltbare Paletten organisieren Pigmente, Mischungen, Wertebereiche und Verläufe und halten Farbentscheidungen innerhalb eines Bildes oder Projekts stimmig.
- **Taktile, fokussierte Werkzeuge**: Pinsel integrieren Stiftdruck, Neigung und Geschwindigkeit für direkte, nuancierte Steuerung; Bedienelemente unterstützen bewusste Entscheidungen ohne unnötige Komplexität. Eine Oberflächentextur kann einem Strich Körnung und Griffigkeit einer Fläche geben, auch bei kalligrafischen Strichen, und eine Wendung kann diese Körnung verschieben. Borstenpinsel lassen sich fein formen und entlang des Strichs animieren.
- **Reaktionsschnelle Leinwand**: OpenGL kann den Ebenenstapel zeichnen und Pinselstriche für einzelne Werkzeugvoreinstellungen malen. Große Airbrush-Pinsel und große Texturstempel halten mit der Hand mit, und die Bewegung über die Leinwand bleibt flüssig.
- **Zerstörungsfreie Zuverlässigkeit**: Ebenen und bearbeitbare Filter skalieren auf komplexe Projekte und bleiben vorhersehbar. Automatisches Speichern, schnelles Speichern, inkrementelle Saves und Wiederherstellung schützen lange Malsitzungen und große Projekte.
- **Live-Arbeitsbereiche**: Benannte Profile bewahren Docks, Werkzeuge, Presets, Paletten und Gerätebindungen und schalten sie zur Laufzeit atomar um.
- **Scheme-Skripting**: Lumi erweitert die Script-Fu-Tradition um eine Scheme-basierte Plug-in-Sprache und zusätzliche Hilfsfunktionen zum Erstellen von Plug-ins und zur Workflow-Automatisierung.

[Filter](/hub/features/filters/) — einschließlich geformter Bokeh-Objektivunschärfe, Tilt-Shift, Tonwertkorrektur, Schärfung und Rauschreduzierung — können neben der Pinselarbeit bearbeitbar bleiben.

## Grenzen

- **Fokussiert, nicht erschöpfend**: Lumi zielt nicht auf Webdesign, Desktop Publishing oder jede Nische ab, die ein breiter Editor wie GIMP abdecken will.
- **Nur Linux**: Lumi wird auf Debian mit Cinnamon (X11) entwickelt. GNOME unter Wayland wurde auf das allgemeine Erscheinungsbild der Anwendung und auf das OpenGL-Rendering hin getestet. Windows und macOS werden nicht unterstützt.

## Danksagungen

Lumi baut auf dem GNU Image Manipulation Program (GIMP) auf. Lumi erkennt die langjährige Arbeit der Entwickler, Künstler und Mitwirkenden an und ist ihnen zutiefst dankbar.

![Lumi logo placeholder](/images/lumi.png)
