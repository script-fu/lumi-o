---
title: "Arbeitsbereiche"
type: docs
---
Ein Arbeitsbereich ist eine gespeicherte Momentaufnahme Ihrer gesamten UI-Umgebung: welche Bedienfelder geöffnet sind und wo, die Leinwanddekorationen und -abstände für die Normal- und Vollbildansicht, das aktive Design und Symbolset, das Toolbox-Layout, die aktive Palette und Ihre Werkzeugeinstellungen. Mit Lumi können Sie beliebig viele benannte Arbeitsbereiche speichern und sofort zwischen ihnen wechseln – alle geöffneten Bilder werden an Ort und Stelle aktualisiert, ein Neustart ist nicht erforderlich.

## Was ein Arbeitsbereich spart

Jeder benannte Arbeitsbereich speichert unabhängig voneinander Folgendes:

| Komponente | Was es abdeckt |
| :--- | :--- |
| **Layout** | Fensterposition und -größe, Dockanordnung (linke und rechte Bedienfeldspalten, welche Bedienfelder geöffnet sind und in welcher Reihenfolge), Einzel- oder Mehrfenstermodus, maximierter Zustand, Sichtbarkeit und Position der Registerkartenleiste |
| **Werkzeugoptionen** | Die aktuellen Einstellungen für jedes Werkzeug (Pinselgröße, Härte, Verzerrungsverhalten usw.) |
| **Eingabegeräte** | Konfiguration der Eingabegeräte: Druckkurven, Tastenbelegungen, Achsenzuordnungen für Stift und andere Geräte |
| **Leinwanddekorationen** | Standardeinstellungen pro Arbeitsbereich für Lineale, Bildlaufleisten, Hilfslinien, Raster, Auswahlhervorhebung, Ebenengrenze und Leinwandgrenze – eingestellt über **Einstellungen → Bildfenster → Standarddarstellung** und **Vollbilddarstellung**, unabhängig für Normal- und Vollbildansicht |
| **Leinwandpolsterung** | Auffüllmodus und Farbe pro Arbeitsbereich für die Normal- und Vollbildansicht – eingestellt über **Einstellungen → Bildfenster → Standarddarstellung** |
| **Thema & Symbole** | Aktives Design, dunkle/helle Farbvariante, Symbolsatz, Überschreibung der Symbolgröße und Schriftskalierung |
| **Werkzeugkasten** | FG/BG-Widget-Position (oben/unten/links/rechts), FG/BG-Skalierung, Wilber-Maskottchen-Sichtbarkeit, Werkzeuggruppen-Header |

Die aktive **Palette** und **Werkzeugvoreinstellung** werden ebenfalls pro Arbeitsbereich aufgezeichnet und beim Wechseln wiederhergestellt.

> **Leinwanddekorationen und Polsterung** unterliegen der Kontrolle von
> **Einstellungen → Bildfenster → Erweiterte Fensteroptionen → Standarddarstellung** (Normalansicht)
> und **Vollbilddarstellung** (Vollbildansicht). Passen Sie diese Einstellungen nach Ihren Wünschen an.
> Speichern Sie dann den Arbeitsbereich. Die Elemente im **Ansichtsmenü** (Lineale, Hilfslinien usw.) sind lokal für die
> aktuelles Bildfenster und werden nicht pro Arbeitsbereich gespeichert.

### Live-Updates beim Switch

Wenn Sie den Arbeitsbereich wechseln, werden alle geöffneten Bildfenster sofort aktualisiert – Lineale, Hilfslinien, Bildlaufleisten, Füllfarben und alle anderen Ansichtseinstellungen ändern sich, ohne dass Bilder geschlossen und erneut geöffnet werden müssen.

## Zugriff

**Bearbeiten → Einstellungen → Arbeitsbereich**

Im oberen Abschnitt der Seite „Arbeitsbereichseinstellungen“ werden alle Ihre gespeicherten Arbeitsbereiche aufgelistet und Steuerelemente für deren Verwaltung bereitgestellt.

## Einen Arbeitsbereich erstellen

Richten Sie Ihre Bedienfelder, Werkzeuge und Paletten genau nach Ihren Wünschen ein und gehen Sie dann wie folgt vor:

1. Öffnen Sie **Bearbeiten → Einstellungen → Arbeitsbereich**.
2. Klicken Sie auf **Layout speichern unter…**.
3. Geben Sie einen Namen ein und klicken Sie auf **Speichern**.

Der neue Arbeitsbereich wird im Dropdown-Menü **Aktives Layout** und im Menü **Windows** angezeigt.

## Arbeitsbereiche wechseln

Es gibt zwei Möglichkeiten zum Wechseln:

- **Windows-Menü**: Die Layoutnamen werden unter **Windows → Layout** angezeigt, um über die Leinwand schnell darauf zugreifen zu können.
- **Einstellungen → Arbeitsbereich**: Wählen Sie ein Layout aus der Dropdown-Liste **Aktives Layout** aus und klicken Sie auf **Layout neu laden**.

Der Wechsel erfolgt sofort – Lumi baut das Panel-Layout neu auf, stellt die Werkzeugoptionen wieder her, lädt die Geräteeinstellungen neu, aktualisiert die Leinwanddekorationen, den Abstand, das Design und das Toolbox-Layout – und das alles ohne Neustart.

## Arbeitsbereiche verwalten

Von **Bearbeiten → Einstellungen → Arbeitsbereich**:| Aktion | Wirkung |
| :--- | :--- |
| **Layout speichern** | Überschreibt den aktuellen Arbeitsbereich mit Ihren aktuellen Einstellungen. |
| **Layout speichern unter…** | Erstellt einen neuen benannten Arbeitsbereich aus Ihren aktuellen Einstellungen. |
| **Layout umbenennen…** | Benennt den ausgewählten Arbeitsbereich um. |
| **Layout neu laden** | Wendet den ausgewählten Arbeitsbereich sofort an. |
| **Layout löschen…** | Entfernt den ausgewählten Arbeitsbereich und seine Dateien dauerhaft. |

## Persistenzeinstellungen

Der untere Teil der Workspace-Einstellungsseite steuert, was Lumi automatisch speichert:

- **Fensterpositionen beim Beenden speichern**: Wenn diese Option aktiviert ist, werden Dock- und Fensterpositionen bei jedem Beenden auf die Festplatte geschrieben.
- **Fenster auf demselben Monitor öffnen**: Öffnet jedes Fenster auf dem Monitor erneut, auf dem es während der letzten Sitzung war.
- **Werkzeugoptionen beim Beenden speichern**: Speichert die aktuellen Werkzeugeinstellungen beim Beenden.
- **Eingabegeräteeinstellungen beim Beenden speichern**: Speichert die Stift- und Gerätekonfiguration beim Beenden.

Diese Einstellungen gelten pro Arbeitsbereich – jedes Layout behält seinen eigenen gespeicherten Status unabhängig bei.

## Beispiel-Workflows

Einige Möglichkeiten, wie Künstler mehrere Arbeitsbereiche nutzen können:

- **Malerei** – große Pinseldocks, warme Füllfarbe (eingestellt in Einstellungen → Bildfenster → Standarddarstellung), Ihre bevorzugte Designvariante
- **Färbung** – Hilfslinien und Leinwandbegrenzung aktiviert, Bildlaufleisten aktiviert (eingestellt in „Einstellungen“ → „Standarddarstellung“), neutrale Füllfarbe
- **Roughs** – versteckte Docks, keine Lineale oder Raster, dunkle Polsterung, kompakte Symbolgröße, um den Platz auf der Leinwand zu maximieren
- **Vollbild-Fokus** – unterschiedliche Füllfarben- und Dekorationseinstellungen im Vollbild-Erscheinungsbild im Vergleich zum Standard-Erscheinungsbild, sodass das Umschalten auf den Vollbildmodus eine wirklich andere Arbeitsumgebung bietet
- **Scripting** – Scripting-Bedienfeld geöffnet, Schriftgröße zur besseren Lesbarkeit erhöht, anderer Symbolsatz