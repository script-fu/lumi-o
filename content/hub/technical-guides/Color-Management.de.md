---
title: "Farbmanagement"
type: docs
weight: 15
---
Lumi-o ist so konfiguriert, dass es sofort einsatzbereit ist. Solange Sie an einem Bild mit **16-Bit-Präzision oder höher** arbeiten, ist die Software bereits für die Verwendung des standardmäßigen gebündelten Softproofing (CMYK) und der integrierten sRGB-Profile eingerichtet; Es sollte alles ohne Konfiguration funktionieren.

Für diejenigen, die eine tiefere Kontrolle benötigen, erklärt dieser Leitfaden das grundlegende Farbmanagementmodell von Lumi, den Unterschied zwischen einem Bildprofil und einem Softproof-Profil, wo sich die Steuerelemente befinden und wie genau die Standardprofile mit der Anwendung gebündelt werden.

## Kurze Zusammenfassung

Lumi verwendet drei verschiedene Profilrollen:

1. **Bild-Arbeitsprofil**
   – Definiert, was die RGB- oder Graustufenzahlen des Bildes bedeuten.
   – Wird für Zuweisungs-/Konvertierungsvorgänge verwendet.
   - Typische Beispiele: integriertes sRGB, Adobe RGB.

2. **Profil anzeigen**
   - Beschreibt Ihren Monitor.
   - Wird verwendet, um das Bild korrekt auf Ihrem Bildschirm anzuzeigen.
   - Wird normalerweise vom System bereitgestellt oder in den Einstellungen ausgewählt.

3. **Softproof-Profil**
   - Simuliert ein anderes Ausgabegerät oder eine andere Druckbedingung.
   - Definiert die Pixelwerte des Bildes **nicht** neu.
   - Typische Beispiele: CMYK-Druckprofile wie `CoatedFOGRA39`.

## Bildprofil vs. Soft-Proof-Profil

### Bildprofil

Verwenden Sie dies, wenn Sie Lumi mitteilen möchten, in welchem Farbraum sich das Bild tatsächlich befindet.

Zwei gängige Operationen:

- **Profil zuweisen**
  - Ändert die dem Bild beigefügte Profilbezeichnung.
  - Konvertiert Pixelwerte **nicht**.
  – Nur verwenden, wenn sich die Pixelzahlen bereits im Bereich dieses Profils befinden.

- **In Profil konvertieren**
  - Konvertiert Pixelwerte vom aktuellen Bildprofil in ein neues.
  – Verwenden Sie diese Option, wenn Sie möchten, dass das Bild wirklich in einen anderen Arbeitsraum verschoben wird.

**Menüorte:**
- Bild > Farbmanagement > Farbprofil zuweisen ...
- Bild > Farbmanagement > In Farbprofil konvertieren ...

### Soft-Proof-Profil

Verwenden Sie diese Option, wenn Sie in der Vorschau sehen möchten, wie das Bild auf einem Zielgerät oder unter Druckbedingungen reproduziert wird.

Softproofing:
- lässt den Bildarbeitsbereich in Ruhe
– Ändert die Vorschau-Pipeline
- Kann Farben markieren, die außerhalb des Farbumfangs liegen
- dient der Vorschau und nicht der Neuzuweisung von Bilddaten

**Menüorte:**
- Bild > Farbmanagement > Soft-Proof-Einstellungen > Soft-Proof-Profil auswählen ...
- Bild > Farbmanagement > Soft-Proof-Einstellungen > Rendering Intent
- Bild > Farbmanagement > Soft-Proof-Einstellungen > Schwarzpunktkompensation
- Ansicht > Farbmanagement > Softproof-Vorschau aktivieren
- Ansicht > Farbmanagement > Farben außerhalb des Farbumfangs markieren

## So sehen Sie die Softproof-Vorschau

Es gibt zwei Haupteinstiegspunkte zum Umschalten von Softproofs.

### 1. Menü anzeigen

Verwendung:
- Ansicht > Farbmanagement > Softproof-Vorschau aktivieren

Dadurch wird die Vorschausimulation für die aktuelle Anzeige ein- oder ausgeschaltet.

### 2. Statusleiste umschalten

Lumi stellt Softproofing auch direkt in der unteren Statusleiste zur Verfügung.

- **Linksklick** (umschalten): Prooffarben aktivieren oder deaktivieren
- **Rechtsklick**: Öffnen Sie das Softproofing-Popover, in dem Sie Folgendes anpassen können:
  - aktuelles Profil
  - Profilauswahl
  - Wiedergabeabsicht
  - Schwarzpunktkompensation
  - Markierung außerhalb des Farbumfangs

{{< callout type="warning" >}}
**Wichtiger Hinweis zur Präzision**
Die Softproof-Vorschau ist nur für **16-Bit- und 32-Bit**-Bilder aktiviert.
Bei **8-Bit**-Bildern ist der Schalter deaktiviert und Lumi fordert Sie auf, zunächst die Präzision in eine höhere Tiefe umzuwandeln, bevor Sie die Farben in der Vorschau genau anzeigen können.
{{< /callout >}}

## Präferenzen und Standardeinstellungen

Globale Standardwerte leben in:
- Bearbeiten > Einstellungen > FarbmanagementRelevante Abschnitte:
- **Manuelles Monitorprofil**
- **Bevorzugtes RGB-Profil**
- **Bevorzugtes Graustufenprofil**
- **Softproofing**

### Aktuelle Lumi-Standardeinstellungen

#### Arbeitsräume

Derzeit angebotene gebündelte Arbeitsraum-ICCs aus dem freigegebenen Datenordner:
- `AdobeRGB1998.icc`
- `AppleRGB.icc`

Für standardmäßige sRGB-Arbeiten bietet Lumi außerdem ein **intern integriertes sRGB-Arbeitsprofil**.

#### Soft-Proof-Standards

Derzeit installierte gebündelte Softproof-Profile:
- `CoatedFOGRA39.icc`
- `USWebCoatedSWOP.icc`
- `JapanColor2001Coated.icc`

Sofern verfügbar, wird `CoatedFOGRA39.icc` als standardmäßiges gebündeltes Softproof-/CMYK-Referenzprofil verwendet.

## Praktische Arbeitsabläufe

### Zum Malen und für normale Bildschirmarbeiten

- Behalten Sie das Bild im integrierten sRGB-Farbraum oder einem anderen gültigen RGB-Arbeitsbereich bei.
- Lassen Sie Lumi das Systemmonitorprofil verwenden, sofern verfügbar.

### Zur Druckvorschau

- Behalten Sie das Bild im Standard-RGB-Arbeitsbereich bei.
- Wählen Sie ein Softproof-Profil, das der Zieldruckbedingung entspricht (z. B. FOGRA39).
- Aktivieren Sie die Softproof-Vorschau.
– Aktivieren Sie optional Gamut-Warnungen, um abgeschnittene Rendering Intents anzuzeigen.