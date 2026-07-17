---
title: "Farbmanagement"
type: docs
weight: 15
url: "hub/technical-guides/Color-Management"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 60e00f1b5e0b4a7bb3034ca99dd3f8f51f6bc52b1629a9ab717d2ac2166393ee
---

Lumi-o ist so konfiguriert, dass es sofort einsatzbereit ist. Solange Sie an einem Bild mit **16-Bit-Präzision oder höher** arbeiten, ist die Software bereits für die Verwendung des standardmäßig gebündelten Softproofing (CMYK) und der integrierten sRGB-Profile eingerichtet; alles sollte ohne Konfiguration funktionieren.

Für diejenigen, die mehr Kontrolle benötigen, erklärt dieser Leitfaden das grundlegende Farbmanagementmodell von Lumi, den Unterschied zwischen einem Bildprofil und einem Softproof-Profil, wo sich die Steuerelemente befinden und wie genau die Standardprofile mit der Anwendung gebündelt werden.

## Kurze Zusammenfassung

Lumi verwendet drei verschiedene Profilrollen:

1. **Bild-Arbeitsprofil**
   - Definiert, was die RGB- oder Graustufenzahlen des Bildes bedeuten.
   - Wird für Zuweisungs- und Konvertierungsvorgänge verwendet.
   - Typische Beispiele: integriertes sRGB, Adobe RGB.

2. **Anzeigeprofil**
   - Beschreibt Ihren Monitor.
   - Wird verwendet, um das Bild korrekt auf Ihrem Bildschirm anzuzeigen.
   - Wird normalerweise vom System bereitgestellt oder in den Einstellungen ausgewählt.

3. **Softproof-Profil**
   - Simuliert ein anderes Ausgabegerät oder eine andere Druckbedingung.
   - Definiert die Pixelwerte des Bildes **nicht** neu.
   - Typische Beispiele: CMYK-Druckprofile wie `CoatedFOGRA39`.

## Bildprofil vs. Softproof-Profil

### Bildprofil

Verwenden Sie dies, wenn Sie Lumi mitteilen möchten, in welchem Farbraum sich das Bild tatsächlich befindet.

Zwei gängige Operationen:

- **Profil zuweisen**
  - Ändert die dem Bild beigefügte Profilbezeichnung.
  - Konvertiert Pixelwerte **nicht**.
  - Nur verwenden, wenn sich die Pixelwerte bereits im Bereich dieses Profils befinden.

- **In Profil konvertieren**
  - Konvertiert Pixelwerte vom aktuellen Bildprofil in ein neues.
  - Verwenden, wenn das Bild wirklich in einen anderen Arbeitsraum wechseln soll.

**Menüorte:**
- Bild > Farbmanagement > Farbprofil zuweisen...
- Bild > Farbmanagement > In Farbprofil konvertieren...

### Softproof-Profil

Verwenden Sie dies, wenn Sie in der Vorschau sehen möchten, wie das Bild auf einem Zielgerät oder unter Druckbedingungen reproduziert wird.

Softproofing:
- lässt den Bild-Arbeitsraum unverändert
- ändert die Vorschau-Pipeline
- kann Farben außerhalb des Farbumfangs markieren
- dient der Vorschau, nicht der Neuzuweisung von Bilddaten

**Menüorte:**
- Bild > Farbmanagement > Softproof-Einstellungen > Softproof-Profil auswählen...
- Bild > Farbmanagement > Softproof-Einstellungen > Rendering Intent
- Bild > Farbmanagement > Softproof-Einstellungen > Schwarzpunktkompensation
- Ansicht > Farbmanagement > Softproof-Vorschau aktivieren
- Ansicht > Farbmanagement > Farben außerhalb des Farbumfangs markieren

## So sehen Sie die Softproof-Vorschau

Es gibt zwei Haupteinstiegspunkte zum Umschalten von Softproofs.

### 1. Menü Ansicht

Verwenden Sie:
- Ansicht > Farbmanagement > Softproof-Vorschau aktivieren

Dadurch wird die Vorschausimulation für die aktuelle Anzeige ein- oder ausgeschaltet.

### 2. Umschalter in der Statusleiste

Lumi stellt Softproofing auch direkt in der unteren Statusleiste zur Verfügung.

- **Linksklick** (Umschalten): Prooffarben aktivieren oder deaktivieren
- **Rechtsklick**: Öffnen Sie das Softproof-Popover, in dem Sie Folgendes anpassen können:
  - aktuelles Profil
  - Profilauswahl
  - Rendering Intent
  - Schwarzpunktkompensation
  - Markierung außerhalb des Farbumfangs

{{< callout type="warning" >}}
**Wichtiger Hinweis zur Präzision**
Die Softproof-Vorschau ist nur für **16-Bit- und 32-Bit**-Bilder aktiviert.
Bei **8-Bit**-Bildern ist der Schalter deaktiviert und Lumi fordert Sie auf, zuerst die Präzision in eine höhere Tiefe umzuwandeln, bevor Sie die Farben in der Vorschau genau anzeigen können.
{{< /callout >}}

## Einstellungen und Standardwerte

Globale Standardwerte finden Sie unter:
- Bearbeiten > Einstellungen > Farbmanagement

Relevante Abschnitte:
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

#### Softproof-Standards

Derzeit installierte gebündelte Softproof-Profile:
- `CoatedFOGRA39.icc`
- `USWebCoatedSWOP.icc`
- `JapanColor2001Coated.icc`

Sofern verfügbar, wird `CoatedFOGRA39.icc` als standardmäßiges gebündeltes Softproof-/CMYK-Referenzprofil verwendet.

## Praktische Arbeitsabläufe

### Zum Malen und für normale Bildschirmarbeit

- Behalten Sie das Bild im integrierten sRGB oder einem anderen gültigen RGB-Arbeitsraum.
- Lassen Sie Lumi das Systemmonitorprofil verwenden, sofern verfügbar.

### Für die Druckvorschau

- Behalten Sie das Bild im Standard-RGB-Arbeitsraum.
- Wählen Sie ein Softproof-Profil, das der Zieldruckbedingung entspricht (z. B. FOGRA39).
- Aktivieren Sie die Softproof-Vorschau.
- Aktivieren Sie optional Gamut-Warnungen, um abgeschnittene Rendering Intents anzuzeigen.
