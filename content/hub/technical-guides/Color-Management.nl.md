---
title: "Kleurbeheer"
type: docs
weight: 15
url: "hub/technical-guides/Color-Management"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 60e00f1b5e0b4a7bb3034ca99dd3f8f51f6bc52b1629a9ab717d2ac2166393ee
---

Lumi-o is geconfigureerd om direct te werken. Zolang u aan een afbeelding werkt met **16-bits of grotere precisie**, is de software al ingesteld om de standaard gebundelde soft-proofing (CMYK) en ingebouwde sRGB-profielen te gebruiken; alles zou zonder configuratie moeten werken.

Voor wie meer controle nodig heeft, legt deze handleiding het kernmodel voor kleurbeheer van Lumi uit, het verschil tussen een afbeeldingsprofiel en een soft-proof-profiel, waar de bedieningselementen staan en hoe de standaardprofielen precies bij de toepassing horen.

## Korte samenvatting

Lumi gebruikt drie verschillende profielrollen:

1. **Werkprofiel van de afbeelding**
   - Bepaalt wat de RGB- of grijswaarden van de afbeelding betekenen.
   - Gebruikt voor toewijzings- en conversiebewerkingen.
   - Typische voorbeelden: ingebouwd sRGB, Adobe RGB.

2. **Beeldschermprofiel**
   - Beschrijft uw monitor.
   - Gebruikt om de afbeelding correct op uw scherm weer te geven.
   - Meestal geleverd door het systeem of gekozen in Voorkeuren.

3. **Soft-proof-profiel**
   - Simuleert een ander uitvoerapparaat of een andere afdrukconditie.
   - Herdefinieert de pixelwaarden van de afbeelding **niet**.
   - Typische voorbeelden: CMYK-persprofielen zoals `CoatedFOGRA39`.

## Afbeeldingsprofiel versus soft-proof-profiel

### Afbeeldingsprofiel

Gebruik dit wanneer u Lumi wilt vertellen in welke kleurruimte de afbeelding zich daadwerkelijk bevindt.

Twee veelvoorkomende bewerkingen:

- **Profiel toewijzen**
  - Wijzigt het profiellabel dat aan de afbeelding is gekoppeld.
  - Converteert pixelwaarden **niet**.
  - Alleen gebruiken wanneer de pixelwaarden al in de ruimte van dat profiel staan.

- **Converteren naar profiel**
  - Converteert pixelwaarden van het huidige afbeeldingsprofiel naar een nieuw profiel.
  - Gebruik dit wanneer u wilt dat de afbeelding echt naar een andere werkruimte gaat.

**Menulocaties:**
- Afbeelding > Kleurbeheer > Kleurprofiel toewijzen...
- Afbeelding > Kleurbeheer > Converteren naar kleurprofiel...

### Soft-proof-profiel

Gebruik dit wanneer u wilt bekijken hoe de afbeelding zou worden gereproduceerd op een doelapparaat of onder een bepaalde afdrukconditie.

Soft-proofing:
- laat de werkruimte van de afbeelding ongewijzigd
- wijzigt de voorbeeldpijplijn
- kan kleuren buiten het gamut markeren
- is bedoeld voor voorbeeldweergave, niet voor het opnieuw toewijzen van afbeeldingsgegevens

**Menulocaties:**
- Afbeelding > Kleurbeheer > Soft-proof-instellingen > Soft-proof-profiel kiezen...
- Afbeelding > Kleurbeheer > Soft-proof-instellingen > Weergave-intentie
- Afbeelding > Kleurbeheer > Soft-proof-instellingen > Zwartpuntcompensatie
- Weergave > Kleurbeheer > Soft-proof-voorbeeld inschakelen
- Weergave > Kleurbeheer > Kleuren buiten gamut markeren

## De soft-proof-voorbeeldweergave inschakelen

Er zijn twee belangrijke toegangspunten om soft proofs in en uit te schakelen.

### 1. Menu Weergave

Gebruik:
- Weergave > Kleurbeheer > Soft-proof-voorbeeld inschakelen

Hiermee schakelt u de voorbeeldsimulatie voor het huidige scherm in of uit.

### 2. Schakelaar in de statusbalk

Lumi biedt soft-proofing ook rechtstreeks in de onderste statusbalk.

- **Linksklik** (schakelen): proofkleuren in- of uitschakelen
- **Rechtsklik**: open het soft-proof-popover waarin u het volgende kunt aanpassen:
  - huidig profiel
  - profielkiezer
  - weergave-intentie
  - zwartpuntcompensatie
  - markering buiten gamut

{{< callout type="warning" >}}
**Belangrijke opmerking over precisie**
Soft-proof-voorbeeldweergave is alleen ingeschakeld voor **16-bits en 32-bits** afbeeldingen.
Voor **8-bits** afbeeldingen is de schakelaar uitgeschakeld en vraagt Lumi u eerst de precisie naar een hogere bitdiepte te converteren voordat u kleuren nauwkeurig kunt bekijken.
{{< /callout >}}

## Voorkeuren en standaardinstellingen

Globale standaardinstellingen staan in:
- Bewerken > Voorkeuren > Kleurbeheer

Relevante secties:
- **Handmatig monitorprofiel**
- **Voorkeurs-RGB-profiel**
- **Voorkeurs-grijswaardenprofiel**
- **Soft-proofing**

### Huidige Lumi-standaardinstellingen

#### Werkruimten

Gebundelde ICC-profielen voor werkruimten die momenteel uit de gedeelde gegevensmap worden aangeboden:
- `AdobeRGB1998.icc`
- `AppleRGB.icc`

Voor standaard sRGB-werk biedt Lumi ook een **ingebouwd sRGB-werkprofiel**.

#### Soft-proof-standaardinstellingen

Gebundelde soft-proof-profielen die momenteel zijn geïnstalleerd:
- `CoatedFOGRA39.icc`
- `USWebCoatedSWOP.icc`
- `JapanColor2001Coated.icc`

Indien beschikbaar wordt `CoatedFOGRA39.icc` gebruikt als het standaard gebundelde soft-proof-/CMYK-referentieprofiel.

## Praktische workflows

### Voor schilderwerk en normaal schermwerk

- Houd de afbeelding in ingebouwd sRGB of een andere geldige RGB-werkruimte.
- Laat Lumi het systeemmonitorprofiel gebruiken, indien beschikbaar.

### Voor afdrukvoorbeeld

- Houd de afbeelding in de standaard RGB-werkruimte.
- Kies een soft-proof-profiel dat overeenkomt met de doelafdrukconditie (bijv. FOGRA39).
- Schakel soft-proof-voorbeeldweergave in.
- Schakel optioneel gamut-waarschuwingen in om afgeknipte weergave-intenties te zien.
