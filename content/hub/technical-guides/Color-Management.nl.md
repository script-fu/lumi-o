---
title: "Kleurbeheer"
type: docs
weight: 15
---
Lumi-o is geconfigureerd om out-of-the-box te werken. Zolang u aan een afbeelding werkt met **16-bits of grotere precisie**, is de software al ingesteld om de standaard gebundelde soft-proofing (CMYK) en ingebouwde sRGB-profielen te gebruiken; het zou allemaal gewoon moeten werken zonder enige configuratie.

Voor degenen die meer controle nodig hebben, legt deze handleiding het belangrijkste kleurbeheermodel van Lumi uit, het verschil tussen een afbeeldingsprofiel en een soft-proof-profiel, waar de bedieningselementen zich bevinden en hoe de standaardprofielen precies bij de toepassing passen.

## Korte samenvatting

Lumi gebruikt drie verschillende profielrollen:

1. **Afbeeldingswerkprofiel**
   - Definieert wat de RGB- of grijswaardencijfers van de afbeelding betekenen.
   - Gebruikt voor toewijzings-/conversiebewerkingen.
   - Typische voorbeelden: ingebouwde sRGB, Adobe RGB.

2. **Profiel weergeven**
   - Beschrijft uw monitor.
   - Wordt gebruikt om de afbeelding correct op uw scherm weer te geven.
   - Meestal geleverd door het systeem of gekozen in Voorkeuren.

3. **Zacht profiel**
   - Simuleert een ander uitvoerapparaat of een andere afdrukconditie.
   - Herdefinieert **niet** de pixelwaarden van de afbeelding.
   - Typische voorbeelden: CMYK-persprofielen zoals `CoatedFOGRA39`.

## Beeldprofiel versus soft-proof profiel

### Afbeeldingsprofiel

Gebruik dit wanneer u Lumi wilt vertellen in welke kleurruimte de afbeelding zich daadwerkelijk bevindt.

Twee veel voorkomende bewerkingen:

- **Profiel toewijzen**
  - Wijzigt het profiellabel dat aan de afbeelding is gekoppeld.
  - Converteert **niet** pixelwaarden.
  - Alleen gebruiken als de pixelnummers zich al in de ruimte van dat profiel bevinden.

- **Converteren naar profiel**
  - Converteert pixelwaarden van het huidige beeldprofiel naar een nieuw profiel.
  - Gebruik deze optie als u wilt dat de afbeelding echt naar een andere werkruimte beweegt.

**Menulocaties:**
- Afbeelding > Kleurbeheer > Kleurprofiel toewijzen...
- Afbeelding > Kleurbeheer > Converteren naar kleurprofiel...

### Zachtbestendig profiel

Gebruik dit als u een voorbeeld wilt bekijken van hoe de afbeelding zou worden gereproduceerd op een doelapparaat of afdrukconditie.

Zachte proofing:
- laat de beeldwerkruimte met rust
- wijzigt de preview-pijplijn
- kan kleuren buiten het gamma markeren
- is bedoeld voor een voorbeeld, niet voor het opnieuw toewijzen van afbeeldingsgegevens

**Menulocaties:**
- Afbeelding > Kleurbeheer > Soft-proof-instellingen > Kies Soft-proof-profiel...
- Afbeelding > Kleurbeheer > Soft-proof-instellingen > Weergave-intentie
- Afbeelding > Kleurbeheer > Soft-proof-instellingen > Zwartpuntcompensatie
- Weergave > Kleurbeheer > Soft-proof voorbeeld inschakelen
- Weergave > Kleurbeheer > Kleuren buiten bereik markeren

## Hoe u het softproofvoorbeeld kunt bekijken

Er zijn twee belangrijke toegangspunten voor het wisselen van proefdrukken.

### 1. Menu bekijken

Gebruik:
- Weergave > Kleurbeheer > Soft-proof voorbeeld inschakelen

Hiermee wordt de voorbeeldsimulatie voor de huidige weergave in- of uitgeschakeld.

### 2. Statusbalk wisselen

Lumi legt soft-proofing ook rechtstreeks in de onderste statusbalk bloot.

- **Klik met de linkermuisknop** (schakelen): proefkleuren in- of uitschakelen
- **Klik met de rechtermuisknop**: open de soft-proofing-popover waarin u het volgende kunt aanpassen:
  - huidig profiel
  - profielkiezer
  - intentie weergeven
  - zwartpuntcompensatie
  - markering buiten het gamma

{{< callout type="warning" >}}
**Belangrijke opmerking over precisie**
Softproof-voorbeeld is alleen ingeschakeld voor **16-bits en 32-bits**-afbeeldingen.
Voor **8-bits**-afbeeldingen is de schakelaar uitgeschakeld en zal Lumi u vragen eerst de precisie naar een hogere diepte om te zetten voordat u de kleuren nauwkeurig kunt bekijken.
{{< /callout >}}

## Voorkeuren en standaardinstellingen

Mondiale wanbetalingen leven in:
- Bewerken > Voorkeuren > KleurbeheerRelevante secties:
- **Handmatig monitorprofiel**
- **Voorkeur RGB-profiel**
- **Voorkeur grijswaardenprofiel**
-**Zachtdicht**

### Huidige standaardinstellingen van Lumi

#### Werkruimtes

Gebundelde ICC's voor werkruimte die momenteel worden aangeboden vanuit de gedeelde gegevensmap:
- `AdobeRGB1998.icc`
- `AppleRGB.icc`

Voor standaard sRGB-werk biedt Lumi ook een **ingebouwd sRGB-werkprofiel intern**.

#### Soft-proof standaardwaarden

Gebundelde softproof-profielen die momenteel zijn geïnstalleerd:
- `CoatedFOGRA39.icc`
- `USWebCoatedSWOP.icc`
- `JapanColor2001Coated.icc`

Indien beschikbaar wordt `CoatedFOGRA39.icc` gebruikt als het standaard gebundelde soft-proof/CMYK-referentieprofiel.

## Praktische workflows

### Voor schilderwerk en normaal schermwerk

- Bewaar de afbeelding in de ingebouwde sRGB of een andere geldige RGB-werkruimte.
- Laat Lumi het systeemmonitorprofiel gebruiken, indien beschikbaar.

### Voor afdrukvoorbeeld

- Houd de afbeelding in de standaard RGB-werkruimte.
- Kies een softproof-profiel dat overeenkomt met de doelafdrukconditie (bijv. FOGRA39).
- Schakel een softproof-voorbeeld in.
- Schakel eventueel gammawaarschuwingen in om de weergave-intenties van geknipte weergaven te zien.