---
title: "Hulpprogramma-browser"
type: docs
---
Met de Utility Browser kunt u het ingebouwde Scheme-hulpprogramma stdlib verkennen dat bij Lumi wordt geleverd, zonder dat u de app hoeft te verlaten of door bronbestanden hoeft te bladeren.

## De hulpprogrammabrowser openen

Ga naar **Help → Programmeren → Hulpprogrammabrowser**.

Het venster gaat onmiddellijk open; er hoeft geen plug-in vooraf te worden geladen.

## Wat het laat zien

De browser vermeldt elke procedure, variabele en syntaxisvorm die wordt geëxporteerd door de zeven hulpprogrammabibliotheken die Lumi automatisch laadt bij het opstarten:

| Bibliotheek | Wat het omvat |
|---|---|
| `common.scm` | Hulpprogramma's voor algemene doeleinden (string, nummer, lijsthulpprogramma's) |
| `files.scm` | Hulpmiddelen voor bestanden en paden |
| `gegl.scm` | GEGL-buffer en kleurhelpers |
| `images.scm` | Helpers op afbeeldingsniveau (`image-get-open-list`, enz.) |
| `layers.scm` | Laag- en tekenhulpjes |
| `parasites.scm` | Parasitaire lees-/schrijfhelpers |
| `paths.scm` | Pad- en vectorhelpers |

Deze zijn allemaal beschikbaar in elke Scheme-plug-in of in de Scheme Console.

## Zoeken en filteren

- **Zoekvak**: filtert op naam terwijl u typt (niet-hoofdlettergevoelige subtekenreeksovereenkomst).
- **Soort filter**: resultaten beperken tot `procedure`, `variable`, of `syntax`.

Als u op een item klikt, wordt de volledige docstring weergegeven, evenals de bibliotheek waar het vandaan komt.

## De Stdlib als wrappers

De hulpprogrammabibliotheken zijn een praktische toepassing van het inpakpatroon: elke helper geeft een duidelijke naam aan een bewerking op laag niveau, verbergt de boilerplate en biedt één plek om bij te werken als de onderliggende opdracht verandert. Als je de ontwerpbenadering erachter wilt begrijpen, bekijk dan de **[Wrapping](@@LUMI_TOKEN_11@@)** tutorial.

## Relatie met de procedurebrowser

De Utility Browser staat los van **Filters → Script-Fu → Console → Browse** (de Procedurebrowser). De Procedurebrowser geeft een overzicht van PDB-geregistreerde procedures. De Utility Browser geeft hulpdefinities weer die opzettelijk *buiten* de PDB leven: ze zijn alleen voor Scheme en hebben geen C-binding.