---
title: "Verktygswebbläsare"
type: docs
---
Utility Browser låter dig utforska det inbyggda Scheme-verktyget stdlib som levereras med Lumi — utan att behöva lämna appen eller gräva igenom källfiler.

## Öppna Utility Browser

Gå till **Hjälp → Programmering → Utility Browser**.

Fönstret öppnas omedelbart; ingen plugin behöver laddas i förväg.

## Vad den visar

Webbläsaren listar varje procedur, variabel och syntaxform som exporteras av de sju verktygsbiblioteken som Lumi laddar automatiskt vid start:

| Bibliotek | Vad det omfattar |
|---|---|
| `common.scm` | Allmänna hjälpmedel (sträng, nummer, listverktyg) |
| `files.scm` | Fil- och sökvägshjälpmedel |
| `gegl.scm` | GEGL buffert och färghjälpmedel |
| `images.scm` | Hjälpare på bildnivå (`image-get-open-list`, etc.) |
| `layers.scm` | Lager- och dragbara hjälpredor |
| `parasites.scm` | Parasit läs/skrivhjälpare |
| `paths.scm` | Sökväg och vektorhjälpare |

Alla dessa är tillgängliga i alla Scheme-plugin-program eller i Scheme Console.

## Sökning och filtrering

- **Sökruta** — filtrerar efter namn medan du skriver (skiftlägesokänslig understrängsmatchning).
- **Snällt filter** — begränsa resultaten till `procedure`, `variable` eller `syntax`.

Om du klickar på en post visas dess fullständiga docstring och biblioteket den kommer ifrån.

## Stdlib som omslag

Verktygsbiblioteken är en praktisk tillämpning av omslagsmönstret: varje hjälpare ger ett tydligt namn till en lågnivåoperation, döljer plattan och ger en enda plats att uppdatera om det underliggande kommandot ändras. Om du vill förstå designstrategin bakom dem, se **[Wrapping](@@LUMI_TOKEN_11@@)** handledning.

## Relation till procedurbläddraren

Verktygswebbläsaren är separat från **Filter → Script-Fu → Konsol → Bläddra** (Procedurbläddraren). Procedurbläddraren listar PDB-registrerade procedurer. Verktygswebbläsaren listar hjälpdefinitioner som avsiktligt lever *utanför* PDB - de är endast för schema och har ingen C-bindning.