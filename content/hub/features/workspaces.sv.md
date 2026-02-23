---
title: "Arbetsytor"
type: docs
---
En arbetsyta är en sparad ögonblicksbild av hela din användargränssnittsmiljö: vilka paneler som är öppna och var, kanvasdekorationer och utfyllnad för både normal och helskärmsvy, det aktiva temat och ikonuppsättningen, verktygslådans layout, den aktiva paletten och dina verktygsinställningar. Lumi låter dig spara så många namngivna arbetsytor som du vill och växla mellan dem direkt - alla öppna bilder uppdateras på plats, ingen omstart krävs.

## Vad en arbetsyta sparar

Varje namngiven arbetsyta lagrar följande oberoende:

| Komponent | Vad det omfattar |
| :--- | :--- |
| **Layout** | Fönsterposition och storlek, dockningsarrangemang (vänster och höger panelkolumner, vilka paneler är öppna och i vilken ordning), enkel- mot flera fönsterläge, maximerat tillstånd, flikfälts synlighet och position |
| **Verktygsalternativ** | Aktuella inställningar för varje verktyg (borststorlek, hårdhet, varpbeteende, etc.) |
| **Indataenheter** | Konfiguration av inmatningsenhet: tryckkurvor, knapptilldelningar, axelmappningar för penna och andra enheter |
| **Canvasdekorationer** | Standardinställningar per arbetsyta för linjaler, rullningslister, stödlinjer, rutnät, markeringsmarkering, lagergräns och arbetsytas gräns – ställ in via **Inställningar → Bildfönster → Standardutseende** och **Utseende i helskärm**, oberoende för normal och helskärmsvy |
| **Canvasstoppning** | Utfyllnadsläge och färg per arbetsyta för normal och helskärmsvy — ställ in via **Inställningar → Bildfönster → Standardutseende** |
| **Tema och ikoner** | Aktivt tema, mörk/ljus färgvariant, ikonuppsättning, åsidosättande av ikonstorlek och teckensnittsskala |
| **Verktygslåda** | FG/BG-widgetposition (överst/botten/vänster/höger), FG/BG-skala, Wilber-maskotsynlighet, verktygsgrupphuvuden |

Den aktiva **paletten** och **verktygsförinställningen** spelas också in per arbetsyta och återställs när du byter.

> **Canvasdekorationer och stoppning** styrs av
> **Inställningar → Bildfönster → Avancerade fönsteralternativ → Standardutseende** (normal vy)
> och **Helskärmsutseende** (helskärmsvy). Justera dessa inställningar efter eget tycke,
> spara sedan arbetsytan. Menyalternativen **Visa** (linjaler, guider, etc.) är lokala för
> aktuellt bildfönster och sparas inte per arbetsyta.

### Liveuppdateringar på switch

När du byter arbetsytor uppdateras alla öppna bildfönster omedelbart – linjaler, guider, rullningslister, utfyllnadsfärg och alla andra vyinställningar ändras på plats utan att behöva stänga och öppna bilder igen.

## Åtkomst

**Redigera → Inställningar → Arbetsyta**

Den övre delen av sidan med arbetsytor med inställningar listar alla dina sparade arbetsytor och innehåller kontroller för att hantera dem.

## Skapa en arbetsyta

Ställ in dina paneler, verktyg och palett precis som du vill ha dem, sedan:

1. Öppna **Redigera → Inställningar → Arbetsyta**.
2. Klicka på **Spara layout som...**.
3. Ange ett namn och klicka på **Spara**.

Den nya arbetsytan visas i rullgardinsmenyn **Aktiv layout** och i menyn **Windows**.

## Byta arbetsytor

Det finns två sätt att byta:

- **Windows-meny**: Layoutnamnen visas under **Windows → Layout** för snabb åtkomst från arbetsytan.
- **Inställningar → Arbetsyta**: Välj en layout från rullgardinsmenyn **Aktiv layout** och klicka på **Ladda om layout**.

Bytet sker omedelbart — Lumi bygger om panellayouten, återställer verktygsalternativ, laddar om enhetsinställningar, uppdaterar canvasdekorationer, utfyllnad, tema och verktygslådan, allt utan att starta om.

## Hantera arbetsytor

Från **Redigera → Inställningar → Arbetsyta**:| Åtgärd | Effekt |
| :--- | :--- |
| **Spara layout** | Skriver över den aktuella arbetsytan med dina nuvarande inställningar. |
| **Spara layout som...** | Skapar en ny namngiven arbetsyta från dina nuvarande inställningar. |
| **Byt namn på layout...** | Byter namn på den valda arbetsytan. |
| **Ladda om layout** | Tillämpar den valda arbetsytan omedelbart. |
| **Ta bort layout...** | Tar bort den valda arbetsytan och dess filer permanent. |

## Beständighetsinställningar

Den nedre delen av inställningssidan för Workspace styr vad Lumi sparar automatiskt:

- **Spara fönsterpositioner vid utgång**: När på, skrivs docknings- och fönsterpositioner till disken varje gång du avslutar.
- **Öppna fönster på samma bildskärm**: Öppnar varje fönster på skärmen den var på under den senaste sessionen igen.
- **Spara verktygsalternativ vid utgång**: Sparar de aktuella verktygsinställningarna när du avslutar.
- **Spara inmatningsenhetsinställningar vid utgång**: Sparar pennan och enhetskonfigurationen när du avslutar.

Dessa inställningar gäller per arbetsyta – varje layout bibehåller sitt eget sparade tillstånd oberoende av varandra.

## Exempel på arbetsflöden

Några sätt artister kan använda flera arbetsytor på:

- **Målning** — stora penseldockor, varm stoppningsfärg (ställs in i Inställningar → Bildfönster → Standardutseende), din föredragna temavariant
- **Infärgning** — stödlinjer och kanvasgräns på, rullningslister på (ställs in i Inställningar → Standardutseende), neutral utfyllnadsfärg
- **Roughs** — dockar dolda, inga linjaler eller rutnät, mörk stoppning, kompakt ikonstorlek för att maximera arbetsytan
- **Fokus i helskärm** — olika stoppningsfärger och dekorationsinställningar i helskärmsutseende kontra standardutseende, så att växla helskärm ger en genuint annorlunda arbetsmiljö
- **Skript** — skriptpanelen öppen, bump i teckenstorlek för läsbarhet, annan ikonuppsättning