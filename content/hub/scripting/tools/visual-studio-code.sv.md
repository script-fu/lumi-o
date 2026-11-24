---
title: "Cursor"
type: docs
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: d3a7764cd3f00336fdcbb4d09a47fc3cabf4c99767cd005ccdc2fafdfa119117
url: "hub/scripting/tools/visual-studio-code"
---

## Vad är Cursor?

[Cursor](https://cursor.com/) är en kodredigerare med AI-hjälp i samma fönster. Den bygger på Visual Studio Code, så syntaxmarkering, tillägg, arbetsytor och Git följer med. Cursor är redigeraren som används här för Lumi-skript, eftersom den AI-hjälpen är mycket billigare.

- **Syntaxmarkering**: Markeringen färglägger koden. Fel syns tidigare, strukturen blir tydligare och komplex kod är lättare att läsa.
- **Tillägg**: Cursor kör tillägg från Visual Studio Code. Språk, tema eller verktyg: det finns ett tillägg som passar.
- **Versionshantering**: Git är inbyggt. Det är enkelt att följa ändringar och hantera grenar, med tjänster som [GitHub](https://github.com/) eller [GitLab](https://gitlab.com/).
- **AI i redigeraren**: Du kan be om en sökning, en omstrukturering eller en liten ändring bredvid filen som redan är öppen. Kostnaden för den hjälpen är låg nog att använda den i det vanliga skriptarbetet.

### Viktiga funktioner för nybörjare

- **Arbetsytor**: Cursor samlar flera projekt i "arbetsytor", som håller ihop relaterade filer och mappar. Navigering och projekthantering blir enklare.

- **Profiler**: Med profiler kan du ha olika upplägg för projekt eller uppgifter. En för webbutveckling och en annan för Scheme, var och en med egna tillägg, teman och inställningar.

### Kom igång med Cursor

Så börjar du använda Cursor:

- **Hämta och installera**: Gå till [Cursors webbplats](https://cursor.com/) och hämta Linux-versionen.

- **Utforska gränssnittet**: När du öppnar Cursor ser du flera paneler. Till vänster finns **aktivitetsfältet**, där du växlar mellan filer, sökning, Git och tillägg.

- **Öppna en fil eller en mapp**: Ställ in arbetsytan genom att lägga till projektmappen och spara den aktiva konfigurationen i en arbetsytefil.

En arbetsyta är bara en sparad mappvy plus redigerarens inställningar för projektet.

### Tillägg

- **Installera tillägg**: Klicka på ikonen **Tillägg** i aktivitetsfältet för att söka och installera verktyg som hjälper med Scheme. Det viktigaste är **"vscode-scheme"** av Allen Huang. Det ger syntaxmarkering och ett grundläggande stöd för Scheme, språket som Lumi-insticksprogram skrivs i.

Du kan också anpassa tillägget genom att redigera språkfilen och lägga till fler Scheme-nyckelord eller termer från Lumi-skript.

Om du sköter ett eget hjälpbibliotek går navigering och omstrukturering snabbare när funktionsnamnen finns i markeringen.

### Slutsats

Använd en redigerare som gör det lätt att söka, strukturera om och göra snabba ändringar, med AI nära till hands. Cursor plus ett Scheme-tillägg är grunden som används här.
