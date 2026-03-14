---
title: "Filåterställning"
type: docs
---
Lumi har två oberoende återställningssystem (automatiska bakgrundssparningar och manuella inkrementella kontrollpunkter), båda tillgängliga från en enda dialogruta.

## Åtkomst

**Fil** → **Återställ bild**

Dialogrutan öppnas i förväg med återställningstillstånd för den för närvarande öppna filen. Använd filväljaren högst upp för att växla till en annan `.lum` fil.

---

## Autospara

Lumi sparar en bakgrundsbild av ditt arbete med jämna mellanrum medan du redigerar. Automatiska sparade filer skrivs till en **separat cachekatalog**, vilket lämnar den fungerande `.lum`-filen orörd:

```
~/.cache/lumi/autosave/~home~user~projects~my-painting.lum/
```

Sökvägskodningen använder `~` som en separator för att skapa en unik cachekatalog per fil. Detta innebär att autosave är tillgängliga även om själva projektfilen försvinner eller är skadad.

- **Frekvens**: Kan konfigureras i **Redigera** → **Inställningar** → **Prestanda** → Autosparaintervall.
- **Lagringsplats**: Ställ även in i Inställningar → Prestanda.
- **Syfte**: Kraschåterställning. Fliken Autospara i dialogrutan Återställ bild visar tillgängliga autosparatillstånd med tidsstämplar.

När du öppnar en fil som har nyare autospara-data, meddelar Lumi dig vid öppning.

---

## Inkrementella besparingar

Inkrementell besparing är ett manuellt kontrollsystem lagrat **inuti projektfilen** under `recovery/`. Strukturen är:

```
my-painting.lum/recovery/
  └── primary-01.lum/       (full baseline, created on first Ctrl+I)
      ├── delta-0001.lum/   (Ctrl+I checkpoint, only modified buffers)
      ├── delta-0002.lum/
      └── ...
```

En ny `primary-NN.lum/` baslinje skrivs efter **Arkiv → Spara**. Efterföljande **File → Save Increment**-tryckningar (`Ctrl+I`) skapar `delta-NNNN.lum/` underkataloger som endast innehåller de buffertar som ändrades sedan den senaste baslinjen. Autospara delta och manuella spara delta använder separata räknare så att de inte stör varandras historia.

Spara inkrement är **alltid tillgänglig** för sparade `.lum`-filer:

1. Använd **Arkiv** → **Spara** (`Ctrl+S`) för att skapa eller uppdatera huvudprojektfilen.
2. Använd **File** → **Save Increment** (`Ctrl+I`) för att skapa en återställningskontrollpunkt.
3. Efter ytterligare en fullständig **File** → **Spara**, skriver nästa `Ctrl+I` en ny `primary-NN.lum/` baslinje innan nya deltan skapas.

Återställda filer namngivna med prefixet `RECOVERED_` måste sparas normalt först innan Save Increment blir tillgängligt för dem.

När du öppnar en `.lum`-fil som har nyare inkrementella lagringar än den primära lagringen, visar Lumi en **Inkrementell lagring upptäckt**-prompt som erbjuder att ladda den senaste kontrollpunkten.

---

## Återställ bilddialog

Dialogrutan har tre flikar och två åtgärdsknappar.

### Fliken Autospara

Listar alla tillgängliga autosparatillstånd för den valda filen med tidsstämplar och miniatyrer (där sådana finns). Välj ett tillstånd och klicka på **Återställ** för att öppna det.

Använd den här fliken för att:
- Återhämta sig efter en krasch.
- Återgå till ett tidigare läge från samma session.

### Inkrementell flik

Listar alla kontrollpunktstillstånd lagrade i projektfilen. Varje post visar kontrollpunktens tidsstämpel. Välj en kontrollpunkt och klicka på **Återställ** för att öppna den.

Använd den här fliken för att:
- Återgå till en tidigare punkt i en session utan att ha sparat separata filer.
- Bläddra igenom versionshistoriken för ett projekt.

### Senaste flik

Standardfliken när dialogrutan öppnas. Identifierar automatiskt det senaste tillgängliga återställningstillståndet över både automatiska sparade och inkrementella kontrollpunkter och visar dess tidsstämpel. Klicka på **Återställ** för att ladda den omedelbart utan att bläddra i enskilda tillstånd.

---

## Knappar

| Knapp | Åtgärd |
|--------|--------|
| **Återhämta** | Öppnar det valda återställningsläget som en ny bild. |
| **Stäng** | Stänger dialogrutan utan att återställa. |
| **Rensa upp gamla stater...** | Öppnar en rensningsprompt (se nedan). |

---

## Rensa upp gamla stater

Ackumulerande återställningstillstånd över tid kan konsumera betydande diskutrymme. Knappen **Rensa upp gamla tillstånd...** (nedre till vänster i dialogrutan) öppnar en rensningsprompt för den aktiva fliken (Autospara eller Inkrementell).

Uppmaningen visar:
- Hur många fullständiga lagringar finns för filen.
- Det totala diskutrymmet de upptar.
- En snurrknapp **Behåll senaste** för att välja hur många besparingar som ska sparas.

Om du ställer in **Behåll senaste** till `0` raderas alla återställningstillstånd. Nästa `Ctrl+I` efter en fullständig rensning kommer att skriva en ny primär sparning.

---

## Startup Recovery

Vid start, om Lumi upptäcker att den senast öppnade filen har nyare autosave-data än den senaste fullständiga lagringen, visar den en återställningsprompt innan den laddas. Du kan acceptera (ladda in autospara) eller avvisa (öppna det primära sparandet som vanligt).