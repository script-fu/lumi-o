---
title: "Villkor"
type: docs
weight: 2
---
Villkor är en grundläggande del av programmering, vilket gör att manus kan fatta beslut och styra sitt flöde baserat på specifika kriterier. I Scheme, som är baserat på Scheme-programmeringsspråket, gör villkoren att du kan skapa dynamiska och intelligenta skript som anpassar sig till ändrade indata, miljöer eller användaråtgärder.

### Villkorens roll i schemat

Villkor tjänar flera viktiga syften i dina skript:
- **Directing Logic:** De låter dig köra olika delar av kod beroende på om vissa villkor är sanna eller falska.
- **Förbättrad flexibilitet:** Genom att svara dynamiskt på indata eller tillstånd hjälper villkor ditt skript att hantera en mängd olika scenarier.
- **Förenklar komplexiteten:** De delar upp beslutsfattande i hanterbara strukturer, vilket gör koden lättare att läsa, felsöka och underhålla.

### Typer av villkor tillgängliga

Schema tillhandahåller flera villkorade konstruktioner, var och en lämpad för olika logiska behov:
- **`if`:** För att göra enkla binära beslut, exekvera ett kodblock om ett villkor är sant och ett annat om det är falskt.
- **`cond`:** En kraftfull konstruktion med flera grenar för att hantera flera förhållanden på ett tydligt, strukturerat sätt.
- **`and` / `or`:** Logiska operatorer som utvärderar kombinationer av villkor, vilket möjliggör mer komplext beslutsfattande.
- **`else`:** En catch-all som definierar reservbeteende när inget av de angivna villkoren är uppfyllda.

### Hur villkor fungerar

Villkoren innefattar vanligtvis:
1. **Utvärdera ett villkor:** Ett testuttryck avgör om ett villkor är sant eller falskt.
2. **Branching Execution:** Baserat på utvärderingen väljer skriptet vilket kodblock som ska köras.
3. **Returnera ett värde (valfritt):** I vissa fall kan villkor även producera ett värde som andra delar av skriptet kan använda.