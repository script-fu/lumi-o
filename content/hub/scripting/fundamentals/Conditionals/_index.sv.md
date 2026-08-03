---
title: "Villkor"
type: docs
weight: 2
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 8e9a64dd1bc1445c996fe17ce6b666b8d597ab16040cf0ef0876232026ff11b2
url: "hub/scripting/fundamentals/Conditionals"
---
Villkor är en grundläggande del av programmering: de låter skript fatta beslut och styra flödet utifrån specifika kriterier. I Scheme, baserat på programmespråket Scheme, hjälper villkor dig att skriva dynamiska, intelligenta skript som anpassar sig till förändrade indata, miljöer eller användaråtgärder.

### Villkorens roll i Scheme

Villkor fyller flera viktiga funktioner i dina skript:
- **Styra logiken:** De kör olika kodstycken beroende på om vissa villkor är sanna eller falska.
- **Ökad flexibilitet:** Genom att reagera dynamiskt på indata eller tillstånd hjälper de skriptet att hantera många scenarier.
- **Förenkla komplexitet:** De delar upp beslut i hanterbara strukturer och gör koden lättare att läsa, felsöka och underhålla.

### Tillgängliga villkorskonstruktioner

Scheme erbjuder flera villkorskonstruktioner, var och en anpassad till olika logiska behov:
- **`if`:** För enkla binära beslut — ett kodblock om villkoret är sant, ett annat om det är falskt.
- **`cond`:** En kraftfull konstruktion med flera grenar för att hantera flera villkor på ett tydligt, strukturerat sätt.
- **`and` / `or`:** Logiska operatorer som utvärderar kombinationer av villkor för mer komplex beslutsgivning.
- **`else`:** Ett reservfall som definierar beteendet när inget av de angivna villkoren uppfylls.

### Så fungerar villkor

Villkor omfattar vanligtvis:
1. **Utvärdera ett villkor:** Ett testuttryck avgör om ett villkor är sant eller falskt.
2. **Förgrenad körning:** Utifrån utvärderingen väljer skriptet vilket kodblock som ska köras.
3. **Returnera ett värde (valfritt):** I vissa fall ger villkor också ett värde som andra delar av skriptet kan använda.