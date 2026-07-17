---
title: "Wacom-konfiguration"
type: docs
url: "hub/quick-start/Wacom-Configuration"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 3af66b116d9f361052280ac9636ae4b23bf5fc30f10f7227fb42d2d9e654ea95
---
För digital målning i Lumi rekommenderas en enkel **linjär tryckinställning**.

- Håll surfplattans tryckkurva linjär.
- Håll tryck/ingångskurvor i Lumi mestadels linjära.
- Forma känslan med själva borsten, eftersom borstdynamiken redan kan vara olinjär.

Vi rekommenderar att den linjära standardtryckkurvan bibehålls på drivrutinnivån för operativsystemet. Att sammansätta flera icke-linjära kurvor leder ofta till oförutsägbart indatabeteende; genom att hålla drivrutinen neutral säkerställer du att alla justeringar som görs inom Lumi-o förblir intuitiva och reproducerbara. En liten justering av Lumis globala kurva kan ändå vara rimlig när det behövs.

## Global Stylus Curve i Lumi

I Lumi, öppet:

Redigera → Inställningar → Indataenheter → Konfigurera surfplatta, penna och ytterligare enheter...

Här kan du ställa in den globala tryckkurvan för din penna.

## Wacom-pekring

Lumi stöder nu Wacom Touch Ring-ingång direkt, inklusive modifieringsbaserade ringingångar.

I samma enhetskonfigurationsdialogruta kan du tilldela ringsignaler per ingång, inklusive:

- Borststorlek
- Relativ storlek på borsten
- Borstvinkel
- Synvinkel
- Visa zoom

Obs! En bild måste vara aktiv för att Touch Ring ska påverka attribut. Ringen har som standard en relativ förändring av penselstorleken. För att förhindra oavsiktliga justeringar krävs en halvcirkelsvepning för att utlösa ett kommando (t.ex. ett halvt svep medurs fördubblar borststorleken).