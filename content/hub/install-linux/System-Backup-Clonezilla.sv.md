---
title: "Systemsäkerhetskopiering med Clonezilla"
type: docs
url: "hub/install-linux/System-Backup-Clonezilla"
---
Det är vanligt att du säkerhetskopierar dina viktiga filer för att återgå till tidigare versioner eller ersätta skadad data. En annan viktig typ av säkerhetskopiering är dock en **diskklon**, en komplett säkerhetskopia av ditt systems tillstånd.

När du väl har installerat ditt system och fungerar bra är det avgörande att skapa en fullständig säkerhetskopia för att återställa din miljö om en katastrof skulle inträffa. Denna säkerhetskopia kompletterar att regelbundet spara dina arbetsdata.

[Clonezilla](https://clonezilla.org/) är en gratis programvara för diskavbildning och kloning med öppen källkod. Det låter användare skapa och återställa fullständiga säkerhetskopior av sin dators hårddisk, vilket gör det till ett populärt verktyg för både IT-proffs och hemanvändare.

Det är alltid bättre att ha en backup och inte behöva den än att behöva en backup och inte ha den.


## Nyckelfunktioner i Clonezilla

- **Diskavbildning**: Clonezilla skapar en exakt kopia av en hårddisk, inklusive operativsystem, applikationer och data.
- **Säkerhetskopiera och återställa**: Det gör att du kan skapa en säkerhetskopia av en hårddisk och återställa den i händelse av fel eller migrering till en ny enhet.
- **Gratis och öppen källkod**: Clonezilla är helt gratis att använda, och källkoden är tillgänglig för modifiering och anpassning.


## Använda Clonezilla för att säkerhetskopiera

### Förberedelsesteg

Du behöver en USB-enhet för Clonezilla och en extern hårddisk som är större än den interna enhet du tänker klona.

Dessa steg förenklar processen baserat på [official guide](https://clonezilla.org//fine-print-live-doc.php?path=./clonezilla-live/doc/01_Save_disk_image/00-boot-clonezilla-live-cd.doc#00-boot-clonezilla-live-cd.doc). Det är en bra idé att granska hela guiden, som inkluderar skärmdumpar för ökad tydlighet.

1. **Skapa en Clonezilla Live USB eller CD/DVD**: Följ de detaljerade instruktionerna på Clonezilla [website](https://clonezilla.org/liveusb.php) för att skapa en startbar USB eller CD/DVD.

2. **Anslut din externa backup-enhet**: Anslut din externa enhet och se till att den känns igen av ditt system. Detta kommer att vara destinationen för din säkerhetskopiering.

3. **Verifiera din partitionslayout**: Använd kommandot `lsblk` i en terminal för att verifiera partitionslayouten för din primära hårddisk. Notera den primära enhetens namn.

4. **Starta från Clonezilla Live USB Drive**: Starta om din dator och starta om från Clonezilla-mediet du skapade. Du kan behöva komma åt dina BIOS/UEFI-inställningar (vanligtvis genom att trycka på F2, F12, ESC eller DEL under uppstart) och justera startordningen för att prioritera USB-enheten.



### Säkerhetskopiera med Clonezilla

1. **Välj säkerhetskopieringsläge**: När Clonezilla startar, välj "enhet-enhet". Detta läge låter dig klona din interna enhet direkt till en extern enhet.

2. **Välj källenhet**: Välj den primära interna enheten.

3. **Välj målenhet**: Välj din externa backup-enhet som målenhet. Var försiktig när du väljer enhet för att undvika att skriva över viktig data. Se till att målenheten är lika stor som eller större än källenheten.

4. **Starta säkerhetskopieringsprocessen**: Clonezilla kommer att starta säkerhetskopieringsprocessen. Beroende på storleken på din partition och hastigheten på dina enheter kan detta ta allt från flera minuter till några timmar.

5. **Märk din säkerhetskopia**: När säkerhetskopieringen är klar, märk USB-enheten och den externa hårddisken med datum och systemet du säkerhetskopierade. Förvara dem på ett säkert ställe.

---

### Återställer från säkerhetskopia

Om du behöver återställa ditt Debiansystem från säkerhetskopian, följ dessa steg:

1. **Starta från Clonezilla Media**: Sätt i Clonezilla USB och starta från den, följ samma steg som under säkerhetskopieringsprocessen.2. **Välj återställningsläge**: Välj läget "enhet-enhet" igen, men den här gången kommer du att återställa från säkerhetskopian. Detta kommer att kopiera all data från din externa enhet tillbaka till din interna enhet.

3. **Välj källenhet**: Välj din externa enhet där säkerhetskopian lagras.

4. **Välj målenhet**: Välj den interna enhet där du vill återställa säkerhetskopian.

5. **Starta återställningsprocessen**: Clonezilla kommer att påbörja återställningsprocessen. Precis som med säkerhetskopieringen kommer den tid som krävs att bero på storleken på enheten och hastigheten på din hårdvara.

---

## Slutanteckningar

Disksäkerhetskopior med Clonezilla säkerställer att hela ditt system – operativsystem, inställningar och applikationer – bevaras. Med minimal ansträngning kan du skydda ditt system från katastrofala fel och minimera driftstopp i händelse av en krasch.

Kom ihåg att **säkerhetskopior är viktiga**. Uppdatera dina säkerhetskopior regelbundet och testa dem regelbundet för att säkerställa att du kan återställa ditt system när det behövs.

Efter uppstart kan du koppla in din externa backup-enhet och inspektera dess partitionsstruktur med hjälp av verktyget Disks i Linux. Säkerhetskopieringsenheten bör spegla den interna enhetens struktur, med samma partitioner och lite oanvänt utrymme om den externa enheten är större.