---
title: "Systemsäkerhetskopiering med Clonezilla"
type: docs
url: "hub/install-linux/System-Backup-Clonezilla"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: b3453289d7da56bb4fc9039616edb73e537acd6a722f0eb8a000e4a398016863
---

Det är vanligt att säkerhetskopiera viktiga filer för att återgå till tidigare versioner eller ersätta skadad data. En annan viktig typ av säkerhetskopia är dock en **diskklon** — en fullständig kopia av systemets tillstånd.

När systemet är konfigurerat och fungerar bra är en fullständig säkerhetskopia avgörande för att kunna återställa miljön vid en katastrof. Den kompletterar regelbundet sparande av arbetsdata.

[Clonezilla](https://clonezilla.org/) är gratis programvara med öppen källkod för diskavbildning och kloning. Den låter användare skapa och återställa fullständiga säkerhetskopior av datorns hårddisk — ett populärt verktyg för både IT-proffs och hemanvändare.

Det är alltid bättre att ha en säkerhetskopia man inte behöver än att behöva en man inte har.


## Viktiga funktioner i Clonezilla

- **Diskavbildning**: Clonezilla skapar en exakt kopia av en hårddisk, inklusive operativsystem, program och data.
- **Säkerhetskopiera och återställa**: du kan skapa en säkerhetskopia av en hårddisk och återställa den vid fel eller migrering till en ny enhet.
- **Gratis och öppen källkod**: Clonezilla är helt gratis; källkoden kan anpassas.


## Säkerhetskopiera med Clonezilla

### Förberedelsesteg

Du behöver en USB-enhet för Clonezilla och en extern hårddisk som är större än den interna enhet du tänker klona.

Dessa steg förenklar processen utifrån den [officiella guiden](https://clonezilla.org//fine-print-live-doc.php?path=./clonezilla-live/doc/01_Save_disk_image/00-boot-clonezilla-live-cd.doc#00-boot-clonezilla-live-cd.doc). Det är bra att också läsa hela guiden, som innehåller skärmdumpar.

1. **Skapa en Clonezilla Live USB eller CD/DVD**: följ de detaljerade instruktionerna på Clonezillas [webbplats](https://clonezilla.org/liveusb.php) för att skapa ett startbart USB-minne eller CD/DVD.

2. **Anslut den externa backup-enheten**: anslut den externa enheten och kontrollera att systemet känner igen den. Det blir målet för säkerhetskopian.

3. **Verifiera partitionslayouten**: använd kommandot `lsblk` i en terminal för att kontrollera partitionslayouten på den primära hårddisken. Notera namnet på primärenheten.

4. **Starta från Clonezilla Live USB**: starta om datorn och boota från Clonezilla-mediet du skapade. Du kan behöva gå in i BIOS/UEFI-inställningarna (vanligtvis F2, F12, ESC eller DEL vid uppstart) och ändra bootordningen så att USB prioriteras.



### Säkerhetskopiera med Clonezilla

1. **Välj backup-läge**: när Clonezilla har startat, välj läget "device-device". I det här läget klonar du den interna enheten direkt till en extern enhet.

2. **Välj källenhet**: välj den primära interna enheten.

3. **Välj målenhet**: välj den externa backup-enheten som mål. Var försiktig vid valet så att du inte skriver över viktig data. Målenheten måste vara minst lika stor som källenheten.

4. **Starta backup-processen**: Clonezilla startar backupen. Beroende på partitionsstorlek och diskhastighet kan det ta från några minuter till några timmar.

5. **Märk säkerhetskopian**: när backupen är klar, märk USB-enheten och den externa hårddisken med datum och vilket system du säkerhetskopierade. Förvara dem på en säker plats.

---

### Återställa från säkerhetskopia

Om du behöver återställa ditt Debian-system från säkerhetskopian, följ dessa steg:

1. **Starta från Clonezilla-media**: sätt i Clonezilla USB och boota från den, med samma steg som vid backup.

2. **Välj återställningsläge**: välj läget "device-device" igen, men återställ den här gången från backup-bilden. All data kopieras tillbaka från den externa enheten till den interna.

3. **Välj källenhet**: välj den externa enhet där säkerhetskopian finns.

4. **Välj målenhet**: välj den interna enhet dit du vill återställa.

5. **Starta återställningen**: Clonezilla påbörjar återställningen. Som vid backup beror tiden på diskstorlek och maskinhastighet.

---

## Avslutande anmärkningar

Disksäkerhetskopior med Clonezilla bevarar hela systemet — operativsystem, inställningar och program. Med lite arbete skyddar du systemet mot allvarliga fel och minskar driftstopp efter en krasch.

Kom ihåg: **säkerhetskopior är viktiga**. Uppdatera dem regelbundet och testa dem med jämna mellanrum så att du kan återställa systemet när det behövs.

Efter uppstart kan du ansluta den externa backup-enheten och inspektera partitionsstrukturen med verktyget Disks i Linux. Backup-enheten bör spegla den interna enhetens struktur, med samma partitioner och eventuellt outnyttjat utrymme om den externa enheten är större.

