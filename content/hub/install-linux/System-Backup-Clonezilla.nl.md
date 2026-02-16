---
title: "Systeemback-up met Clonezilla"
type: docs
url: "hub/install-linux/System-Backup-Clonezilla"
---
Het is gebruikelijk om een ​​back-up te maken van uw belangrijke bestanden om terug te keren naar eerdere versies of beschadigde gegevens te vervangen. Een ander essentieel type back-up is echter een **schijfkloon**, een volledige back-up van de systeemstatus.

Zodra uw systeem is ingesteld en goed werkt, is het maken van een volledige back-up van cruciaal belang voor het herstellen van uw omgeving in geval van een ramp. Deze back-up vormt een aanvulling op het regelmatig opslaan van uw werkgegevens.

[Clonezilla](https://clonezilla.org/) is een gratis en open-source software voor het maken en klonen van schijven. Hiermee kunnen gebruikers volledige back-ups van de harde schijf van hun computer maken en herstellen, waardoor het een populair hulpmiddel is voor zowel IT-professionals als thuisgebruikers.

Het is altijd beter om een ​​back-up te hebben en deze niet nodig te hebben dan een back-up nodig te hebben en deze niet te hebben.


## Belangrijkste kenmerken van Clonezilla

- **Disk Imaging**: Clonezilla maakt een exacte kopie van een harde schijf, inclusief het besturingssysteem, applicaties en gegevens.
- **Back-up en herstel**: Hiermee kunt u een back-upimage van een harde schijf maken en deze herstellen in geval van een storing of migratie naar een nieuwe schijf.
- **Gratis en open source**: Clonezilla is volledig gratis te gebruiken en de broncode is beschikbaar voor wijziging en aanpassing.


## Clonezilla gebruiken om een back-up te maken

### Voorbereidingsstappen

Voor Clonezilla hebt u een USB-station nodig en een externe harde schijf die groter is dan de interne schijf die u wilt klonen.

Deze stappen vereenvoudigen het proces op basis van de [official guide](https://clonezilla.org//fine-print-live-doc.php?path=./clonezilla-live/doc/01_Save_disk_image/00-boot-clonezilla-live-cd.doc#00-boot-clonezilla-live-cd.doc). Het is een goed idee om de volledige handleiding door te nemen, inclusief schermafbeeldingen voor extra duidelijkheid.

1. **Maak een Clonezilla Live USB of CD/DVD**: Volg de gedetailleerde instructies op de Clonezilla [website](https://clonezilla.org/liveusb.php) om een ​​opstartbare USB of CD/DVD te maken.

2. **Verbind uw externe back-upschijf**: Sluit uw externe schijf aan en zorg ervoor dat deze door uw systeem wordt herkend. Dit is de bestemming voor uw back-up.

3. **Verifieer uw partitie-indeling**: gebruik de opdracht `lsblk` in een terminal om de partitie-indeling van uw primaire harde schijf te verifiëren. Noteer de naam van het primaire apparaat.

4. **Opstarten vanaf Clonezilla Live USB Drive**: Start uw computer opnieuw op en start op vanaf de Clonezilla-media die u hebt gemaakt. Mogelijk moet u toegang krijgen tot uw BIOS/UEFI-instellingen (meestal door tijdens het opstarten op F2, F12, ESC of DEL te drukken) en de opstartvolgorde aanpassen om prioriteit te geven aan het USB-station.



### Back-up met Clonezilla

1. **Selecteer Back-upmodus**: Zodra Clonezilla opstart, kiest u de modus "apparaat-apparaat". In deze modus kunt u uw interne schijf rechtstreeks naar een extern apparaat klonen.

2. **Selecteer het bronapparaat**: Kies de primaire interne schijf.

3. **Selecteer het doelapparaat**: Kies uw externe back-upschijf als doelapparaat. Wees voorzichtig bij het selecteren van het apparaat om te voorkomen dat belangrijke gegevens worden overschreven. Zorg ervoor dat het doelstation gelijk of groter is dan het bronstation.

4. **Start het back-upproces**: Clonezilla start het back-upproces. Afhankelijk van de grootte van uw partitie en de snelheid van uw schijven kan dit enkele minuten tot enkele uren duren.

5. **Label uw back-up**: Nadat de back-up is voltooid, labelt u het USB-station en de externe harde schijf met de datum en het systeem waarvan u een back-up hebt gemaakt. Bewaar ze op een veilige plaats.

---

### Herstellen vanuit een back-up

Als u uw Debian-systeem vanaf de back-up moet herstellen, volgt u deze stappen:

1. **Opstarten vanaf Clonezilla Media**: Plaats de Clonezilla USB en start ervan op, volgens dezelfde stappen als tijdens het back-upproces.2. **Selecteer de herstelmodus**: Kies opnieuw de modus "apparaat-apparaat", maar deze keer herstelt u vanaf de back-upimage. Hiermee worden alle gegevens van uw externe schijf terug naar uw interne schijf gekopieerd.

3. **Selecteer het bronapparaat**: Kies uw externe schijf waarop de back-up is opgeslagen.

4. **Selecteer het doelapparaat**: Selecteer de interne schijf waarop u de back-up wilt herstellen.

5. **Start het herstelproces**: Clonezilla begint het herstelproces. Net als bij de back-up is de benodigde tijd afhankelijk van de grootte van de schijf en de snelheid van uw hardware.

---

## Laatste opmerkingen

Schijfback-ups met Clonezilla zorgen ervoor dat uw hele systeem (besturingssysteem, instellingen en applicaties) behouden blijft. Met minimale inspanning kunt u uw systeem beschermen tegen catastrofale storingen en de uitvaltijd bij een crash tot een minimum beperken.

Onthoud: **back-ups zijn essentieel**. Werk uw back-ups regelmatig bij en test ze regelmatig om er zeker van te zijn dat u uw systeem kunt herstellen wanneer dat nodig is.

Na het opstarten kunt u uw externe back-upschijf aansluiten en de partitiestructuur inspecteren met behulp van het hulpprogramma Schijven in Linux. De back-upschijf moet de structuur van de interne schijf weerspiegelen, met dezelfde partities en wat ongebruikte ruimte als de externe schijf groter is.