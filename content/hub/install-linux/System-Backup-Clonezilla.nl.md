---
title: "Systeemback-up met Clonezilla"
type: docs
url: "hub/install-linux/System-Backup-Clonezilla"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: b3453289d7da56bb4fc9039616edb73e537acd6a722f0eb8a000e4a398016863
---

Het is gebruikelijk om belangrijke bestanden te back-uppen om terug te keren naar eerdere versies of beschadigde gegevens te vervangen. Een ander essentieel type back-up is echter een **schijfkloon**: een volledige kopie van de staat van uw systeem.

Zodra uw systeem goed werkt, is een volledige back-up cruciaal om uw omgeving te herstellen als er iets misgaat. Deze back-up vult het regelmatig opslaan van uw werkgegevens aan.

[Clonezilla](https://clonezilla.org/) is gratis, open-source software voor schijfimaging en -klonen. Hiermee kunt u volledige back-ups van de harde schijf van uw computer maken en herstellen — een populair hulpmiddel voor IT-professionals en thuisgebruikers.

Het is altijd beter een back-up te hebben die u niet nodig hebt, dan een back-up nodig te hebben die u niet hebt.


## Belangrijkste functies van Clonezilla

- **Schijfimaging**: Clonezilla maakt een exacte kopie van een harde schijf, inclusief besturingssysteem, applicaties en gegevens.
- **Back-up en herstel**: u kunt een back-upimage van een schijf maken en deze herstellen bij storing of migratie naar een nieuwe schijf.
- **Gratis en open source**: Clonezilla is volledig gratis; de broncode is beschikbaar voor aanpassing.


## Clonezilla gebruiken voor back-up

### Voorbereidingsstappen

U hebt een USB-station voor Clonezilla nodig en een externe harde schijf die groter is dan de interne schijf die u wilt klonen.

Deze stappen vereenvoudigen het proces op basis van de [officiële gids](https://clonezilla.org//fine-print-live-doc.php?path=./clonezilla-live/doc/01_Save_disk_image/00-boot-clonezilla-live-cd.doc#00-boot-clonezilla-live-cd.doc). Lees ook de volledige gids door; die bevat schermafbeeldingen voor extra duidelijkheid.

1. **Maak een Clonezilla Live USB of CD/DVD**: volg de gedetailleerde instructies op de [website](https://clonezilla.org/liveusb.php) van Clonezilla om een opstartbare USB of CD/DVD te maken.

2. **Sluit uw externe back-upschijf aan**: sluit de externe schijf aan en controleer of uw systeem deze herkent. Dit wordt de bestemming voor uw back-up.

3. **Controleer uw partitie-indeling**: gebruik in een terminal het commando `lsblk` om de partitie-indeling van uw primaire harde schijf te controleren. Noteer de naam van het primaire apparaat.

4. **Start op vanaf de Clonezilla Live USB**: start uw computer opnieuw op vanaf het Clonezilla-medium dat u hebt gemaakt. Mogelijk moet u de BIOS/UEFI-instellingen openen (meestal met F2, F12, ESC of DEL tijdens het opstarten) en de opstartvolgorde aanpassen zodat USB voorrang krijgt.



### Back-up met Clonezilla

1. **Selecteer back-upmodus**: zodra Clonezilla is opgestart, kiest u de modus "device-device". In deze modus kloont u uw interne schijf rechtstreeks naar een extern apparaat.

2. **Selecteer het bronapparaat**: kies de primaire interne schijf.

3. **Selecteer het doelapparaat**: kies uw externe back-upschijf als doel. Wees voorzichtig bij de selectie om belangrijke gegevens niet te overschrijven. Het doelapparaat moet even groot of groter zijn dan het bronapparaat.

4. **Start het back-upproces**: Clonezilla start het back-upproces. Afhankelijk van de partitiegrootte en schijfsnelheid kan dit enkele minuten tot enkele uren duren.

5. **Label uw back-up**: label na voltooiing het USB-station en de externe schijf met de datum en het systeem dat u hebt geback-upt. Bewaar ze op een veilige plek.

---

### Herstellen vanuit back-up

Als u uw Debian-systeem vanuit de back-up moet herstellen, volgt u deze stappen:

1. **Start op vanaf Clonezilla-media**: plaats de Clonezilla USB en start ervan op, met dezelfde stappen als bij het maken van de back-up.

2. **Selecteer herstelmodus**: kies opnieuw de modus "device-device", maar herstel deze keer vanuit de back-upimage. Alle gegevens worden van de externe schijf terug naar de interne schijf gekopieerd.

3. **Selecteer het bronapparaat**: kies de externe schijf waarop de back-up staat.

4. **Selecteer het doelapparaat**: kies de interne schijf waarop u wilt herstellen.

5. **Start het herstelproces**: Clonezilla start het herstel. Net als bij de back-up hangt de duur af van schijfgrootte en hardwaresnelheid.

---

## Slotopmerkingen

Schijfback-ups met Clonezilla bewaren uw hele systeem — besturingssysteem, instellingen en applicaties. Met weinig moeite beschermt u uw systeem tegen ernstige storingen en beperkt u uitvaltijd na een crash.

Onthoud: **back-ups zijn essentieel**. Werk back-ups regelmatig bij en test ze periodiek, zodat u kunt herstellen wanneer dat nodig is.

Na het opstarten kunt u de externe back-upschijf aansluiten en de partitiestructuur bekijken met het hulpprogramma Disks in Linux. De back-upschijf moet de structuur van de interne schijf weerspiegelen, met dezelfde partities en eventueel ongebruikte ruimte als de externe schijf groter is.

