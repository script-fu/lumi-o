---
title: "Debian installeren"
type: docs
url: "hub/install-linux/Installing-Debian"
---
Dit document schetst het proces dat wordt gebruikt om Debian Stable te installeren als het Lumi·o-ontwikkelingsbesturingssysteem. Het kan nuttig zijn voor anderen die een soortgelijke omgeving opzetten.

Debian Stable werd geselecteerd omdat Lumi ernaar streeft betrouwbaar te bouwen op een voorspelbaar platform voor de lange termijn. De ontwikkeling van GIMP is gericht op Debian Testing, waardoor Debian Stable een nauw op elkaar afgestemd basissysteem is.

Als u van Windows komt, is de belangrijkste conceptuele verandering dat de meeste software-installatie en -configuratie plaatsvindt via pakketbeheerders en eenvoudige terminalopdrachten in plaats van downloadbare installatieprogramma's.

## Voor wie is deze gids bedoeld

Deze handleiding documenteert een werkende Debian Stable-installatie die wordt gebruikt voor de ontwikkeling van Lumi. Het is geen algemene Linux-installatiehandleiding.

Het is het nuttigst voor:

- artiesten die overstappen van Windows en een voorspelbare Linux-installatie willen
- ontwikkelaars die Lumi vanaf de bron bouwen
- gebruikers die liever een bekende werkomgeving reproduceren dan hun eigen systeemconfiguratie te ontwerpen

Basiskennis van schijfpartitionering en eenvoudig gebruik van de opdrachtregel wordt verondersteld.

## Maak een back-up van uw gegevens

Voordat u Debian installeert, maakt u een volledige back-up van uw thuismap op een externe schijf. Voeg eventuele extra gegevensmappen toe die u wilt behouden.

Opmerking: in Linux vertegenwoordigt `~` uw thuismap.

Als je Git-opslagplaatsen gebruikt, push dan alle belangrijke wijzigingen naar hun oorsprong, zodat ze na installatie gemakkelijk kunnen worden hersteld. Deze stap is alleen relevant als je Git al gebruikt.

## Maak een partitie

Maak ruimte vrij op uw primaire schijf voor Debian. Er bestaan ​​veel handleidingen en hulpmiddelen voor deze stap, waaronder GParted. Afhankelijk van uw opstelling kunt u:

- een bestaande Windows-partitie verkleinen voor dual-boot
- hergebruik een bestaande Linux-partitie
- nieuwe Linux voorbereiden en partities wisselen

Als u het niet zeker weet, raadpleeg dan hardwarespecifieke handleidingen voordat u wijzigingen aanbrengt, aangezien de partitiestappen aanzienlijk variëren tussen systemen.


## Maak een Debian-installatie-USB

Ervan uitgaande dat er al een doelpartitie en swapruimte bestaan:

1. Download de Debian ISO van de officiële website: https://www.debian.org/
2. Gebruik in Windows BalenaEtcher om de ISO naar een USB-station te schrijven.
3. Gebruik op Linux een opdrachtregelprogramma zoals `dd` om een ​​opstartbare USB te maken.

## Installeer Debian

1. Plaats het USB-station.
2. Start opnieuw op en druk op de opstartmenutoets (gewoonlijk `F2`, `F12`, `Esc` of `Del`) tijdens het opstarten.
3. Selecteer het USB-apparaat.
4. Kies een niet-grafisch installatieprogramma.
5. Laat het root-wachtwoord leeg wanneer daarom wordt gevraagd, zodat het installatieprogramma sudo-toegang tot uw gebruikersaccount verleent.
6. Handmatig verdelen:

   - Bestandssysteem: ext4 (journaling)
   - Swap: bestaande swappartitie
   - Mountpunt: `/`
   - Label: `linux`
   - Hostnaam: systeemnaam weergegeven als `user@hostname`
   - Gebruikersaccount: uw volledige naam
   - Gebruikersnaam: terminal-inlognaam

7. Selecteer **Cinnamon** als bureaubladomgeving.
8. Voltooi de installatie en start opnieuw op in Debian Stable.

## Systeeminstellingen

### Schermschalen

Debian Stable gaat momenteel inconsistent om met fractionele schaling, vooral op 4K-beeldschermen. In plaats van de weergaveresolutie te verlagen, kunt u de interface-elementen rechtstreeks aanpassen.

Aanbevolen aanpassingen:- Vermijd fractionele weergaveschaling.
- Menu → Lettertypeselectie → Lettertype-instellingen → Tekstschaalfactor: `2.5`
- Bureaubladlettertype: `14`
- Paneel → Aanpassen → Paneelhoogte: `60`
- Paneeluiterlijk → Symbolische pictogramgrootte rechterzone: `48px`
- Muis en touchpad → Aanpassing van de aanwijzergrootte
- Bureaublad (klik met de rechtermuisknop) → Aanpassen → Groter pictogramformaat

Firefox-aanpassing:

- Adresbalk → `about:config`
- Stel `layout.css.devPixelsPerPx` in op `1`

### Terminal

Terminalvoorkeuren configureren:

1. Menu → Terminal → Bewerken → Voorkeuren
2. Tekst → Oorspronkelijke grootte: `140 columns`, `40 rows`
3. Tekst → Aangepast lettertype: `Monospace 10`
4. Kleuren → Ingebouwde schema's → Solarized Dark

## Gegevens herstellen

Herstel indien nodig geback-upte bestanden naar de thuismap, bijvoorbeeld:

- `Backup/Home/Artwork` → `~/Artwork`
- `Backup/Home/code` → `~/code`
- `Backup/Home/Desktop` → `~/Desktop`
- `Backup/Home/.ssh` → `~/.ssh`
- `Backup/Home/.config/lumi` → `~/.config/lumi`

Opmerking: mappen die beginnen met `.` zijn verborgen configuratiemappen in Linux.

## Optioneel: Git-installatie

Alleen vereist als u Lumi wilt bouwen of opslagplaatsen wilt herstellen.

### Git installeren

```bash
sudo apt install git
```

Configureer uw identiteit:

```bash
git config --global --edit
```

#### GitLab-toegang

Herstel toegang tot de repository tot GitLab of GitHub:

1. Wijzig de machtigingen voor het SSH-sleutelbestand: `chmod 600 ~/.ssh/id_rsa`
2. Voeg de gebruiker toe aan de nieuwe Git-installatie: `ssh-add ~/.ssh/id_rsa`
3. Test de verbinding: `ssh -T git@ssh.gitlab.gnome.org` of `ssh -T git@github.com`

Haal voor elke repository de oorsprong op en reset de lokale vertakking zodat deze overeenkomt:

```bash
git reset --hard remote-name/branch-name
git clean -df
```

Voer `git status` uit om te bevestigen dat de opslagplaatsen schoon zijn.

We hebben nu een nieuw besturingssysteem waarin alle gegevens en opslagplaatsen zijn hersteld. Deze opstelling weerspiegelt een bekende werkomgeving die wordt gebruikt voor Lumi-ontwikkeling en kan indien nodig worden aangepast aan individuele workflows.

## Bouw Lumi na de installatie van het besturingssysteem

Lumi-buildscripts bevinden zich in:

`~/code/lumi-dev/build/lumi/scripts`.

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# Install dependencies once
sudo bash lumi-install-packages.sh

# First full setup build
bash lumi-build-script.sh --scope setup --dir lumi-dev

# Regular rebuild after code changes
bash lumi-build-script.sh --scope build --dir lumi-dev

# Quick compile path
bash lumi-build-script.sh --scope compile --dir lumi-dev

# Launch Lumi
bash lumi-launch-active.sh lumi-dev
```