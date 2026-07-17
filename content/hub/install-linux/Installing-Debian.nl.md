---
title: "Debian installeren"
type: docs
url: "hub/install-linux/Installing-Debian"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 42b2f95f8ff71be8eff6777dfd9808855f99f943c55575079566588a08fd8fcd
---

Dit document beschrijft het proces dat wordt gebruikt om Debian Stable te installeren als ontwikkelbesturingssysteem voor Lumi-o. Het kan nuttig zijn voor anderen die een vergelijkbare omgeving opzetten.

Debian Stable is gekozen omdat Lumi betrouwbaar wil bouwen op een voorspelbaar platform voor de lange termijn. GIMP-ontwikkeling richt zich op Debian Testing, waardoor Debian Stable een nauw aansluitend basissysteem is.

Lumi werkt het best op Debian met Cinnamon (X11) en wordt in die omgeving ontwikkeld en getest. Cinnamon biedt een vertrouwde, Windows-achtige desktopworkflow, terwijl X11 de meest stabiele omgeving voor Lumi-ontwikkeling biedt.

Als u van Windows komt, is de belangrijkste conceptuele verandering dat de meeste software-installatie en -configuratie plaatsvindt via pakketbeheerders en eenvoudige terminalopdrachten, in plaats van downloadbare installatieprogramma's.

## Voor wie is deze gids bedoeld

Deze gids documenteert een werkende Debian Stable-installatie die wordt gebruikt voor Lumi-ontwikkeling. Het is geen algemene Linux-installatiehandleiding.

Het is vooral nuttig voor:

- artiesten die overstappen van Windows en een voorspelbare Linux-installatie willen
- ontwikkelaars die Lumi vanaf de bron bouwen
- gebruikers die liever een bekende werkomgeving reproduceren dan hun eigen systeemconfiguratie ontwerpen

Basiskennis van schijfpartitionering en eenvoudig gebruik van de opdrachtregel wordt verondersteld.

## Maak een back-up van uw gegevens

Maak vóór de installatie van Debian een volledige back-up van uw thuismap op een externe schijf. Neem ook extra datamappen op die u wilt behouden.

Opmerking: in Linux staat `~` voor uw thuismap.

Als u Git-repository's gebruikt, push dan belangrijke wijzigingen naar de remotes, zodat u ze na de installatie eenvoudig kunt herstellen. Deze stap is alleen relevant als u Git al gebruikt.

## Maak een partitie

Maak ruimte vrij op uw primaire schijf voor Debian. Er bestaan veel handleidingen en hulpmiddelen voor deze stap, waaronder GParted. Afhankelijk van uw opstelling kunt u:

- een bestaande Windows-partitie verkleinen voor dual boot
- een bestaande Linux-partitie hergebruiken
- nieuwe Linux- en swappartities aanmaken

Als u het niet zeker weet, raadpleeg dan hardwarespecifieke handleidingen voordat u wijzigingen aanbrengt, want partitioneringsstappen verschillen sterk per systeem.


## Maak een Debian-installatie-USB

Ervan uitgaande dat er al een doelpartitie en swapruimte bestaan:

1. Download de Debian ISO van de officiële website: https://www.debian.org/
2. Gebruik op Windows BalenaEtcher om de ISO naar een USB-station te schrijven.
3. Gebruik op Linux een opdrachtregelprogramma zoals `dd` om een opstartbare USB te maken.

## Installeer Debian

1. Plaats het USB-station.
2. Start opnieuw op en druk tijdens het opstarten op de opstartmenutoets (meestal `F2`, `F12`, `Esc` of `Del`).
3. Selecteer het USB-apparaat.
4. Kies een niet-grafisch installatieprogramma.
5. Laat het root-wachtwoord leeg wanneer daarom wordt gevraagd, zodat het installatieprogramma sudo-toegang aan uw gebruikersaccount verleent.
6. Partitioneer handmatig:

   - Bestandssysteem: ext4 (journaling)
   - Swap: bestaande swappartitie
   - Mountpunt: `/`
   - Label: `linux`
   - Hostnaam: systeemnaam weergegeven als `user@hostname`
   - Gebruikersaccount: uw volledige naam
   - Gebruikersnaam: terminal-inlognaam

7. Het Debian-installatieprogramma biedt in dit stadium een keuze uit desktopomgevingen; selecteer **Cinnamon** voor de door Lumi aanbevolen configuratie.
8. Voltooi de installatie en start opnieuw op in Debian Stable.

## Systeeminstellingen

### Schermschalen

Debian Stable gaat momenteel inconsistent om met fractionele schaling, vooral op 4K-beeldschermen. Pas in plaats van de weergaveresolutie te verlagen de interface-elementen rechtstreeks aan.

Aanbevolen aanpassingen:

- Vermijd fractionele weergaveschaling.
- Menu → Font Selection → Font Settings → Text Scaling Factor: `2.5`
- Desktop Font: `14`
- Panel → Customize → Panel Height: `60`
- Panel Appearance → Right Zone Symbolic Icon Size: `48px`
- Muis en touchpad → Grootte aanwijzer
- Desktop (right-click) → Customize → Larger icon size

Firefox-aanpassing:

- Address bar → `about:config`
- Stel `layout.css.devPixelsPerPx` in op `1`

### Terminal

Configureer terminalvoorkeuren:

1. Menu → Terminal → Edit → Preferences
2. Text → Initial size: `140 columns`, `40 rows`
3. Text → Custom font: `Monospace 10`
4. Colours → Built-in schemes → Solarized Dark

## Gegevens herstellen

Herstel indien nodig geback-upte bestanden in de thuismap, bijvoorbeeld:

- `Backup/Home/Artwork` → `~/Artwork`
- `Backup/Home/code` → `~/code`
- `Backup/Home/Desktop` → `~/Desktop`
- `Backup/Home/.ssh` → `~/.ssh`
- `Backup/Home/.config/lumi` → `~/.config/lumi`

Opmerking: mappen die beginnen met `.` zijn verborgen configuratiemappen in Linux.

## Optioneel: Git instellen

Alleen nodig als u Lumi wilt bouwen of repository's wilt herstellen.

### Installeer Git

```bash
sudo apt install git
```

Configureer uw identiteit:

```bash
git config --global --edit
```

#### GitLab-toegang

Herstel repositorytoegang voor GitLab of GitHub:

1. Wijzig de machtigingen van het SSH-sleutelbestand: `chmod 600 ~/.ssh/id_rsa`
2. Voeg de sleutel toe aan de ssh-agent: `ssh-add ~/.ssh/id_rsa`
3. Test de verbinding: `ssh -T git@ssh.gitlab.gnome.org` of `ssh -T git@github.com`

Haal voor elke repository de remotes op en reset de lokale branch zodat deze overeenkomt:

```bash
git reset --hard remote-name/branch-name
git clean -df
```

Voer `git status` uit om te bevestigen dat de repository's schoon zijn.

U hebt nu een nieuw besturingssysteem met herstelde gegevens en repository's. Deze opstelling weerspiegelt een bekende werkomgeving voor Lumi-ontwikkeling en kan indien nodig worden aangepast aan individuele workflows.

## Lumi bouwen na OS-installatie

Lumi-buildscripts staan in:

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

