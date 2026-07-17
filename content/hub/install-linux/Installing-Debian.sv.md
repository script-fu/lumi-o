---
title: "Installera Debian"
type: docs
url: "hub/install-linux/Installing-Debian"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 42b2f95f8ff71be8eff6777dfd9808855f99f943c55575079566588a08fd8fcd
---

Det här dokumentet beskriver processen för att installera Debian Stable som utvecklingsoperativsystem för Lumi-o. Det kan vara användbart för andra som sätter upp en liknande miljö.

Debian Stable valdes eftersom Lumi ska byggas tillförlitligt på en förutsägbar plattform på lång sikt. GIMP-utvecklingen riktar sig mot Debian Testing, vilket gör Debian Stable till ett nära anpassat bassystem.

Lumi fungerar bäst på Debian med Cinnamon (X11) och utvecklas och testas i den miljön. Cinnamon ger ett välbekant, Windows-liknande skrivbordsarbetsflöde, medan X11 har varit den mest stabila miljön för Lumi-utveckling.

Om du kommer från Windows är den viktigaste konceptuella förändringen att de flesta programinstallationer och konfigurationer sker via pakethanterare och enkla terminalkommandon i stället för nedladdningsbara installationsprogram.

## Vem guiden är till för

Guiden dokumenterar en fungerande Debian Stable-installation som används för Lumi-utveckling. Det är inte en allmän handledning för Linux-installation.

Den är mest användbar för:

- artister som flyttar från Windows och vill ha en förutsägbar Linux-installation
- utvecklare som bygger Lumi från källkod
- användare som föredrar att återskapa en känd arbetsmiljö i stället för att designa sin egen systemkonfiguration

Grundläggande kunskap om diskpartitionering och enkel kommandoradsanvändning förutsätts.

## Säkerhetskopiera dina data

Innan du installerar Debian, skapa en fullständig säkerhetskopia av din hemkatalog på en extern enhet. Inkludera eventuella extra datamappar som du vill behålla.

Obs: I Linux representerar `~` din hemkatalog.

Om du använder Git-repositoryer, pusha viktiga ändringar till remotes så att de enkelt kan återställas efter installationen. Detta steg är bara relevant om du redan använder Git.

## Skapa en partition

Skapa utrymme på din primära enhet för Debian. Det finns många guider och verktyg för detta steg, inklusive GParted. Beroende på din konfiguration kan du:

- krympa en befintlig Windows-partition för dual boot
- återanvända en befintlig Linux-partition
- förbereda nya Linux- och swap-partitioner

Om du är osäker, läs hårdvaruspecifika guider innan du gör ändringar, eftersom partitioneringsstegen varierar avsevärt mellan system.


## Skapa ett Debian-installations-USB

Förutsatt att en målpartition och swap-utrymme redan finns:

1. Ladda ner Debian ISO från den officiella webbplatsen: https://www.debian.org/
2. På Windows, använd BalenaEtcher för att skriva ISO till en USB-enhet.
3. På Linux, använd ett kommandoradsverktyg som `dd` för att skapa ett startbart USB-minne.

## Installera Debian

1. Sätt i USB-enheten.
2. Starta om och tryck på startmenyknappen (vanligtvis `F2`, `F12`, `Esc` eller `Del`) under uppstart.
3. Välj USB-enheten.
4. Välj en icke-grafisk installerare.
5. Lämna root-lösenordet tomt när du uppmanas, så att installeraren ger sudo-åtkomst till ditt användarkonto.
6. Partitionera manuellt:

   - Filsystem: ext4 (journaling)
   - Swap: befintlig swap-partition
   - Monteringspunkt: `/`
   - Etikett: `linux`
   - Värdnamn: systemnamn visas som `user@hostname`
   - Användarkonto: ditt fullständiga namn
   - Användarnamn: terminalens inloggningsnamn

7. Debians installerare erbjuder ett val av skrivbordsmiljö i detta skede; välj **Cinnamon** för den av Lumi rekommenderade konfigurationen.
8. Slutför installationen och starta om till Debian Stable.

## Systeminställningar

### Displayskalning

Debian Stable hanterar för närvarande fraktionerad skalning inkonsekvent, särskilt på 4K-skärmar. I stället för att sänka skärmupplösningen, justera gränssnittselement direkt.

Rekommenderade justeringar:

- Undvik fraktionerad skärmskalning.
- Menu → Font Selection → Font Settings → Text Scaling Factor: `2.5`
- Desktop Font: `14`
- Panel → Customize → Panel Height: `60`
- Panel Appearance → Right Zone Symbolic Icon Size: `48px`
- Mus och pekplatta → Storlek på pekare
- Desktop (right-click) → Customize → Larger icon size

Firefox-justering:

- Address bar → `about:config`
- Ställ in `layout.css.devPixelsPerPx` till `1`

### Terminal

Konfigurera terminalinställningar:

1. Menu → Terminal → Edit → Preferences
2. Text → Initial size: `140 columns`, `40 rows`
3. Text → Custom font: `Monospace 10`
4. Colours → Built-in schemes → Solarized Dark

## Återställ data

Återställ säkerhetskopierade filer till hemkatalogen efter behov, till exempel:

- `Backup/Home/Artwork` → `~/Artwork`
- `Backup/Home/code` → `~/code`
- `Backup/Home/Desktop` → `~/Desktop`
- `Backup/Home/.ssh` → `~/.ssh`
- `Backup/Home/.config/lumi` → `~/.config/lumi`

Obs: Mappar som börjar med `.` är dolda konfigurationskataloger i Linux.

## Valfritt: Git-konfiguration

Krävs endast om du planerar att bygga Lumi eller återställa repositoryer.

### Installera Git

```bash
sudo apt install git
```

Konfigurera din identitet:

```bash
git config --global --edit
```

#### GitLab-åtkomst

Återställ repositoryåtkomst till GitLab eller GitHub:

1. Ändra behörigheterna för SSH-nyckelfilen: `chmod 600 ~/.ssh/id_rsa`
2. Lägg till nyckeln i ssh-agenten: `ssh-add ~/.ssh/id_rsa`
3. Testa anslutningen: `ssh -T git@ssh.gitlab.gnome.org` eller `ssh -T git@github.com`

För varje repository, hämta remotes och återställ den lokala grenen så att den matchar:

```bash
git reset --hard remote-name/branch-name
git clean -df
```

Kör `git status` för att bekräfta att repositoryerna är rena.

Du har nu ett nytt operativsystem med återställda data och repositoryer. Den här konfigurationen speglar en känd arbetsmiljö för Lumi-utveckling och kan anpassas till individuella arbetsflöden efter behov.

## Bygg Lumi efter OS-konfiguration

Lumi-byggskript finns i:

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

