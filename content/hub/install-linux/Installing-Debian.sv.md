---
title: "Installerar Debian"
type: docs
url: "hub/install-linux/Installing-Debian"
---
Det här dokumentet beskriver processen som används för att installera Debian Stable som utvecklingsoperativsystemet Lumio. Det kan vara användbart för andra som sätter upp en liknande miljö.

Debian Stable valdes för att Lumi strävar efter att bygga tillförlitligt på en förutsägbar långsiktig plattform. GIMP-utvecklingen är inriktad på Debian-testning, vilket gör Debian Stable till ett nära anpassat bassystem.

Om du kommer från Windows är den huvudsakliga konceptuella förändringen att de flesta programvaruinstallationer och konfigurationer sker genom pakethanterare och enkla terminalkommandon snarare än nedladdningsbara installationsprogram.

## Vem den här guiden är till för

Den här guiden dokumenterar en fungerande Debian Stable-installation som används för Lumi-utveckling. Det är inte en allmän Linux-installationshandledning.

Det är mest användbart för:

- artister som flyttar från Windows som vill ha en förutsägbar Linux-installation
- utvecklare som bygger Lumi från källan
- användare som föredrar att återskapa en känd arbetsmiljö snarare än att designa sin egen systemkonfiguration

Grundläggande förtrogenhet med diskpartitionering och enkel kommandoradsanvändning förutsätts.

## Säkerhetskopiera dina data

Innan du installerar Debian, skapa en fullständig säkerhetskopia av din hemkatalog på en extern enhet. Inkludera eventuella ytterligare datamappar som du vill bevara.

Obs: I Linux representerar `~` din hemkatalog.

Om du använder Git-förråd, tryck på alla viktiga ändringar till deras ursprung så att de enkelt kan återställas efter installationen. Detta steg är endast relevant om du redan använder Git.

## Skapa en partition

Skapa utrymme på din primära enhet för Debian. Det finns många guider och verktyg för detta steg, inklusive GParted. Beroende på din inställning kan du:

- krympa en befintlig Windows-partition för dubbelstart
- återanvänd en befintlig Linux-partition
- förbered nytt Linux och byt partitioner

Om du är osäker, konsultera hårdvaruspecifika guider innan du gör ändringar, eftersom partitioneringsstegen varierar avsevärt mellan olika system.


## Skapa en Debian Installation USB

Förutsatt att en målpartition och växlingsutrymme redan finns:

1. Ladda ner Debian ISO från den officiella webbplatsen: https://www.debian.org/
2. På Windows använder du BalenaEtcher för att skriva ISO till en USB-enhet.
3. På Linux, använd ett kommandoradsverktyg som `dd` för att skapa en startbar USB.

## Installera Debian

1. Sätt i USB-enheten.
2. Starta om och tryck på startmenyknappen (vanligtvis `F2`, `F12`, `Esc` eller `Del`) under uppstart.
3. Välj USB-enheten.
4. Välj ett icke-grafiskt installationsprogram.
5. Lämna root-lösenordet tomt när du uppmanas så att installationsprogrammet ger sudo-åtkomst till ditt användarkonto.
6. Partitionera manuellt:

   - Filsystem: ext4 (journalföring)
   - Swap: befintlig swap-partition
   - Monteringspunkt: `/`
   - Etikett: `linux`
   - Värdnamn: systemnamnet visas som `user@hostname`
   - Användarkonto: ditt fullständiga namn
   - Användarnamn: terminalens inloggningsnamn

7. Välj **Cinnamon** som skrivbordsmiljö.
8. Slutför installationen och starta om i Debian Stable.

## Systeminställningar

### Displayskalning

Debian Stable hanterar för närvarande fraktionerad skalning inkonsekvent, särskilt på 4K-skärmar. Istället för att minska skärmupplösningen, justera gränssnittselement direkt.

Rekommenderade justeringar:- Undvik fraktionerad skärmskalning.
- Meny → Teckensnittsval → Teckensnittsinställningar → Textskalningsfaktor: `2.5`
- Skrivbordstypsnitt: `14`
- Panel → Anpassa → Panelhöjd: `60`
- Panelens utseende → Symbolisk ikonstorlek för höger zon: `48px`
- Mus och pekplatta → Justering av pekarens storlek
- Skrivbord (högerklicka) → Anpassa → Större ikonstorlek

Firefox-justering:

- Adressfält → `about:config`
- Ställ in `layout.css.devPixelsPerPx` till `1`

### Terminal

Konfigurera terminalinställningar:

1. Meny → Terminal → Redigera → Inställningar
2. Text → Initial storlek: `140 columns`, `40 rows`
3. Text → Anpassat teckensnitt: `Monospace 10`
4. Färger → Inbyggda scheman → Solarized Dark

## Återställ data

Återställ säkerhetskopierade filer till hemkatalogen efter behov, till exempel:

- `Backup/Home/Artwork` → `~/Artwork`
- `Backup/Home/code` → `~/code`
- `Backup/Home/Desktop` → `~/Desktop`
- `Backup/Home/.ssh` → `~/.ssh`
- `Backup/Home/.config/lumi` → `~/.config/lumi`

Obs: Mappar som börjar med `.` är dolda konfigurationskataloger i Linux.

## Valfritt: Git Setup

Krävs endast om du planerar att bygga Lumi eller återställa förråd.

### Installera Git

```bash
sudo apt install git
```

Konfigurera din identitet:

```bash
git config --global --edit
```

#### GitLab Access

Återställ arkivåtkomst till GitLab eller GitHub:

1. Ändra behörigheterna för SSH-nyckelfilen: `chmod 600 ~/.ssh/id_rsa`
2. Lägg till användaren till den nya Git-installationen: `ssh-add ~/.ssh/id_rsa`
3. Testa anslutningen: `ssh -T git@ssh.gitlab.gnome.org` eller `ssh -T git@github.com`

För varje arkiv, hämta ursprunget och återställ den lokala grenen så att den matchar:

```bash
git reset --hard remote-name/branch-name
git clean -df
```

Kör `git status` för att bekräfta att förråden är rena.

Vi har nu ett nytt operativsystem med alla data och arkiv återställda. Denna inställning speglar en känd arbetsmiljö som används för Lumi-utveckling och kan anpassas till individuella arbetsflöden efter behov.

## Bygg Lumi efter OS-installation

Lumi build-skript finns i:

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