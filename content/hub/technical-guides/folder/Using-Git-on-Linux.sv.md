---
title: "Använder Git på Linux"
type: docs
url: "hub/technical-guides/folder/Using-Git-on-Linux"
---
Välkommen till denna nybörjarguide för att använda Git på Linux! Den här guiden är utformad för att hjälpa dig komma igång med Git och GitLab, och för att ge en grundläggande förståelse för hur du använder dessa verktyg.

## Git Översikt

Koden som används för att skapa applikationer sparas i en samling mappar och filer på ditt system. Git är en applikation som låter oss säkerhetskopiera, dela och kopiera den samlingen. Git är känt som ett versionskontrollsystem som låter dig spåra ändringar i din kod och samarbeta med andra. Det är ett kraftfullt verktyg som används i stor utsträckning i öppen källkod. GitLab är en webbaserad plattform som låter dig vara värd för och hantera dina Git-förråd online, vilket gör det enkelt att samarbeta med andra och spåra ändringar i din kod.

## Vad är ett arkiv?

En _repo_, förkortning för repository, är en Git-hanterad lokal mapp med en onlinekopia. En Git Lab-repo är en samling filer och mappar som utgör ett projekt. Det kan ha _grenar_ som är oberoende kopior av samma projekt. En filial är en separat version av ditt projekt som låter dig göra ändringar utan att påverka huvudversionen. Detta är användbart för att testa nya funktioner eller fixa buggar utan att störa huvudprojektet. Det finns ditt lokala repo, lagrat på din hårddisk, och fjärrrepo, lagrat online med Git och GitLab.

## Använda Git

Du måste installera Git på ditt system. På Debian-baserade system kan du använda kommandot apt för att installera programpaket. I det här fallet använder vi det för att installera Git, som är ett paket som tillhandahåller Git versionskontrollsystem. Kommandot sudo ger installationsprogrammet behörighet att installera på ditt system.

```bash
 sudo apt install git
```

## Gå till GitLab

Innan du kan använda [GitLab](https://gitlab.com/users/sign_up) måste du skapa ett konto genom att besöka GitLabs webbplats och slutföra registreringsprocessen.

GitLab kräver _SSH_ för säker och autentiserad kommunikation mellan en klient (till exempel du) och GitLab-servern när du utför Git-operationer som _cloning_, _pushing_ och _fetching_ repositories. Kloning är att göra en lokal kopia av repet, hämtning överför alla ändringar som gjorts i repet till din lokala kopia, och push skickar ändringar och innehåll till serverförvaret. SSH (Secure Shell) är ett nätverksprotokoll som tillåter säker fjärråtkomst och använder _nyckelpar_ för att autentisera och upprätta säkra anslutningar. För att generera ett SSH-nyckelpar kan du använda kommandot ssh-keygen i din terminal.

```bash
 ssh-keygen
```

Ange ett filnamn, eller använd standardinställningen genom att trycka på Enter, och eventuellt ett lösenord. I din hemkatalog, i en dold mapp som heter .ssh, finns det nu två id_rsa-filer, om du gick med standardnamn. .pub-filen är den offentliga nyckeln och du kan se dess innehåll med en textredigerare.

Logga in på ditt GitLab-konto och navigera till dina användarinställningar. Klicka på 'SSH-nycklar' i den vänstra navigeringsmenyn. Kopiera och klistra in din publika nyckel i nyckelfältet och ge nyckeln en relevant titel, som PC@Home. Klicka på knappen "Lägg till nyckel" för att spara nyckeln. Din offentliga SSH-nyckel läggs nu till ditt GitLab-konto och du kan använda den för att autentisera med GitLab-arkiv. Testa om dina nycklar och anslutning fungerar med kommandot ssh -T för att se ett välkomstmeddelande från GitLab.

```bash
 $ ssh -T git@ssh.gitlab.gnome.org
 Welcome to GitLab, @username!
```

## Grundläggande Git-kommandonNu när du har installerat Git och har ställt in din SSH-nyckel med GitLab, låt oss gå igenom några viktiga Git-kommandon för att hantera arkiv. Dessa kommandon hjälper dig att arbeta med befintliga projekt, hålla dem uppdaterade och göra ändringar på ett säkert sätt.

### 1. **Klona ett arkiv**

Kloning är processen att skapa en lokal kopia av ett fjärrlager. Detta är användbart när du vill arbeta med ett projekt som redan finns på GitLab. För att klona ett arkiv, använd kommandot `git clone` följt av arkivets URL:

```sh
git clone https://gitlab.com/username/repository.git
```

Byt ut `https://gitlab.com/username/repository.git` med URL:en till det arkiv du vill klona. Detta kommando kommer att skapa en lokal kopia av förvaret i en ny katalog.

### 2. **Kontrollerar arkivets status**

För att se om ditt lokala arkiv har några ändringar eller för att se dess nuvarande tillstånd, använd:

```sh
git status
```

Detta kommando visar dig vilka filer som har ändrats, lagts till eller tagits bort i din lokala kopia av förvaret.

### 3. **Fjärrförråd**

Fjärrlager är versioner av ditt projekt som är värd online, till exempel på GitLab. De fungerar som den centrala platsen där din kod lagras och kan nås av andra. Standardfjärrförrådet som Git skapar när du klona ett projekt heter `origin`. Du kan lägga till, ta bort eller lista fjärrlager med hjälp av följande kommandon:

- **Fjärrkontroller:**

  För att se vilka fjärrlager som är länkade till ditt lokala projekt, använd:

  ```sh
  git remote -v
  ```

  Detta kommando listar alla fjärrkontroller och deras webbadresser. Vanligtvis kommer du att se `origin` listad här.

- **Lägga till en fjärrkontroll:**

  Om du behöver lägga till ett nytt fjärrlager kan du göra det med:

  ```sh
  git remote add <name> <url>
  ```

  Ersätt `<name>` med ett namn för fjärrkontrollen och `<url>` med URL:en till förvaret.

- **Ta bort en fjärrkontroll:**

  För att ta bort ett fjärrlager, använd:

  ```sh
  git remote remove <name>
  ```

  Ersätt `<name>` med namnet på fjärrkontrollen du vill ta bort.

### 4. **Hämta ändringar från Remote Repository**

Om du vill se vilka ändringar som har gjorts i fjärrförvaret utan att tillämpa dem på din lokala kopia, använd:

```sh
git fetch origin
```

Det här kommandot hämtar de senaste ändringarna från fjärrarkivet men slår inte ihop dem med din lokala filial. Det är ett sätt att söka efter uppdateringar innan du bestämmer dig för att införliva dem.

### 5. **Återställa ditt lokala arkiv**

Om du vill återställa ditt lokala förråd så att det matchar fjärrförvaret exakt, kan du använda en "hård" återställning. **Varning:** Detta kommer att skriva över alla lokala ändringar du har gjort.

```sh
git reset --hard origin/branch-name
```

Ersätt `branch-name` med namnet på grenen du vill återställa. Detta kommando tar bort alla lokala ändringar och gör ditt lokala arkiv identiskt med fjärrarkivet.

### 6. **Visning av åtagandehistorik**

För att se en lista över ändringar som gjorts i arkivet över tid, använd:

```sh
git log
```

Det här kommandot visar en historik över åtaganden, inklusive författare, datum och meddelande för varje ändring. Det är användbart för att förstå vilka ändringar som har gjorts och när.

### Sammanfattning

Dessa grundläggande Git-kommandon hjälper dig att arbeta med arkiv, hålla dina lokala kopior uppdaterade och säkerställa att du säkert kan hantera fjärrarkiv. Att klona förråd, kontrollera statusen för din lokala kopia och hantera fjärrförvar är nyckelfärdigheter för att hantera projekt med Git.