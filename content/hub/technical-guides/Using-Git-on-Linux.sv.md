---
title: "Använda Git på Linux"
type: docs
url: "hub/technical-guides/Using-Git-on-Linux"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 7054a9ff9efeb93b4f494197e09ed1fe34d5d6bde7bc305480693c3982d375ae
---

Välkommen till den här nybörjarguiden för att använda Git på Linux! Guiden hjälper dig att komma igång med Git och GitLab och ger en grundläggande förståelse för hur du använder dessa verktyg.

## Git-översikt

Koden som används för att skapa applikationer finns i en samling mappar och filer på ditt system. Git är ett program som låter dig säkerhetskopiera, dela och kopiera den samlingen. Git är ett versionskontrollsystem som låter dig spåra ändringar i koden och samarbeta med andra. Det är ett kraftfullt verktyg som används flitigt i open source-gemenskapen. GitLab är en webbaserad plattform där du kan hosta och hantera dina Git-repositories online, vilket gör det enkelt att samarbeta med andra och följa ändringar i koden.

## Vad är ett repository?

Ett _repo_, förkortning av repository, är en lokal mapp som hanteras av Git och som har en onlinekopia. Ett GitLab-repo är en samling filer och mappar som utgör ett projekt. Det kan ha _branches_ — oberoende kopior av samma projekt. En branch är en separat version av projektet som låter dig göra ändringar utan att påverka huvudversionen. Det är användbart för att testa nya funktioner eller fixa buggar utan att störa huvudprojektet. Du har ett lokalt repo på hårddisken och ett remote repo som lagras online med Git och GitLab.

## Använda Git

Du behöver installera Git på ditt system. På Debian-baserade system kan du använda kommandot apt för att installera programpaket. Här använder vi det för att installera Git, ett paket som tillhandahåller Git-versionskontrollsystemet. Kommandot sudo ger installationsprogrammet behörighet att installera på ditt system.

```bash
 sudo apt install git
```

## Åtkomst till GitLab

Innan du kan använda [GitLab](https://gitlab.com/users/sign_up) behöver du skapa ett konto genom att besöka GitLabs webbplats och slutföra registreringen.

GitLab kräver _SSH_ för säker, autentiserad kommunikation mellan en klient (till exempel du) och GitLab-servern när du utför Git-operationer som _clone_, _push_ och _fetch_ av repositories. Clone innebär att skapa en lokal kopia av repot, fetch hämtar ändringar från repot till din lokala kopia, och push skickar ändringar och innehåll till server-repot. SSH (Secure Shell) är ett nätverksprotokoll som möjliggör säker fjärråtkomst och använder _nyckelpar_ för att autentisera och upprätta säkra anslutningar. För att generera ett SSH-nyckelpar kan du använda kommandot ssh-keygen i terminalen.

```bash
 ssh-keygen
```

Ange ett filnamn, eller tryck Enter för standardnamnet, och eventuellt ett lösenord. I din hemkatalog, i en dold mapp som heter .ssh, finns nu två id_rsa-filer om du använde standardnamn. .pub-filen är den publika nyckeln; du kan se innehållet i en textredigerare.

Logga in på ditt GitLab-konto och gå till användarinställningarna. Klicka på 'SSH Keys' i navigeringsmenyn till vänster. Kopiera och klistra in din publika nyckel i fältet Key och ge nyckeln en relevant titel, till exempel PC@Home. Klicka på 'Add Key' för att spara nyckeln. Din publika SSH-nyckel är nu tillagd i ditt GitLab-konto och du kan använda den för att autentisera mot GitLab-repositories. Testa om nycklarna och anslutningen fungerar med kommandot ssh -T; du bör se ett välkomstmeddelande från GitLab.

```bash
 $ ssh -T git@ssh.gitlab.gnome.org
 Welcome to GitLab, @username!
```

## Grundläggande Git-kommandon

Nu när du har installerat Git och konfigurerat din SSH-nyckel med GitLab går vi igenom några viktiga Git-kommandon för att hantera repositories. Dessa kommandon hjälper dig att arbeta med befintliga projekt, hålla dem uppdaterade och göra ändringar på ett säkert sätt.

### 1. **Klona ett repository**

Att klona är processen att skapa en lokal kopia av ett remote repository. Det är användbart när du vill arbeta med ett projekt som redan finns på GitLab. För att klona ett repository använder du kommandot `git clone` följt av repository-URL:en:

```sh
git clone https://gitlab.com/username/repository.git
```

Byt ut `https://gitlab.com/username/repository.git` mot URL:en till det repository du vill klona. Detta kommando skapar en lokal kopia av repositoryt i en ny katalog.

### 2. **Kontrollera repositorystatus**

För att se om ditt lokala repository har några ändringar, eller för att se dess nuvarande tillstånd, använd:

```sh
git status
```

Detta kommando visar vilka filer som har ändrats, lagts till eller tagits bort i din lokala kopia av repositoryt.

### 3. **Remote repositories**

Remote repositories är versioner av ditt projekt som hostas online, till exempel på GitLab. De fungerar som den centrala platsen där koden lagras och där andra kan komma åt den. Det standard remote repository som Git skapar när du klonar ett projekt heter `origin`. Du kan lägga till, ta bort eller lista remote repositories med följande kommandon:

- **Lista remote repositories:**

  För att se vilka remote repositories som är kopplade till ditt lokala projekt, använd:

  ```sh
  git remote -v
  ```

  Detta kommando listar alla remote repositories och deras URL:er. Vanligtvis ser du `origin` här.

- **Lägga till ett remote repository:**

  Om du behöver lägga till ett nytt remote repository kan du göra det med:

  ```sh
  git remote add <name> <url>
  ```

  Byt ut `<name>` mot ett namn för remote repositoryt och `<url>` mot repository-URL:en.

- **Ta bort ett remote repository:**

  För att ta bort ett remote repository, använd:

  ```sh
  git remote remove <name>
  ```

  Byt ut `<name>` mot namnet på det remote repository du vill ta bort.

### 4. **Hämta ändringar från remote repository**

Om du vill se vilka ändringar som gjorts i remote repository utan att tillämpa dem på din lokala kopia, använd:

```sh
git fetch origin
```

Detta kommando hämtar de senaste ändringarna från remote repository men slår inte ihop dem med din lokala branch. Det är ett sätt att kontrollera efter uppdateringar innan du bestämmer dig för att ta in dem.

### 5. **Återställa ditt lokala repository**

Om du vill återställa ditt lokala repository så att det exakt matchar remote repository kan du använda en 'hard' reset. **Varning:** detta skriver över alla lokala ändringar du har gjort.

```sh
git reset --hard origin/branch-name
```

Byt ut `branch-name` mot namnet på den branch du vill återställa. Detta kommando kasserar alla lokala ändringar och gör ditt lokala repository identiskt med remote repository.

### 6. **Visa commithistorik**

För att se en lista över ändringar som gjorts i repositoryt över tid, använd:

```sh
git log
```

Detta kommando visar en historik över commits, inklusive författare, datum och meddelande för varje ändring. Det är användbart för att förstå vilka ändringar som gjorts och när.

### Sammanfattning

Dessa grundläggande Git-kommandon hjälper dig att arbeta med repositories, hålla dina lokala kopior uppdaterade och hantera remote repositories på ett säkert sätt. Att klona repositories, kontrollera statusen på din lokala kopia och hantera remote repositories är nyckelkunskaper för projekthantering med Git.
