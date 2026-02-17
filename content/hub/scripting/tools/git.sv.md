---
title: "Git"
type: docs
---
Använd Git för att spåra ändringar av dina plugin-program, återställa misstag och dela kod mellan maskiner.

## Varför organisera din kod?

När du väl har mer än ett skript sparar en konsekvent mappstruktur tid och gör versionskontrollen enkel.

## Konfigurera en kodmappstruktur

Ett av de enklaste sätten att organisera dina projekt är genom att skapa en dedikerad **kodmapp** på din lokala dator. Inuti den här mappen kan du skapa undermappar för varje projekt eller arkiv. Här är en rekommenderad mappstruktur:

```plaintext
/home/your-username/code/
  ├── project1/
  ├── project2/
  └── project3/
```

Varje undermapp (t.ex. `project1`) representerar ett **lager**, där du lagrar filerna och koden för det projektet.

## Vad är ett arkiv?

Ett **repository** (eller **repo**) är i huvudsak en mapp med innehåll som Git spårar. När du skapar en repo lokalt, initialiserar du Git i den mappen, så att du kan spara alla ändringar i en onlineklon.

### Lokala och Remote Repositories

- **Lokal Repo**: Detta är arkivet som är lagrat på din dator, i en av dina projektmappar.
- **Remote Repo**: En version av förvaret som lagras online (till exempel på GitLab eller GitHub).

## Använda Git och GitHub

När din mappstruktur är på plats kan du initiera Git och ansluta dina lokala projekt till GitHub. Följ dessa steg för att komma igång:

### Grundläggande steg för att använda Git och GitHub

1. **Installera Git**
2. **Skapa ett GitHub-konto**
3. **Skapa ett tomt arkiv på GitHub**
4. **Initiera Git i ditt lokala projekt**
5. **Anslut ditt lokala repo till GitHub**
6. **Iscenesätt dina filer**
7. **Befästa dina ändringar**
8. **Överför dina ändringar till GitHub**
9. **Visa ditt arkiv online**

### 1. Installera Git

Om du inte har installerat Git än kan du göra det på Linux med:

```sh
sudo apt install git
```

### 2. Skapa ett GitHub-konto

Om du inte redan har ett konto, besök [GitHub](https://github.com/) för att registrera dig. När du väl har registrerat dig kan du skapa repositories på GitHub för att lagra din kod online.

### 3. Skapa ett tomt arkiv på GitHub

1. **Logga in på GitHub**: Gå till [GitHub](https://github.com/) och logga in på ditt konto.
2. **Skapa ett nytt arkiv**:
   - Klicka på ikonen **+** i det övre högra hörnet och välj **Nytt arkiv**.
   - Ange ett arkivnamn (t.ex. `your-repository`).
   - Lägg till en beskrivning om så önskas.
   - Välj **Offentlig** eller **Privat** synlighet.
   - **Initiera inte arkivet med en README, `.gitignore` eller licens (för att undvika konflikter).
   - Klicka på **Skapa arkiv**.

### 4. Initiera Git i ditt lokala projekt

För att börja spåra en projektmapp med Git, öppna din terminal, navigera till projektmappen och kör:

```sh
cd code/your/project/folder
git init
```

Detta kommando initierar ett tomt Git-förråd i din projektmapp.

### 5. Anslut ditt lokala repo till GitHub

Därefter vill du ansluta ditt lokala arkiv till GitHub. Efter att ha skapat ett tomt arkiv på GitHub, lägg till det som en fjärrkontroll till ditt lokala projekt:

```sh
cd code/your/project/folder
git remote add origin https://github.com/your-username/your-repository.git
```

Ersätt `your-username` och `your-repository` med ditt faktiska GitHub-användarnamn och förvarsnamnet. Detta kommando länkar ditt lokala projekt med fjärrarkivet på GitHub.

### 6. iscensätt dina filer

Innan du kan spara dina ändringar i Git måste du berätta för Git vilka filer du har ändrat och vill spara. Detta kallas att "staging" dina filer. Använd följande kommando för att iscensätta alla ändrade eller nya filer:

```sh
git add .
```Detta säger åt Git att spåra ändringarna du har gjort i alla filer i ditt projekt. Du kan också iscensätta specifika filer genom att ersätta `.` med filens namn.

### 7. Bekräfta dina ändringar

Efter iscensättning är nästa steg att spara (eller "commit") ändringarna i ditt lokala Git-förråd. När du förbinder dig bör du alltid inkludera ett meddelande som beskriver vilka ändringar du har gjort. Till exempel:

```sh
git commit -m "Add new feature"
```

Flaggan `-m` låter dig skriva ett meddelande som sammanfattar de ändringar du gjort. Det här meddelandet hjälper dig och andra att förstå vad som ändrades i denna commit.

### 8. Skicka dina ändringar till GitHub

När du har genomfört ändringarna lokalt kan du nu "pusha" dem till GitHub så att ditt fjärrlager uppdateras. Kör följande kommando för att ladda upp dina ändringar:

```sh
git push -u origin main
```

Grenen `main` är standardgrenen i GitHub där koden lagras, och detta kommando laddar upp dina lokala ändringar till fjärrförvaret, vilket gör dem tillgängliga online.

### 9. Se din kod på GitHub

När du har skickat din kod till GitHub kan du se ditt arkiv i GitHubs webbgränssnitt. Du bör se filerna från ditt lokala repo, tillsammans med en commit-historik som visar de ändringar du har gjort.

## Slutsats

Genom att organisera din kod i dedikerade mappar och använda GitHub för att hantera och säkerhetskopiera dina arkiv, kommer du att hålla dina projekt välstrukturerade och lättillgängliga. När du har en fungerande version av din kod, skicka den till GitHub. Du kan sedan enkelt spåra eventuella ändringar med antingen GitHub-webbgränssnittet eller Visual Studio Code, som markerar modifierade linjer. Detta tillvägagångssätt låter dig fortsätta att förfina och utöka din kod utan att tappa koll på framsteg eller ändringar.

Git och plattformar som GitHub och GitLab är kraftfulla verktyg, och även om de kan vara invecklade, finns det många resurser tillgängliga online för att hjälpa dig att förstå dem bättre. En av de mest värdefulla resurserna jag har hittat är AI-hjälpare som ChatGPT. Du kan beskriva vad du behöver åstadkomma, och dessa verktyg guidar dig tålmodigt genom processen steg för steg.

## Ordlista

Här är några vanliga termer du kommer att stöta på när du arbetar med Git och GitHub:- **Commit**: En ögonblicksbild av dina ändringar i arkivet. Varje commit innehåller ett meddelande som beskriver vad som ändrades och skapar en historisk post som du kan referera till eller återgå till senare.
- **Repository (Repo)**: En samling filer och deras historik spåras av Git. Lagrar kan finnas lokalt på din dator eller på distans på plattformar som GitHub. Varje projekt lagras vanligtvis i sitt eget arkiv.
- **Fjärr**: Ett fjärrlager är en version av ditt projekt som är värd på en plattform som GitHub. Den lokala versionen av ditt projekt på din dator är länkad till denna fjärrkontroll så att du kan ladda upp (push) och ladda ner (pull) ändringar.
- **Staging**: Processen att förbereda filer för en commit. När du iscensätter en fil, säger du till Git att du vill inkludera den i nästa commit. Staging låter dig välja vilka ändringar som ska inkluderas i en commit.
- **Push**: Handlingen att skicka dina engagerade ändringar från ditt lokala arkiv till ett fjärrarkiv (t.ex. GitHub), så att andra kan komma åt den uppdaterade versionen av din kod.
- **Pull**: Hämta ändringar från ett fjärrlager för att uppdatera din lokala kopia. Du drar ändringar när du vill synkronisera ditt lokala arkiv med den senaste versionen från fjärrkontrollen.
- **Ursprung**: Standardnamnet för ett fjärrlager när du först ansluter ditt lokala arkiv till en fjärrenhet. Refererar vanligtvis till huvudadressen till ditt projekt på GitHub.