---
title: "Git"
type: docs
---
Gebruik Git om wijzigingen in uw plug-ins bij te houden, fouten ongedaan te maken en code tussen machines te delen.

## Waarom uw code organiseren?

Als u meer dan één script heeft, bespaart een consistente mapstructuur tijd en wordt versiebeheer eenvoudig.

## Een codemapstructuur opzetten

Een van de eenvoudigste manieren om uw projecten te organiseren is door een speciale **codemap** op uw lokale computer te maken. Binnen deze map kunt u voor elk project of elke repository submappen maken. Hier is een aanbevolen mapstructuur:

```plaintext
/home/your-username/code/
  ├── project1/
  ├── project2/
  └── project3/
```

Elke submap (bijvoorbeeld `project1`) vertegenwoordigt een **repository**, waar u de bestanden en code voor dat project opslaat.

## Wat is een opslagplaats?

Een **repository** (of **repo**) is in wezen een map met inhoud die Git bijhoudt. Wanneer je lokaal een repository aanmaakt, initialiseer je Git in die map, waardoor je eventuele wijzigingen in een online kloon kunt opslaan.

### Lokale en externe opslagplaatsen

- **Lokale opslagplaats**: dit is de opslagplaats die op uw computer is opgeslagen, in een van uw projectmappen.
- **Remote Repo**: een versie van de repository die online is opgeslagen (bijvoorbeeld op GitLab of GitHub).

## Git en GitHub gebruiken

Zodra uw mappenstructuur aanwezig is, kunt u Git initialiseren en uw lokale projecten verbinden met GitHub. Volg deze stappen om aan de slag te gaan:

### Basisstappen voor het gebruik van Git en GitHub

1. **Git installeren**
2. **Maak een GitHub-account**
3. **Maak een lege repository op GitHub**
4. **Initialiseer Git in uw lokale project**
5. **Verbind uw lokale opslagplaats met GitHub**
6. **Plaats uw bestanden**
7. **Bevestig uw wijzigingen**
8. **Push uw wijzigingen naar GitHub**
9. **Bekijk uw opslagplaats online**

### 1. Installeer Git

Als je Git nog niet hebt geïnstalleerd, kun je dit op Linux doen met behulp van:

```sh
sudo apt install git
```

### 2. Maak een GitHub-account

Als u nog geen account heeft, gaat u naar [GitHub](https://github.com/) om u aan te melden. Eenmaal geregistreerd, kunt u opslagplaatsen op GitHub maken om uw code online op te slaan.

### 3. Maak een lege repository op GitHub

1. **Log in op GitHub**: Ga naar [GitHub](https://github.com/) en log in op uw account.
2. **Maak een nieuwe opslagplaats**:
   - Klik op het pictogram **+** in de rechterbovenhoek en selecteer **Nieuwe repository**.
   - Voer een naam voor de opslagplaats in (bijvoorbeeld `your-repository`).
   - Voeg indien gewenst een beschrijving toe.
   - Kies **Openbare** of **Privé** zichtbaarheid.
   - **Initialiseer de repository niet** met een README, `.gitignore` of licentie (om conflicten te voorkomen).
   - Klik op **Repository maken**.

### 4. Initialiseer Git in uw lokale project

Om te beginnen met het bijhouden van een projectmap met Git, open je je terminal, navigeer je naar de projectmap en voer je het volgende uit:

```sh
cd code/your/project/folder
git init
```

Deze opdracht initialiseert een lege Git-repository in uw projectmap.

### 5. Verbind uw lokale opslagplaats met GitHub

Vervolgens wil je je lokale repository verbinden met GitHub. Nadat je een lege repository op GitHub hebt gemaakt, voeg je deze als afstandsbediening toe aan je lokale project:

```sh
cd code/your/project/folder
git remote add origin https://github.com/your-username/your-repository.git
```

Vervang `your-username` en `your-repository` door uw werkelijke GitHub-gebruikersnaam en de naam van de repository. Deze opdracht koppelt uw lokale project aan de externe repository op GitHub.

### 6. Zet uw bestanden klaar

Voordat je je wijzigingen in Git kunt opslaan, moet je Git vertellen welke bestanden je hebt gewijzigd en wilt opslaan. Dit wordt het "staging" van uw bestanden genoemd. Gebruik de volgende opdracht om alle gewijzigde of nieuwe bestanden te stagen:

```sh
git add .
```Dit vertelt Git om de wijzigingen bij te houden die je in alle bestanden in je project hebt aangebracht. U kunt ook specifieke bestanden stagen door `.` te vervangen door de naam van het bestand.

### 7. Leg uw wijzigingen vast

Na de fasering is de volgende stap het opslaan (of "commit") van de wijzigingen in uw lokale Git-repository. Wanneer u een commit maakt, moet u altijd een bericht opnemen waarin wordt beschreven welke wijzigingen u heeft aangebracht. Bijvoorbeeld:

```sh
git commit -m "Add new feature"
```

Met de vlag `-m` kunt u een bericht schrijven waarin de aangebrachte wijzigingen worden samengevat. Dit bericht helpt jou en anderen te begrijpen wat er in deze commit is gewijzigd.

### 8. Push uw wijzigingen naar GitHub

Zodra je de wijzigingen lokaal hebt vastgelegd, kun je ze nu naar GitHub "pushen", zodat je externe repository wordt bijgewerkt. Voer de volgende opdracht uit om uw wijzigingen te uploaden:

```sh
git push -u origin main
```

De `main` branch is de standaard branch in GitHub waar de code wordt opgeslagen, en deze opdracht uploadt uw lokale wijzigingen naar de externe repository, waardoor ze online toegankelijk worden.

### 9. Bekijk uw code op GitHub

Nadat u uw code naar GitHub heeft gepusht, kunt u uw repository bekijken in de GitHub-webinterface. U zou de bestanden uit uw lokale repository moeten zien, samen met een commitgeschiedenis die de wijzigingen toont die u heeft aangebracht.

## Conclusie

Door uw code in speciale mappen te organiseren en GitHub te gebruiken voor het beheren en back-uppen van uw repository's, houdt u uw projecten goed gestructureerd en gemakkelijk toegankelijk. Zodra je een werkende versie van je code hebt, push je deze naar GitHub. Vervolgens kunt u eventuele wijzigingen eenvoudig volgen met behulp van de GitHub-webinterface of Visual Studio Code, waarin gewijzigde regels worden gemarkeerd. Met deze aanpak kunt u uw code blijven verfijnen en uitbreiden zonder de voortgang of wijzigingen uit het oog te verliezen.

Git en platforms zoals GitHub en GitLab zijn krachtige tools, en hoewel ze ingewikkeld kunnen zijn, zijn er online talloze bronnen beschikbaar om je te helpen ze beter te begrijpen. Een van de meest waardevolle bronnen die ik heb gevonden zijn AI-helpers zoals ChatGPT. U kunt beschrijven wat u moet bereiken, en deze hulpmiddelen zullen u geduldig stap voor stap door het proces leiden.

## Woordenlijst

Hier zijn enkele veelvoorkomende termen die je tegenkomt als je met Git en GitHub werkt:- **Commit**: een momentopname van uw wijzigingen in de repository. Elke commit bevat een bericht waarin wordt beschreven wat er is gewijzigd en creëert een historisch record waar u naar kunt verwijzen of waar u later naar kunt terugkeren.
- **Repository (Repo)**: een verzameling bestanden en hun geschiedenis bijgehouden door Git. Opslagplaatsen kunnen lokaal op uw computer staan ​​of op afstand op platforms zoals GitHub. Elk project wordt doorgaans opgeslagen in een eigen repository.
- **Op afstand**: een externe repository is een versie van uw project die wordt gehost op een platform zoals GitHub. Aan deze afstandsbediening wordt de lokale versie van uw project op uw computer gekoppeld, zodat u wijzigingen kunt uploaden (push) en downloaden (pull).
- **Staging**: het proces van het voorbereiden van bestanden voor een commit. Wanneer je een bestand staget, vertel je Git dat je het in de volgende commit wilt opnemen. Met staging kun je kiezen welke wijzigingen je in een commit wilt opnemen.
- **Push**: het verzenden van uw vastgelegde wijzigingen van uw lokale opslagplaats naar een externe opslagplaats (bijvoorbeeld GitHub), zodat anderen toegang kunnen krijgen tot de bijgewerkte versie van uw code.
- **Pull**: het ophalen van wijzigingen uit een externe opslagplaats om uw lokale kopie bij te werken. U haalt wijzigingen op wanneer u uw lokale repository wilt synchroniseren met de nieuwste versie vanaf de afstandsbediening.
- **Oorsprong**: de standaardnaam voor een externe opslagplaats wanneer u uw lokale opslagplaats voor het eerst verbindt met een externe opslagplaats. Verwijst doorgaans naar de hoofd-URL van uw project op GitHub.