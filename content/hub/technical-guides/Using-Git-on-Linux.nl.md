---
title: "Git gebruiken op Linux"
type: docs
url: "hub/technical-guides/Using-Git-on-Linux"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 7054a9ff9efeb93b4f494197e09ed1fe34d5d6bde7bc305480693c3982d375ae
---

Welkom bij deze beginnershandleiding voor het gebruik van Git op Linux! Deze handleiding helpt je op weg met Git en GitLab en geeft een basisbegrip van hoe je deze tools gebruikt.

## Git-overzicht

De code waarmee applicaties worden gemaakt, staat in een verzameling mappen en bestanden op je systeem. Git is een applicatie waarmee je die verzameling kunt back-uppen, delen en kopiëren. Git is een versiebeheersysteem waarmee je wijzigingen in je code kunt volgen en met anderen kunt samenwerken. Het is een krachtig hulpmiddel dat veel wordt gebruikt in de open-sourcegemeenschap. GitLab is een webplatform waarmee je je Git-repositories online kunt hosten en beheren, zodat je eenvoudig met anderen kunt samenwerken en wijzigingen in je code kunt volgen.

## Wat is een repository?

Een _repo_, kort voor repository, is een lokaal door Git beheerde map met een online kopie. Een GitLab-repo is een verzameling bestanden en mappen die samen een project vormen. Het kan _branches_ hebben: onafhankelijke kopieën van hetzelfde project. Een branch is een aparte versie van je project waarmee je wijzigingen kunt maken zonder de hoofdversie te beïnvloeden. Dat is handig om nieuwe functies te testen of bugs te verhelpen zonder het hoofdproject te verstoren. Je hebt een lokaal repo op je harde schijf en een remote repo dat online staat via Git en GitLab.

## Git gebruiken

Je moet Git op je systeem installeren. Op Debian-gebaseerde systemen kun je het apt-commando gebruiken om softwarepakketten te installeren. In dit geval gebruiken we het om Git te installeren, een pakket dat het Git-versiebeheersysteem levert. Het sudo-commando geeft het installatieprogramma toestemming om op je systeem te installeren.

```bash
 sudo apt install git
```

## Toegang tot GitLab

Voordat je [GitLab](https://gitlab.com/users/sign_up) kunt gebruiken, moet je een account aanmaken door de GitLab-website te bezoeken en het registratieproces te voltooien.

GitLab vereist _SSH_ voor veilige, geauthenticeerde communicatie tussen een client (jij, bijvoorbeeld) en de GitLab-server bij Git-bewerkingen zoals het _clonen_, _pushen_ en _fetchen_ van repositories. Clonen betekent een lokale kopie van het repo maken, fetchen haalt wijzigingen uit het repo naar je lokale kopie, en pushen stuurt wijzigingen en inhoud naar het server-repo. SSH (Secure Shell) is een netwerkprotocol voor veilige toegang op afstand en gebruikt _sleutelparen_ om verbindingen te authenticeren en te beveiligen. Om een SSH-sleutelpaar te genereren, gebruik je het ssh-keygen-commando in je terminal.

```bash
 ssh-keygen
```

Geef een bestandsnaam op, of druk op Enter voor de standaardnaam, en eventueel een wachtwoord. In je thuismap, in een verborgen map genaamd .ssh, staan nu twee id_rsa-bestanden als je de standaardnamen hebt gebruikt. Het .pub-bestand is de publieke sleutel; je kunt de inhoud bekijken met een teksteditor.

Log in op je GitLab-account en ga naar je gebruikersinstellingen. Klik op 'SSH Keys' in het navigatiemenu links. Kopieer en plak je publieke sleutel in het veld Key en geef de sleutel een duidelijke titel, bijvoorbeeld PC@Home. Klik op 'Add Key' om de sleutel op te slaan. Je publieke SSH-sleutel staat nu in je GitLab-account en je kunt die gebruiken om je te authenticeren bij GitLab-repositories. Test of je sleutels en verbinding werken met het ssh -T-commando; je zou een welkomstbericht van GitLab moeten zien.

```bash
 $ ssh -T git@ssh.gitlab.gnome.org
 Welcome to GitLab, @username!
```

## Basis Git-commando's

Nu Git is geïnstalleerd en je SSH-sleutel bij GitLab staat, behandelen we enkele essentiële Git-commando's voor het beheren van repositories. Deze commando's helpen je met bestaande projecten te werken, ze up-to-date te houden en veilig wijzigingen door te voeren.

### 1. **Een repository clonen**

Clonen is het proces waarbij je een lokale kopie van een remote repository maakt. Dat is handig als je aan een project wilt werken dat al op GitLab staat. Om een repository te clonen, gebruik je het `git clone`-commando gevolgd door de repository-URL:

```sh
git clone https://gitlab.com/username/repository.git
```

Vervang `https://gitlab.com/username/repository.git` door de URL van de repository die je wilt clonen. Dit commando maakt een lokale kopie van de repository in een nieuwe map.

### 2. **Repositorystatus controleren**

Om te zien of je lokale repository wijzigingen heeft, of om de huidige status te bekijken, gebruik je:

```sh
git status
```

Dit commando laat zien welke bestanden in je lokale kopie van de repository zijn gewijzigd, toegevoegd of verwijderd.

### 3. **Remote repositories**

Remote repositories zijn versies van je project die online worden gehost, bijvoorbeeld op GitLab. Ze vormen de centrale plek waar je code staat en waar anderen die kunnen gebruiken. De standaard remote repository die Git aanmaakt wanneer je een project clonet, heet `origin`. Met de volgende commando's kun je remote repositories toevoegen, verwijderen of weergeven:

- **Remotes weergeven:**

  Om te zien welke remote repositories aan je lokale project zijn gekoppeld, gebruik je:

  ```sh
  git remote -v
  ```

  Dit commando toont alle remotes en hun URL's. Meestal zie je hier `origin` staan.

- **Een remote toevoegen:**

  Als je een nieuwe remote repository wilt toevoegen, gebruik je:

  ```sh
  git remote add <name> <url>
  ```

  Vervang `<name>` door een naam voor de remote en `<url>` door de URL van de repository.

- **Een remote verwijderen:**

  Om een remote repository te verwijderen, gebruik je:

  ```sh
  git remote remove <name>
  ```

  Vervang `<name>` door de naam van de remote die je wilt verwijderen.

### 4. **Wijzigingen ophalen uit de remote repository**

Als je wilt zien welke wijzigingen in de remote repository zijn gemaakt zonder ze op je lokale kopie toe te passen, gebruik je:

```sh
git fetch origin
```

Dit commando haalt de nieuwste wijzigingen op uit de remote repository, maar voegt ze niet samen in je lokale branch. Zo kun je controleren op updates voordat je besluit ze over te nemen.

### 5. **Je lokale repository resetten**

Als je je lokale repository exact wilt laten overeenkomen met de remote repository, kun je een 'hard' reset gebruiken. **Waarschuwing:** hiermee worden alle lokale wijzigingen die je hebt gemaakt overschreven.

```sh
git reset --hard origin/branch-name
```

Vervang `branch-name` door de naam van de branch die je wilt resetten. Dit commando verwijdert alle lokale wijzigingen en maakt je lokale repository identiek aan de remote repository.

### 6. **Commitgeschiedenis bekijken**

Om een overzicht te zien van wijzigingen die in de loop van de tijd aan de repository zijn aangebracht, gebruik je:

```sh
git log
```

Dit commando toont een geschiedenis van commits, inclusief auteur, datum en bericht bij elke wijziging. Handig om te begrijpen welke wijzigingen wanneer zijn gemaakt.

### Samenvatting

Deze basis Git-commando's helpen je met repositories te werken, je lokale kopieën up-to-date te houden en remote repositories veilig te beheren. Repositories clonen, de status van je lokale kopie controleren en remotes beheren zijn kernvaardigheden voor projectbeheer met Git.
