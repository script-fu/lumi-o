---
title: "Git gebruiken op Linux"
type: docs
url: "hub/technical-guides/folder/Using-Git-on-Linux"
---
Welkom bij deze beginnershandleiding voor het gebruik van Git op Linux! Deze handleiding is bedoeld om u te helpen aan de slag te gaan met Git en GitLab, en om basiskennis te geven over het gebruik van deze tools.

## Git-overzicht

De code die wordt gebruikt voor het maken van applicaties wordt bewaard in een verzameling mappen en bestanden op uw systeem. Git is een applicatie waarmee we een back-up kunnen maken van die verzameling, deze kunnen delen en kopiëren. Git staat bekend als een versiebeheersysteem waarmee u wijzigingen in uw code kunt volgen en met anderen kunt samenwerken. Het is een krachtig hulpmiddel dat veel wordt gebruikt in de open-sourcegemeenschap. GitLab is een webgebaseerd platform waarmee u uw Git-repository's online kunt hosten en beheren, waardoor u gemakkelijk met anderen kunt samenwerken en wijzigingen in uw code kunt volgen.

## Wat is een opslagplaats?

Een _repo_, een afkorting van repository, is een door Git beheerde lokale map met een online kopie. Een Git Lab-repository is een verzameling bestanden en mappen waaruit een project bestaat. Het kan _branches_ hebben die onafhankelijke kopieën zijn van hetzelfde project. Een vertakking is een afzonderlijke versie van uw project waarmee u wijzigingen kunt aanbrengen zonder de hoofdversie te beïnvloeden. Dit is handig voor het testen van nieuwe functies of het oplossen van bugs zonder het hoofdproject te verstoren. Er is uw lokale opslagplaats, opgeslagen op uw harde schijf, en de externe opslagplaats, online opgeslagen met Git en GitLab.

## Git gebruiken

Je moet Git op je systeem installeren. Op op Debian gebaseerde systemen kunt u de opdracht apt gebruiken om softwarepakketten te installeren. In dit geval gebruiken we het om Git te installeren, een pakket dat het Git-versiecontrolesysteem biedt. De opdracht sudo geeft het installatieprogramma toestemming om op uw systeem te installeren.

```bash
 sudo apt install git
```

## Toegang tot GitLab

Voordat u [GitLab](https://gitlab.com/users/sign_up) kunt gebruiken, moet u een account aanmaken door naar de GitLab-website te gaan en het registratieproces te voltooien.

GitLab vereist _SSH_ voor veilige en geauthenticeerde communicatie tussen een client (jij bijvoorbeeld) en de GitLab-server bij het uitvoeren van Git-bewerkingen zoals _cloning_, _pushing_ en _fetching_ repositories. Klonen is het maken van een lokale kopie van de repository, ophalen is het overbrengen van eventuele wijzigingen in de repository naar uw lokale kopie, en pushen is het verzenden van wijzigingen en inhoud naar de serverrepository. SSH (Secure Shell) is een netwerkprotocol dat veilige toegang op afstand mogelijk maakt en gebruik maakt van _sleutelparen_ om veilige verbindingen te verifiëren en tot stand te brengen. Om een ​​SSH-sleutelpaar te genereren, kunt u de opdracht ssh-keygen in uw terminal gebruiken.

```bash
 ssh-keygen
```

Geef een bestandsnaam op, of gebruik de standaardnaam door op Enter te drukken, en eventueel een wachtwoord. In je thuismap, in een verborgen map met de naam .ssh, staan ​​nu twee id_rsa-bestanden, als je met standaardnamen hebt gewerkt. Het .pub-bestand is de openbare sleutel en u kunt de inhoud ervan bekijken met een teksteditor.

Log in op uw GitLab-account en navigeer naar uw gebruikersinstellingen. Klik op 'SSH Keys' in het linkernavigatiemenu. Kopieer en plak uw publieke sleutel in het veld Sleutel en geef de sleutel een relevante titel, zoals PC@Home. Klik op de knop 'Sleutel toevoegen' om de sleutel op te slaan. Uw openbare SSH-sleutel is nu toegevoegd aan uw GitLab-account en u kunt deze gebruiken om te authenticeren bij GitLab-repository's. Test of je sleutels en verbinding werken met het ssh -T commando om een ​​welkomstbericht van GitLab te zien.

```bash
 $ ssh -T git@ssh.gitlab.gnome.org
 Welcome to GitLab, @username!
```

## Basis Git-opdrachtenNu je Git hebt geïnstalleerd en je SSH-sleutel hebt ingesteld met GitLab, gaan we een aantal essentiële Git-opdrachten doornemen voor het beheren van repository's. Met deze opdrachten kunt u met bestaande projecten werken, ze up-to-date houden en veilig wijzigingen aanbrengen.

### 1. **Een repository klonen**

Klonen is het proces waarbij een lokale kopie van een externe opslagplaats wordt gemaakt. Dit is handig als u aan een project wilt werken dat al op GitLab bestaat. Om een repository te klonen, gebruikt u de opdracht `git clone` gevolgd door de repository-URL:

```sh
git clone https://gitlab.com/username/repository.git
```

Vervang `https://gitlab.com/username/repository.git` door de URL van de repository die u wilt klonen. Met deze opdracht wordt een lokale kopie van de repository in een nieuwe map gemaakt.

### 2. **Repositorystatus controleren**

Om te zien of uw lokale repository wijzigingen heeft of om de huidige status ervan te bekijken, gebruikt u:

```sh
git status
```

Met deze opdracht kunt u zien welke bestanden zijn gewijzigd, toegevoegd of verwijderd in uw lokale kopie van de repository.

### 3. **Remote opslagplaatsen**

Externe opslagplaatsen zijn versies van uw project die online worden gehost, zoals op GitLab. Ze dienen als centrale locatie waar uw code wordt opgeslagen en toegankelijk is voor anderen. De standaard externe repository die Git aanmaakt als je een project kloont, heet `origin`. U kunt externe opslagplaatsen toevoegen, verwijderen of weergeven met behulp van de volgende opdrachten:

- **Afstandsbedieningen vermelden:**

  Om te zien welke externe opslagplaatsen aan uw lokale project zijn gekoppeld, gebruikt u:

  ```sh
  git remote -v
  ```

  Met deze opdracht worden alle afstandsbedieningen en hun URL's weergegeven. Meestal ziet u hier `origin` staan.

- **Een afstandsbediening toevoegen:**

  Als u een nieuwe externe opslagplaats moet toevoegen, kunt u dit doen met:

  ```sh
  git remote add <name> <url>
  ```

  Vervang `<name>` door een naam voor de afstandsbediening, en `<url>` door de URL van de repository.

- **Een afstandsbediening verwijderen:**

  Om een externe opslagplaats te verwijderen, gebruikt u:

  ```sh
  git remote remove <name>
  ```

  Vervang `<name>` door de naam van de afstandsbediening die u wilt verwijderen.

### 4. **Wijzigingen ophalen uit de externe opslagplaats**

Als u wilt zien welke wijzigingen zijn aangebracht in de externe repository zonder deze op uw lokale kopie toe te passen, gebruikt u:

```sh
git fetch origin
```

Deze opdracht haalt de laatste wijzigingen op uit de externe repository, maar voegt deze niet samen in uw lokale branch. Het is een manier om te controleren op updates voordat u besluit deze op te nemen.

### 5. **Uw lokale opslagplaats opnieuw instellen**

Als u uw lokale repository opnieuw wilt instellen zodat deze exact overeenkomt met de externe repository, kunt u een 'harde' reset gebruiken. **Waarschuwing:** Hiermee worden alle lokale wijzigingen die u heeft aangebracht, overschreven.

```sh
git reset --hard origin/branch-name
```

Vervang `branch-name` door de naam van de vertakking die u wilt resetten. Met deze opdracht worden alle lokale wijzigingen ongedaan gemaakt en wordt uw lokale repository identiek aan de externe repository.

### 6. **Commitgeschiedenis bekijken**

Om een lijst met wijzigingen te zien die in de loop van de tijd in de repository zijn aangebracht, gebruikt u:

```sh
git log
```

Met dit commando wordt een geschiedenis van commits weergegeven, inclusief de auteur, datum en bericht voor elke wijziging. Het is handig om te begrijpen welke wijzigingen zijn aangebracht en wanneer.

### Samenvatting

Deze basis Git-opdrachten helpen je bij het werken met repository's, houden je lokale kopieën up-to-date en zorgen ervoor dat je externe repository's veilig kunt beheren. Het klonen van opslagplaatsen, het controleren van de status van uw lokale kopie en het beheren van externe opslagplaatsen zijn sleutelvaardigheden voor het beheren van projecten met Git.