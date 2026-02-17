---
title: "Va bene"
type: docs
---
Utilizza Git per tenere traccia delle modifiche ai plug-in, ripristinare gli errori e condividere il codice su più computer.

## Perché organizzare il tuo codice?

Una volta che si dispone di più di uno script, una struttura di cartelle coerente consente di risparmiare tempo e semplifica il controllo della versione.

## Impostazione di una struttura di cartelle di codice

Uno dei modi più semplici per organizzare i tuoi progetti è creare una **cartella del codice** dedicata sul tuo computer locale. All'interno di questa cartella è possibile creare sottocartelle per ciascun progetto o repository. Ecco una struttura di cartelle consigliata:

```plaintext
/home/your-username/code/
  ├── project1/
  ├── project2/
  └── project3/
```

Ciascuna sottocartella (ad esempio, `project1`) rappresenta un **repository**, ovvero il luogo in cui archivierai i file e il codice per quel progetto.

## Cos'è un repository?

Un **repository** (o **repo**) è essenzialmente una cartella con contenuti di cui Git tiene traccia. Quando crei un repository localmente, inizializzi Git all'interno di quella cartella, permettendoti di salvare qualsiasi modifica su un clone online.

### Repository locali e remoti

- **Repo locale**: questo è il repository archiviato sul tuo computer, in una delle cartelle del tuo progetto.
- **Remote Repo**: una versione del repository archiviata online (ad esempio, su GitLab o GitHub).

## Utilizzo di Git e GitHub

Una volta che la struttura delle cartelle è a posto, puoi inizializzare Git e connettere i tuoi progetti locali a GitHub. Segui questi passaggi per iniziare:

### Passaggi di base per l'utilizzo di Git e GitHub

1. **Installa Git**
2. **Crea un account GitHub**
3. **Crea un repository vuoto su GitHub**
4. **Inizializza Git nel tuo progetto locale**
5. **Connetti il tuo repository locale a GitHub**
6. **Mette in scena i tuoi file**
7. **Conferma le tue modifiche**
8. **Invia le modifiche a GitHub**
9. **Visualizza il tuo repository online**

### 1. Installa Git

Se non hai ancora installato Git, puoi farlo su Linux usando:

```sh
sudo apt install git
```

### 2. Crea un account GitHub

Se non disponi già di un account, visita [GitHub](https://github.com/) per registrarti. Una volta registrato, puoi creare repository su GitHub per archiviare il tuo codice online.

### 3. Crea un repository vuoto su GitHub

1. **Accedi a GitHub**: vai a [GitHub](https://github.com/) e accedi al tuo account.
2. **Crea un nuovo repository**:
   - Fai clic sull'icona **++** nell'angolo in alto a destra e seleziona **Nuovo repository**.
   - Immettere il nome del repository (ad esempio, `your-repository`).
   - Aggiungi una descrizione se lo desideri.
   - Scegli la visibilità **Pubblica** o **Privata**.
   - **Non** inizializzare il repository con un README, `.gitignore` o una licenza (per evitare conflitti).
   - Fai clic su **Crea archivio**.

### 4. Inizializza Git nel tuo progetto locale

Per iniziare a monitorare una cartella di progetto con Git, apri il terminale, vai alla cartella del progetto ed esegui:

```sh
cd code/your/project/folder
git init
```

Questo comando inizializza un repository Git vuoto nella cartella del progetto.

### 5. Connetti il tuo repository locale a GitHub

Successivamente, ti consigliamo di connettere il tuo repository locale a GitHub. Dopo aver creato un repository vuoto su GitHub, aggiungilo come remoto al tuo progetto locale:

```sh
cd code/your/project/folder
git remote add origin https://github.com/your-username/your-repository.git
```

Sostituisci `your-username` e `your-repository` con il tuo nome utente GitHub effettivo e il nome del repository. Questo comando collega il tuo progetto locale al repository remoto su GitHub.

### 6. Metti in scena i tuoi file

Prima di poter salvare le modifiche in Git, devi dire a Git quali file hai modificato e vuoi salvare. Questo si chiama "staging" dei tuoi file. Utilizzare il comando seguente per mettere in scena tutti i file modificati o nuovi:

```sh
git add .
```Questo dice a Git di tenere traccia delle modifiche apportate a tutti i file nel tuo progetto. È inoltre possibile organizzare file specifici sostituendo `.` con il nome del file.

### 7. Applica le tue modifiche

Dopo lo staging, il passo successivo è salvare (o "impegnare") le modifiche nel repository Git locale. Quando ti impegni, dovresti sempre includere un messaggio che descriva le modifiche apportate. Ad esempio:

```sh
git commit -m "Add new feature"
```

Il flag `-m` ti consente di scrivere un messaggio che riassume le modifiche apportate. Questo messaggio aiuta te e gli altri a capire cosa è stato modificato in questo commit.

### 8. Invia le tue modifiche a GitHub

Dopo aver eseguito il commit delle modifiche localmente, ora puoi "spingerle" su GitHub in modo che il tuo repository remoto venga aggiornato. Esegui il comando seguente per caricare le modifiche:

```sh
git push -u origin main
```

Il ramo `main` è il ramo predefinito in GitHub in cui è archiviato il codice e questo comando carica le modifiche locali nel repository remoto, rendendole accessibili online.

### 9. Visualizza il tuo codice su GitHub

Dopo aver inviato il codice a GitHub, puoi visualizzare il tuo repository nell'interfaccia web di GitHub. Dovresti vedere i file dal tuo repository locale, insieme a una cronologia dei commit che mostra le modifiche apportate.

## Conclusione

Organizzando il tuo codice in cartelle dedicate e utilizzando GitHub per gestire ed eseguire il backup dei tuoi repository, manterrai i tuoi progetti ben strutturati e facilmente accessibili. Una volta che hai una versione funzionante del tuo codice, inviala a GitHub. Puoi quindi tenere facilmente traccia di eventuali modifiche utilizzando l'interfaccia Web GitHub o Visual Studio Code, che evidenzia le righe modificate. Questo approccio ti consente di continuare a perfezionare ed espandere il tuo codice senza perdere traccia dei progressi o delle modifiche.

Git e piattaforme come GitHub e GitLab sono strumenti potenti e, sebbene possano essere complessi, sono disponibili numerose risorse online per aiutarti a comprenderli meglio. Una delle risorse più preziose che ho trovato sono gli aiutanti AI come ChatGPT. Puoi descrivere ciò che devi realizzare e questi strumenti ti guideranno pazientemente attraverso il processo passo dopo passo.

## Glossario

Ecco alcuni termini comuni che incontrerai quando lavori con Git e GitHub:- **Commit**: un'istantanea delle modifiche apportate al repository. Ogni commit include un messaggio che descrive cosa è stato modificato e crea un record storico a cui puoi fare riferimento o a cui puoi tornare in seguito.
- **Repository (Repo)**: una raccolta di file e la relativa cronologia tracciata da Git. I repository possono esistere localmente sul tuo computer o in remoto su piattaforme come GitHub. Ogni progetto viene in genere archiviato nel proprio repository.
- **Remoto**: un repository remoto è una versione del tuo progetto ospitata su una piattaforma come GitHub. La versione locale del tuo progetto sul tuo computer è collegata a questo telecomando in modo che tu possa caricare (push) e scaricare (pull) le modifiche.
- **Staging**: il processo di preparazione dei file per un commit. Quando metti in stage un file, stai dicendo a Git che vuoi includerlo nel prossimo commit. La gestione temporanea ti consente di scegliere quali modifiche includere in un commit.
- **Push**: l'atto di inviare le modifiche apportate dal repository locale a un repository remoto (ad esempio GitHub), in modo che altri possano accedere alla versione aggiornata del tuo codice.
- **Pull**: l'atto di recuperare le modifiche da un repository remoto per aggiornare la copia locale. Puoi apportare modifiche quando desideri sincronizzare il tuo repository locale con la versione più recente dal remoto.
- **Origine**: il nome predefinito per un repository remoto quando colleghi per la prima volta il tuo repository locale a un repository remoto. In genere si riferisce all'URL principale del tuo progetto su GitHub.