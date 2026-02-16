---
title: "Utilizzo di Git su Linux"
type: docs
url: "hub/technical-guides/folder/Using-Git-on-Linux"
---
Benvenuto in questa guida per principianti sull'utilizzo di Git su Linux! Questa guida è progettata per aiutarti a iniziare con Git e GitLab e per fornire una conoscenza di base su come utilizzare questi strumenti.

## Panoramica di Git

Il codice utilizzato per creare applicazioni viene conservato in una raccolta di cartelle e file sul tuo sistema. Git è un'applicazione che ci consente di eseguire il backup, condividere e copiare quella raccolta. Git è noto come sistema di controllo della versione che ti consente di tenere traccia delle modifiche al tuo codice e collaborare con altri. È uno strumento potente ampiamente utilizzato nella comunità open source. GitLab è una piattaforma basata sul Web che ti consente di ospitare e gestire i tuoi repository Git online, semplificando la collaborazione con altri e tenendo traccia delle modifiche al tuo codice.

## Cos'è un repository?

Un _repo_, abbreviazione di repository, è una cartella locale gestita da Git con una copia online. Un repository Git Lab è una raccolta di file e cartelle che compongono un progetto. Può avere _rami_ che sono copie indipendenti dello stesso progetto. Un ramo è una versione separata del tuo progetto che ti consente di apportare modifiche senza influenzare la versione principale. Ciò è utile per testare nuove funzionalità o correggere bug senza interrompere il progetto principale. C'è il tuo repository locale, archiviato sul tuo disco rigido, e il repository remoto, archiviato online utilizzando Git e GitLab.

## Utilizzo di Git

Dovrai installare Git sul tuo sistema. Sui sistemi basati su Debian, è possibile utilizzare il comando apt per installare pacchetti software. In questo caso, lo utilizzeremo per installare Git, che è un pacchetto che fornisce il sistema di controllo della versione Git. Il comando sudo fornisce al programma di installazione l'autorizzazione per l'installazione sul tuo sistema.

```bash
 sudo apt install git
```

## Accedi a GitLab

Prima di poter utilizzare [GitLab](https://gitlab.com/users/sign_up), dovrai creare un account visitando il sito Web GitLab e completando il processo di registrazione.

GitLab richiede _SSH_ per una comunicazione sicura e autenticata tra un client (tu, ad esempio) e il server GitLab quando si eseguono operazioni Git come _cloning_, _pushing_ e _fetching_ repository. La clonazione sta creando una copia locale del repository, il recupero sta portando tutte le modifiche apportate nel repository alla copia locale e il push sta inviando modifiche e contenuti al repository del server. SSH (Secure Shell) è un protocollo di rete che consente l'accesso remoto sicuro e utilizza _coppie di chiavi_ per autenticare e stabilire connessioni sicure. Per generare una coppia di chiavi SSH, puoi utilizzare il comando ssh-keygen nel tuo terminale.

```bash
 ssh-keygen
```

Specificare un nome file o utilizzare quello predefinito premendo Invio e, facoltativamente, una password. Nella tua directory home, in una cartella nascosta chiamata .ssh, ora ci sono due file id_rsa, se hai scelto i nomi predefiniti. Il file .pub è la chiave pubblica e puoi vederne il contenuto con un editor di testo.

Accedi al tuo account GitLab e vai alle impostazioni utente. Fai clic su "Chiavi SSH" nel menu di navigazione a sinistra. Copia e incolla la tua chiave pubblica nel campo Chiave e assegna alla chiave un titolo pertinente, come PC@Home. Fare clic sul pulsante "Aggiungi chiave" per salvare la chiave. La tua chiave pubblica SSH è ora aggiunta al tuo account GitLab e puoi usarla per autenticarti con i repository GitLab. Verifica se le chiavi e la connessione funzionano con il comando ssh -T per visualizzare un messaggio di benvenuto da GitLab.

```bash
 $ ssh -T git@ssh.gitlab.gnome.org
 Welcome to GitLab, @username!
```

## Comandi Git di baseOra che hai installato Git e hai configurato la tua chiave SSH con GitLab, esaminiamo alcuni comandi Git essenziali per la gestione dei repository. Questi comandi ti aiuteranno a lavorare con progetti esistenti, mantenendoli aggiornati e apportando modifiche in modo sicuro.

### 1. **Clonazione di un repository**

La clonazione è il processo di creazione di una copia locale di un repository remoto. Ciò è utile quando vuoi lavorare su un progetto già esistente su GitLab. Per clonare un repository, utilizzare il comando `git clone` seguito dall'URL del repository:

```sh
git clone https://gitlab.com/username/repository.git
```

Sostituisci `https://gitlab.com/username/repository.git` con l'URL del repository che desideri clonare. Questo comando creerà una copia locale del repository in una nuova directory.

### 2. **Verifica dello stato del repository**

Per vedere se il tuo repository locale presenta modifiche o per visualizzare il suo stato corrente, utilizza:

```sh
git status
```

Questo comando ti mostrerà quali file sono stati modificati, aggiunti o eliminati nella tua copia locale del repository.

### 3. **Archivi remoti**

I repository remoti sono versioni del tuo progetto ospitate online, ad esempio su GitLab. Fungono da posizione centrale in cui è archiviato il tuo codice e a cui possono accedere altri. Il repository remoto predefinito che Git crea quando cloni un progetto si chiama `origin`. Puoi aggiungere, rimuovere o elencare repository remoti utilizzando i seguenti comandi:

- **Elenco telecomandi:**

  Per vedere quali repository remoti sono collegati al tuo progetto locale, utilizza:

  ```sh
  git remote -v
  ```

  Questo comando elenca tutti i telecomandi e i relativi URL. In genere, vedrai `origin` elencato qui.

- **Aggiunta di un telecomando:**

  Se devi aggiungere un nuovo repository remoto, puoi farlo con:

  ```sh
  git remote add <name> <url>
  ```

  Sostituisci `<name>` con un nome per il telecomando e `<url>` con l'URL del repository.

- **Rimozione di un telecomando:**

  Per rimuovere un repository remoto, utilizzare:

  ```sh
  git remote remove <name>
  ```

  Sostituisci `<name>` con il nome del telecomando che desideri rimuovere.

### 4. **Recupero delle modifiche dal repository remoto**

Se vuoi vedere quali modifiche sono state apportate al repository remoto senza applicarle alla tua copia locale, usa:

```sh
git fetch origin
```

Questo comando recupera le ultime modifiche dal repository remoto ma non le unisce al tuo ramo locale. È un modo per verificare la presenza di aggiornamenti prima di decidere di incorporarli.

### 5. **Reimpostare il repository locale**

Se desideri reimpostare il tuo repository locale in modo che corrisponda esattamente al repository remoto, puoi utilizzare un ripristino "hard". **Attenzione:** questa operazione sovrascriverà tutte le modifiche locali apportate.

```sh
git reset --hard origin/branch-name
```

Sostituisci `branch-name` con il nome del ramo che desideri ripristinare. Questo comando scarterà qualsiasi modifica locale e renderà il tuo repository locale identico al repository remoto.

### 6. **Visualizzazione della cronologia dei commit**

Per visualizzare un elenco delle modifiche apportate al repository nel tempo, utilizzare:

```sh
git log
```

Questo comando visualizza una cronologia dei commit, incluso l'autore, la data e il messaggio per ogni modifica. È utile per capire quali modifiche sono state apportate e quando.

### Riepilogo

Questi comandi Git di base ti aiuteranno a lavorare con i repository, mantenendo aggiornate le tue copie locali e assicurandoti di poter gestire in sicurezza i repository remoti. La clonazione dei repository, il controllo dello stato della copia locale e la gestione dei repository remoti sono competenze chiave per la gestione dei progetti utilizzando Git.