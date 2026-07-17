---
title: "Usare Git su Linux"
type: docs
url: "hub/technical-guides/Using-Git-on-Linux"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 7054a9ff9efeb93b4f494197e09ed1fe34d5d6bde7bc305480693c3982d375ae
---

Benvenuto in questa guida per principianti sull'uso di Git su Linux! È pensata per aiutarti a iniziare con Git e GitLab e per offrirti una conoscenza di base su come usare questi strumenti.

## Panoramica di Git

Il codice usato per creare le applicazioni è conservato in una raccolta di cartelle e file sul tuo sistema. Git è un'applicazione che ci consente di eseguire backup, condividere e copiare quella raccolta. Git è un sistema di controllo versione che ti permette di tracciare le modifiche al codice e collaborare con altri. È uno strumento molto diffuso nella comunità open source. GitLab è una piattaforma web che ti consente di ospitare e gestire i repository Git online, facilitando la collaborazione e il tracciamento delle modifiche al codice.

## Cos'è un repository?

Un _repo_, abbreviazione di repository, è una cartella locale gestita da Git con una copia online. Un repository GitLab è una raccolta di file e cartelle che compongono un progetto. Può avere _branch_ che sono copie indipendenti dello stesso progetto. Un branch è una versione separata del tuo progetto che ti consente di apportare modifiche senza influenzare la versione principale. Ciò è utile per testare nuove funzionalità o correggere bug senza interrompere il progetto principale. C'è il tuo repository locale, archiviato sul disco rigido, e il repository remoto, archiviato online tramite Git e GitLab.

## Usare Git

Dovrai installare Git sul tuo sistema. Sui sistemi basati su Debian, puoi usare il comando apt per installare pacchetti software. In questo caso, lo usiamo per installare Git, il pacchetto che fornisce il sistema di controllo versione Git. Il comando sudo concede all'installer il permesso di installare sul tuo sistema.

```bash
 sudo apt install git
```

## Accedere a GitLab

Prima di poter usare [GitLab](https://gitlab.com/users/sign_up), dovrai creare un account visitando il sito web di GitLab e completando la registrazione.

GitLab richiede _SSH_ per una comunicazione sicura e autenticata tra un client (tu, ad esempio) e il server GitLab quando esegui operazioni Git come _clonare_, _push_ e _fetch_ dei repository. Clonare significa creare una copia locale del repository; il fetch porta nella copia locale le modifiche effettuate nel remoto; il push invia modifiche e contenuti al repository sul server. SSH (Secure Shell) è un protocollo di rete che consente l'accesso remoto sicuro e usa _coppie di chiavi_ per autenticarsi e stabilire connessioni sicure. Per generare una coppia di chiavi SSH, puoi usare il comando ssh-keygen nel terminale.

```bash
 ssh-keygen
```

Specifica un nome file o usa quello predefinito premendo Invio e, facoltativamente, una password. Nella directory home, in una cartella nascosta chiamata .ssh, compariranno ora due file id_rsa se hai scelto i nomi predefiniti. Il file .pub è la chiave pubblica e puoi vederne il contenuto con un editor di testo.

Accedi al tuo account GitLab e vai alle impostazioni utente. Fai clic su «Chiavi SSH» nel menu di navigazione a sinistra. Copia e incolla la chiave pubblica nel campo Chiave e assegna alla chiave un titolo descrittivo, come PC@Home. Fai clic sul pulsante «Aggiungi chiave» per salvarla. La chiave pubblica SSH è ora aggiunta al tuo account GitLab e puoi usarla per autenticarti con i repository GitLab. Verifica che chiavi e connessione funzionino con il comando ssh -T per vedere un messaggio di benvenuto da GitLab.

```bash
 $ ssh -T git@ssh.gitlab.gnome.org
 Welcome to GitLab, @username!
```

## Comandi Git di base

Ora che hai installato Git e configurato la chiave SSH con GitLab, vediamo alcuni comandi Git essenziali per gestire i repository. Questi comandi ti aiuteranno a lavorare con progetti esistenti, mantenerli aggiornati e apportare modifiche in modo sicuro.

### 1. **Clonare un repository**

La clonazione è il processo di creazione di una copia locale di un repository remoto. È utile quando vuoi lavorare su un progetto già presente su GitLab. Per clonare un repository, usa il comando `git clone` seguito dall'URL del repository:

```sh
git clone https://gitlab.com/username/repository.git
```

Sostituisci `https://gitlab.com/username/repository.git` con l'URL del repository che desideri clonare. Questo comando creerà una copia locale del repository in una nuova directory.

### 2. **Verificare lo stato del repository**

Per vedere se il repository locale ha modifiche o consultarne lo stato attuale, usa:

```sh
git status
```

Questo comando mostra quali file sono stati modificati, aggiunti o eliminati nella copia locale del repository.

### 3. **Repository remoti**

I repository remoti sono versioni del tuo progetto ospitate online, ad esempio su GitLab. Funzionano come posizione centrale in cui è archiviato il codice e a cui possono accedere altri. Il repository remoto predefinito che Git crea quando cloni un progetto si chiama `origin`. Puoi aggiungere, rimuovere o elencare repository remoti con i seguenti comandi:

- **Elencare i remote:**

  Per vedere quali repository remoti sono collegati al progetto locale, usa:

  ```sh
  git remote -v
  ```

  Questo comando elenca tutti i remote e i relativi URL. Di solito vedrai `origin` nell'elenco.

- **Aggiungere un remote:**

  Se devi aggiungere un nuovo repository remoto, puoi farlo con:

  ```sh
  git remote add <name> <url>
  ```

  Sostituisci `<name>` con un nome per il remote e `<url>` con l'URL del repository.

- **Rimuovere un remote:**

  Per rimuovere un repository remoto, usa:

  ```sh
  git remote remove <name>
  ```

  Sostituisci `<name>` con il nome del remote che desideri rimuovere.

### 4. **Recuperare le modifiche dal repository remoto**

Se vuoi vedere quali modifiche sono state apportate al repository remoto senza applicarle alla copia locale, usa:

```sh
git fetch origin
```

Questo comando recupera le ultime modifiche dal repository remoto ma non le unisce al branch locale. È un modo per verificare gli aggiornamenti prima di decidere di incorporarli.

### 5. **Reimpostare il repository locale**

Se vuoi reimpostare il repository locale in modo che corrisponda esattamente al repository remoto, puoi usare un reset «hard». **Attenzione:** questa operazione sovrascriverà tutte le modifiche locali apportate.

```sh
git reset --hard origin/branch-name
```

Sostituisci `branch-name` con il nome del branch che desideri reimpostare. Questo comando scarterà qualsiasi modifica locale e renderà il repository locale identico a quello remoto.

### 6. **Visualizzare la cronologia dei commit**

Per vedere un elenco delle modifiche apportate al repository nel tempo, usa:

```sh
git log
```

Questo comando visualizza una cronologia dei commit, inclusi autore, data e messaggio per ogni modifica. È utile per capire quali modifiche sono state apportate e quando.

### Riepilogo

Questi comandi Git di base ti aiuteranno a lavorare con i repository, mantenere aggiornate le copie locali e gestire i repository remoti in modo sicuro. Clonare repository, verificare lo stato della copia locale e gestire i repository remoti sono competenze chiave per amministrare progetti con Git.
