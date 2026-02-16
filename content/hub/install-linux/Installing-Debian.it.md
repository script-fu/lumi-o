---
title: "Installazione di Debian"
type: docs
url: "hub/install-linux/Installing-Debian"
---
Questo documento descrive il processo utilizzato per installare Debian Stable come sistema operativo di sviluppo Lumi·o. Potrebbe essere utile per altri che creano un ambiente simile.

Debian Stable è stata selezionata perché Lumi mira a costruire in modo affidabile su una piattaforma prevedibile a lungo termine. Lo sviluppo di GIMP mira a Debian Testing, rendendo Debian Stable un sistema di base strettamente allineato.

Se provieni da Windows, il principale cambiamento concettuale è che la maggior parte dell'installazione e della configurazione del software avviene tramite gestori di pacchetti e semplici comandi del terminale anziché tramite programmi di installazione scaricabili.

## A chi è rivolta questa guida

Questa guida documenta una configurazione Debian Stable funzionante utilizzata per lo sviluppo di Lumi. Non è un tutorial generale sull'installazione di Linux.

È molto utile per:

- artisti che passano da Windows e desiderano una configurazione Linux prevedibile
- sviluppatori che costruiscono Lumi dal sorgente
- utenti che preferiscono riprodurre un ambiente di lavoro conosciuto piuttosto che progettare la propria configurazione di sistema

Si presuppone una familiarità di base con il partizionamento del disco e un semplice utilizzo della riga di comando.

## Effettua il backup dei tuoi dati

Prima di installare Debian, crea un backup completo della tua directory Home su un'unità esterna. Includi eventuali cartelle di dati aggiuntive che desideri conservare.

Nota: in Linux, `~` rappresenta la directory Home.

Se utilizzi repository Git, esegui il push di eventuali modifiche importanti alle loro origini in modo che possano essere ripristinate facilmente dopo l'installazione. Questo passaggio è rilevante solo se usi già Git.

## Crea una partizione

Crea spazio sul tuo disco primario per Debian. Esistono molte guide e strumenti per questo passaggio, incluso GParted. A seconda della configurazione, puoi:

- ridurre una partizione Windows esistente per il dual boot
- riutilizzare una partizione Linux esistente
- preparare nuovo Linux e scambiare partizioni

Se non sei sicuro, consulta le guide specifiche dell'hardware prima di apportare modifiche, poiché i passaggi di partizionamento variano in modo significativo tra i sistemi.


## Crea un USB di installazione Debian

Supponendo che esistano già una partizione di destinazione e uno spazio di swap:

1. Scarica l'ISO Debian dal sito ufficiale: https://www.debian.org/
2. Su Windows, utilizzare BalenaEtcher per scrivere l'ISO su un'unità USB.
3. Su Linux, utilizzare uno strumento da riga di comando come `dd` per creare un USB avviabile.

## Installa Debian

1. Inserire l'unità USB.
2. Riavviare e premere il tasto del menu di avvio (comunemente `F2`, `F12`, `Esc` o `Del`) durante l'avvio.
3. Selezionare il dispositivo USB.
4. Scegli un programma di installazione non grafico.
5. Lascia vuota la password di root quando richiesto in modo che il programma di installazione conceda l'accesso sudo al tuo account utente.
6. Partizionare manualmente:

   - Filesystem: ext4 (journaling)
   - Swap: partizione di swap esistente
   - Punto di montaggio: `/`
   - Etichetta: `linux`
   - Nome host: nome del sistema mostrato come `user@hostname`
   - Account utente: il tuo nome completo
   - Nome utente: nome di accesso al terminale

7. Seleziona **Cinnamon** come ambiente desktop.
8. Completa l'installazione e riavvia in Debian Stable.

## Configurazione del sistema

### Ridimensionamento della visualizzazione

Debian Stable attualmente gestisce il ridimensionamento frazionario in modo incoerente, specialmente su display 4K. Invece di ridurre la risoluzione del display, regola direttamente gli elementi dell'interfaccia.

Aggiustamenti consigliati:- Evitare il ridimensionamento della visualizzazione frazionario.
- Menu → Selezione carattere → Impostazioni carattere → Fattore di scala testo: `2.5`
- Carattere desktop: `14`
- Pannello → Personalizza → Altezza pannello: `60`
- Aspetto del pannello → Dimensioni icona simbolica zona destra: `48px`
- Mouse e touchpad → Regolazione dimensione puntatore
- Desktop (tasto destro) → Personalizza → Dimensione icona più grande

Regolazione di Firefox:

- Barra degli indirizzi → `about:config`
- Imposta `layout.css.devPixelsPerPx` su `1`

### Terminale

Configura le preferenze del terminale:

1. Menu → Terminale → Modifica → Preferenze
2. Testo → Dimensione iniziale: `140 columns`, `40 rows`
3. Testo → Carattere personalizzato: `Monospace 10`
4. Colori → Schemi integrati → Solarized Dark

## Ripristina dati

Ripristina i file di backup nella directory Home secondo necessità, ad esempio:

- `Backup/Home/Artwork` → `~/Artwork`
- `Backup/Home/code` → `~/code`
- `Backup/Home/Desktop` → `~/Desktop`
- `Backup/Home/.ssh` → `~/.ssh`
- `Backup/Home/.config/lumi` → `~/.config/lumi`

Nota: le cartelle che iniziano con `.` sono directory di configurazione nascoste in Linux.

## Facoltativo: configurazione Git

Richiesto solo se prevedi di creare Lumi o ripristinare i repository.

### Installa Git

```bash
sudo apt install git
```

Configura la tua identità:

```bash
git config --global --edit
```

#### Accesso a GitLab

Ripristina l'accesso al repository su GitLab o GitHub:

1. Modificare le autorizzazioni sul file della chiave SSH: `chmod 600 ~/.ssh/id_rsa`
2. Aggiungi l'utente alla nuova installazione Git: `ssh-add ~/.ssh/id_rsa`
3. Testare la connessione: `ssh -T git@ssh.gitlab.gnome.org` o `ssh -T git@github.com`

Per ogni repository, recupera le origini e reimposta il ramo locale in modo che corrisponda:

```bash
git reset --hard remote-name/branch-name
git clean -df
```

Esegui `git status` per verificare che i repository siano puliti.

Ora disponiamo di un nuovo sistema operativo con tutti i dati e i repository ripristinati. Questa configurazione riflette un ambiente di lavoro noto utilizzato per lo sviluppo di Lumi e può essere adattato ai singoli flussi di lavoro secondo necessità.

## Costruisci Lumi dopo l'installazione del sistema operativo

Gli script di build Lumi si trovano in:

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