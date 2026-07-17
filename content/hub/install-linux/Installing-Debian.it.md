---
title: "Installazione di Debian"
type: docs
url: "hub/install-linux/Installing-Debian"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 42b2f95f8ff71be8eff6777dfd9808855f99f943c55575079566588a08fd8fcd
---

Questo documento descrive il processo utilizzato per installare Debian Stable come sistema operativo di sviluppo di Lumi-o. Può essere utile a chi configura un ambiente simile.

Debian Stable è stata scelta perché Lumi è pensato per compilarsi in modo affidabile su una piattaforma prevedibile a lungo termine. Lo sviluppo di GIMP punta a Debian Testing, il che rende Debian Stable una base molto allineata.

Lumi dà il meglio di sé su Debian con Cinnamon (X11) ed è sviluppato e testato in quell'ambiente. Cinnamon offre un flusso di lavoro desktop familiare, simile a Windows, mentre X11 ha fornito l'ambiente più stabile per lo sviluppo di Lumi.

Se provieni da Windows, il principale cambiamento concettuale è che la maggior parte dell'installazione e della configurazione del software avviene tramite gestori di pacchetti e semplici comandi da terminale, anziché tramite programmi di installazione scaricabili.

## A chi è rivolta questa guida

Questa guida documenta una configurazione Debian Stable funzionante utilizzata per lo sviluppo di Lumi. Non è un tutorial generale sull'installazione di Linux.

È particolarmente utile per:

- artisti che passano da Windows e desiderano una configurazione Linux prevedibile
- sviluppatori che compilano Lumi dal codice sorgente
- utenti che preferiscono riprodurre un ambiente di lavoro noto piuttosto che progettare la propria configurazione di sistema

Si presuppone una familiarità di base con il partizionamento del disco e un semplice utilizzo della riga di comando.

## Effettua il backup dei tuoi dati

Prima di installare Debian, crea un backup completo della tua directory Home su un'unità esterna. Includi eventuali cartelle di dati aggiuntive che desideri conservare.

Nota: in Linux, `~` rappresenta la directory Home.

Se utilizzi repository Git, esegui il push delle modifiche importanti verso le rispettive origini, così da poterle ripristinare facilmente dopo l'installazione. Questo passaggio è rilevante solo se usi già Git.

## Crea una partizione

Crea spazio sul disco principale per Debian. Esistono molte guide e strumenti per questo passaggio, incluso GParted. A seconda della configurazione, puoi:

- ridurre una partizione Windows esistente per il dual boot
- riutilizzare una partizione Linux esistente
- preparare nuove partizioni Linux e di swap

Se non sei sicuro, consulta le guide specifiche per il tuo hardware prima di apportare modifiche, poiché i passaggi di partizionamento variano molto tra i sistemi.


## Crea un USB di installazione Debian

Supponendo che esistano già una partizione di destinazione e uno spazio di swap:

1. Scarica l'ISO Debian dal sito ufficiale: https://www.debian.org/
2. Su Windows, usa BalenaEtcher per scrivere l'ISO su un'unità USB.
3. Su Linux, usa uno strumento da riga di comando come `dd` per creare un USB avviabile.

## Installa Debian

1. Inserisci l'unità USB.
2. Riavvia e premi il tasto del menu di avvio (di solito `F2`, `F12`, `Esc` o `Del`) durante l'avvio.
3. Seleziona il dispositivo USB.
4. Scegli un programma di installazione non grafico.
5. Lascia vuota la password di root quando richiesto, così che il programma di installazione conceda l'accesso sudo al tuo account utente.
6. Partiziona manualmente:

   - Filesystem: ext4 (journaling)
   - Swap: partizione di swap esistente
   - Punto di montaggio: `/`
   - Etichetta: `linux`
   - Nome host: nome del sistema mostrato come `user@hostname`
   - Account utente: il tuo nome completo
   - Nome utente: nome di accesso al terminale

7. Il programma di installazione Debian offre una scelta dell'ambiente desktop in questa fase; seleziona **Cinnamon** per la configurazione consigliata da Lumi.
8. Completa l'installazione e riavvia in Debian Stable.

## Configurazione del sistema

### Ridimensionamento del display

Debian Stable gestisce attualmente il ridimensionamento frazionario in modo incoerente, soprattutto sui display 4K. Invece di ridurre la risoluzione del display, regola direttamente gli elementi dell'interfaccia.

Regolazioni consigliate:

- Evita il ridimensionamento frazionario del display.
- Menu → Selezione carattere → Impostazioni carattere → Fattore di scala testo: `2.5`
- Carattere desktop: `14`
- Pannello → Personalizza → Altezza pannello: `60`
- Aspetto del pannello → Dimensione icona simbolica zona destra: `48px`
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

## Ripristina i dati

Ripristina i file di backup nella directory Home secondo necessità, ad esempio:

- `Backup/Home/Artwork` → `~/Artwork`
- `Backup/Home/code` → `~/code`
- `Backup/Home/Desktop` → `~/Desktop`
- `Backup/Home/.ssh` → `~/.ssh`
- `Backup/Home/.config/lumi` → `~/.config/lumi`

Nota: le cartelle che iniziano con `.` sono directory di configurazione nascoste in Linux.

## Facoltativo: configurazione Git

Necessario solo se prevedi di compilare Lumi o ripristinare i repository.

### Installa Git

```bash
sudo apt install git
```

Configura la tua identità:

```bash
git config --global --edit
```

#### Accesso a GitLab

Ripristina l'accesso ai repository su GitLab o GitHub:

1. Modifica i permessi del file della chiave SSH: `chmod 600 ~/.ssh/id_rsa`
2. Aggiungi la chiave alla nuova installazione Git: `ssh-add ~/.ssh/id_rsa`
3. Prova la connessione: `ssh -T git@ssh.gitlab.gnome.org` o `ssh -T git@github.com`

Per ogni repository, recupera le origini e reimposta il ramo locale in modo che corrisponda:

```bash
git reset --hard remote-name/branch-name
git clean -df
```

Esegui `git status` per verificare che i repository siano puliti.

Ora hai un nuovo sistema operativo con dati e repository ripristinati. Questa configurazione riflette un ambiente di lavoro noto utilizzato per lo sviluppo di Lumi e può essere adattata ai singoli flussi di lavoro secondo necessità.

## Compila Lumi dopo la configurazione del sistema operativo

Gli script di build di Lumi si trovano in:

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
