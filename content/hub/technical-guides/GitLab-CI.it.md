---
title: "GitLabCI"
type: docs
url: "hub/technical-guides/GitLab-CI"
---
L'integrazione continua (CI) è un modo per testare, creare e convalidare automaticamente il codice ogni volta che vengono apportate modifiche.

**GitLab** fornisce funzionalità CI/CD integrate tramite il file `.gitlab-ci.yml`. Questo file, inserito nella radice del tuo repository, indica a GitLab come costruire e testare il tuo progetto. Definisce fasi e script che vengono eseguiti in un ambiente pulito ogni volta che vengono apportate modifiche.

Questo documento descrive il funzionamento della pipeline CI/CD GitLab di Lumi, compreso il ruolo del file `.gitlab-ci.yml`, degli script di shell e degli strumenti esterni come Meson e Ninja.

Per la documentazione tecnica dettagliata del processo di creazione di Lumi CI, vedere [README-CI.md](https://gitlab.gnome.org/pixelmixer/lumi/-/blob/main/build/linux/appimage/README-CI.md) nel repository.

## Nozioni di base su CI/CD GitLab

Il CI è controllato da un file denominato `.gitlab-ci.yml`. Questo file definisce:

- **Fasi**: gruppi ordinati di lavori (ad esempio, `build-this`, `build-that`, `package-up`)
- **Lavori**: attività individuali da eseguire all'interno di ciascuna fase
- **Script**: comandi shell eseguiti per ogni lavoro
- **Runner**: computer utilizzati da GitLab per eseguire i processi definiti nella pipeline.

In Lumi, le fasi della pipeline sono:

- `dependencies`
- `build lumi`
- `appimage`

## Build basate su contenitori

La pipeline Lumi utilizza la containerizzazione per build coerenti:

1. **Creazione del contenitore di build**: la prima fase utilizza Buildah per creare un'immagine Docker con tutte le dipendenze
2. **Utilizzo del contenitore**: le fasi successive vengono eseguite all'interno di questo contenitore, garantendo un ambiente coerente
3. **Build riproducibili**: l'isolamento del contenitore garantisce gli stessi risultati su diversi corridori

Questo approccio garantisce che le build funzionino allo stesso modo in qualsiasi runner GitLab e fornisce un ambiente controllato per processi di build complessi.

## Ruolo degli script di shell

I lavori in `.gitlab-ci.yml` in genere richiamano direttamente i comandi della shell. Le operazioni complesse vengono spesso spostate in script separati archiviati nel repository.

Lumi CI utilizza script di shell modulari per organizzare la logica di creazione:

**Esempio di invocazione di script:**
```yaml
script:
  - bash build/linux/appimage/lumi-goappimage.sh 2>&1 | tee appimage_creation.log
```

**Vantaggi di questo approccio:**
- **Clean YAML**: mantiene il file `.gitlab-ci.yml` focalizzato sulla struttura del lavoro
- **Manutenibilità**: la logica complessa è più semplice da eseguire il debug e modificare negli script di shell
- **Riutilizzabilità**: gli script possono essere utilizzati in diversi contesti o ambienti
- **Modularità**: diversi aspetti della build possono essere separati in script mirati

Ciò mantiene pulita la configurazione della CI consentendo al tempo stesso processi di creazione sofisticati.

## Integrazione con i sistemi di costruzione

Lumi utilizza **Meson** e **Ninja** per preparare e quindi creare il codice.

Ad esempio:

```
script:
  - meson setup _build-${CI_RUNNER_TAG} -Dprefix="${LUMI_PREFIX}"
  - ninja -C _build-${CI_RUNNER_TAG}
  - ninja -C _build-${CI_RUNNER_TAG} install
```

Qui:

- `meson setup` prepara la directory di build e genera `build.ninja`
- `ninja` esegue i comandi di compilazione come definito

## Struttura del sistema di costruzione dei mesoni

Il sistema di compilazione **Meson** utilizza un file root `meson.build` posizionato nella directory root del progetto. Questo file definisce la configurazione di compilazione di livello superiore e il punto di ingresso per il processo di compilazione.

- La radice `meson.build` si trova generalmente nella stessa directory di `.gitlab-ci.yml`
- Da lì, **si estende ricorsivamente** in sottodirectory, ognuna delle quali può avere il proprio file `meson.build`
- Questi file di sottodirectory definiscono obiettivi, origini, dipendenze e istruzioni di creazione rilevanti per quella directory

## Variabili d'ambiente

Le variabili chiave nella pipeline Lumi includono:

```yaml
variables:
  DEBIAN_FRONTEND: "noninteractive"  # Prevents interactive prompts
  DEB_VERSION: "trixie"              # Debian version for consistency
  CI_RUNNER_TAG: "x86_64"            # Architecture specification
```

**Variabili specifiche del lavoro:**
```yaml
build-lumi:
  variables:
    COMPILER: "clang"                                           # Compiler selection
    LINKER: "lld"                                               # Linker selection
    LUMI_PREFIX: "${CI_PROJECT_DIR}/_install-${CI_RUNNER_TAG}"  # Installation path
    DEPS_PREFIX: "/opt/lumi-deps"                               # Prebuilt dependency prefix
    MESON_OPTIONS: "-Dpkgconfig.relocatable=true -Drelocatable-bundle=yes"  # Build configuration
```Queste variabili controllano il comportamento della build e garantiscono la coerenza tra le diverse fasi e i diversi corridori.

## Esempio di struttura

```
project-root/
├── .gitlab-ci.yml
├── meson.build              <-- Root Meson file
├── src/
│   ├── meson.build          <-- Subdirectory Meson file
│   └── some_source.c
├── data/
│   ├── meson.build
│   └── icons/
```

In questa struttura:

- Il file root `meson.build` configura l'ambiente di costruzione generale
- I file della sottodirectory `meson.build` gestiscono i dettagli di compilazione per componenti o moduli specifici
- Questo layout gerarchico mantiene la logica di costruzione modulare e manutenibile

## Artefatti tra le fasi

Gli artefatti sono file generati da lavori necessari nelle fasi successive:

```yaml
build-lumi:
  # ...job configuration...
  artifacts:
    paths:
      - "${LUMI_PREFIX}/"      # Installation files
      - _build-${CI_RUNNER_TAG}/meson-logs/meson-log.txt  # Build logs
```

## Fasi e dipendenze della pipeline

La pipeline Lumi è composta da tre fasi principali:

1. **Dipendenze**: crea un ambiente di compilazione containerizzato con tutti gli strumenti e le librerie richiesti
2. **Costruisci Lumi**: compila Lumi utilizzando Meson e Ninja nell'ambiente preparato
3. **AppImage**: impacchetta l'applicazione creata in un formato AppImage distribuibile

**Dipendenze della fase:**
```yaml
build-lumi:
  needs: [deps-debian]  # Waits for dependency container

lumi-appimage:
  needs: [build-lumi] # Waits for application build
```

Ogni fase viene eseguita solo dopo che le relative dipendenze sono state completate correttamente, garantendo il corretto ordine di compilazione e la disponibilità degli artefatti.

## Nomi dei lavori attuali

Il Lumi `.gitlab-ci.yml` attualmente definisce questi nomi di lavoro:

- `deps-debian`
- `build-lumi`
- `lumi-appimage`

## Riepilogo

- `.gitlab-ci.yml` definisce la struttura e la logica della pipeline
- I lavori contengono comandi shell o script esterni
- Strumenti come Meson e Ninja vengono utilizzati all'interno dei lavori come parte del processo di creazione

Lumi utilizza GitLab CI per creare automaticamente la sua AppImage per piattaforme basate su Debian. La pipeline crea dipendenze, compila Lumi e quindi crea il pacchetto AppImage.

Per i dettagli a livello di origine, utilizzare:

- `.gitlab-ci.yml` nella radice del repository Lumi
- `build/linux/appimage/lumi-goappimage.sh`
- `build/linux/appimage/README-CI.md`

Per dettagli tecnici completi sul processo di creazione di Lumi CI, inclusa la configurazione dell'ambiente, l'architettura dello script e la risoluzione dei problemi, fare riferimento a [README-CI.md](https://gitlab.gnome.org/pixelmixer/lumi/-/blob/main/build/linux/appimage/README-CI.md).