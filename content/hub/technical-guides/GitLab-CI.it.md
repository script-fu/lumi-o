---
title: "GitLab CI"
type: docs
url: "hub/technical-guides/GitLab-CI"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 9917cebc417adeeae24d91b05b919b679a397d5db652cf4442d4330c0f8eeea5
---

L'integrazione continua (CI) è un modo per testare, compilare e convalidare automaticamente il codice ogni volta che vengono apportate modifiche.

**GitLab** fornisce funzionalità CI/CD integrate tramite il file `.gitlab-ci.yml`. Questo file, posizionato nella radice del repository, indica a GitLab come compilare e testare il progetto. Definisce fasi e script eseguiti in un ambiente pulito ogni volta che vengono inviate modifiche.

Questo documento descrive il funzionamento della pipeline CI/CD GitLab di Lumi, compreso il ruolo del file `.gitlab-ci.yml`, degli script shell e di strumenti esterni come Meson e Ninja.

Per la documentazione tecnica dettagliata del processo di build CI di Lumi, vedi [README-CI.md](https://gitlab.gnome.org/pixelmixer/lumi/-/blob/main/build/linux/appimage/README-CI.md) nel repository.

## Nozioni di base su GitLab CI/CD

La CI è controllata da un file denominato `.gitlab-ci.yml`. Questo file definisce:

- **Fasi**: gruppi ordinati di job (ad es. `build-this`, `build-that`, `package-up`)
- **Job**: attività individuali da eseguire in ciascuna fase
- **Script**: comandi shell eseguiti per ogni job
- **Runner**: macchine che GitLab usa per eseguire i job definiti nella pipeline

In Lumi, le fasi della pipeline sono:

- `dependencies`
- `build lumi`
- `appimage`

## Build basate su contenitori

La pipeline Lumi usa la containerizzazione per build coerenti:

1. **Creazione del contenitore di build**: la prima fase usa Buildah per creare un'immagine Docker con tutte le dipendenze
2. **Utilizzo del contenitore**: le fasi successive vengono eseguite all'interno di questo contenitore, garantendo un ambiente coerente
3. **Build riproducibili**: l'isolamento del contenitore garantisce gli stessi risultati su runner diversi

Questo approccio garantisce che le build funzionino allo stesso modo su qualsiasi runner GitLab e fornisce un ambiente controllato per processi di build complessi.

### Sorgenti di dipendenza integrate

L'immagine delle dipendenze CI di Lumi compila lo stack biforcato da **sorgenti integrate nel repository** (non cloni esterni):

- `lumi-babl/` (BABL)
- `lumi-gegl/` (GEGL)
- `lumi-gtk3/` (GTK3)

Queste directory vengono copiate nel contesto di build del contenitore e compilate nel prefisso delle dipendenze (in genere `/opt/lumi-deps`). Ciò mantiene la CI riproducibile e garantisce che la build dell'AppImage usi la stessa fonte di verità dello sviluppo locale.

## Ruolo degli script shell

I job in `.gitlab-ci.yml` in genere richiamano direttamente comandi shell. Le operazioni complesse vengono spesso spostate in script separati archiviati nel repository.

La CI di Lumi usa script shell modulari per organizzare la logica di build:

**Esempio di invocazione di script:**
```yaml
script:
  - bash build/linux/appimage/lumi-goappimage.sh 2>&1 | tee appimage_creation.log
```

**Vantaggi di questo approccio:**
- **YAML pulito**: mantiene il file `.gitlab-ci.yml` focalizzato sulla struttura dei job
- **Manutenibilità**: la logica complessa è più semplice da eseguire il debug e modificare negli script shell
- **Riutilizzabilità**: gli script possono essere usati in contesti o ambienti diversi
- **Modularità**: diversi aspetti della build possono essere separati in script mirati

Ciò mantiene pulita la configurazione CI consentendo processi di build sofisticati.

## Integrazione con i sistemi di build

Lumi usa **Meson** e **Ninja** per preparare e poi compilare il codice.

Ad esempio:

```
script:
  - meson setup _build-${CI_RUNNER_TAG} -Dprefix="${LUMI_PREFIX}"
  - ninja -C _build-${CI_RUNNER_TAG}
  - ninja -C _build-${CI_RUNNER_TAG} install
```

Qui:

- `meson setup` prepara la directory di build e genera `build.ninja`
- `ninja` esegue i comandi di build definiti

## Struttura del sistema di build Meson

Il sistema di build **Meson** usa un file root `meson.build` posizionato nella directory radice del progetto. Questo file definisce la configurazione di build di livello superiore e il punto di ingresso del processo di compilazione.

- Il `meson.build` root si trova generalmente nella stessa directory di `.gitlab-ci.yml`
- Da lì, **si estende ricorsivamente** alle sottodirectory, ognuna delle quali può avere il proprio file `meson.build`
- Questi file di sottodirectory definiscono target, sorgenti, dipendenze e istruzioni di build rilevanti per quella directory

## Variabili d'ambiente

Le variabili chiave nella pipeline Lumi includono:

```yaml
variables:
  DEBIAN_FRONTEND: "noninteractive"  # Prevents interactive prompts
  DEB_VERSION: "trixie"              # Debian version for consistency
  CI_RUNNER_TAG: "x86_64"            # Architecture specification
```

**Variabili specifiche del job:**
```yaml
build-lumi:
  variables:
    COMPILER: "clang"                                           # Compiler selection
    LINKER: "lld"                                               # Linker selection
    LUMI_PREFIX: "${CI_PROJECT_DIR}/_install-${CI_RUNNER_TAG}"  # Installation path
    DEPS_PREFIX: "/opt/lumi-deps"                               # Prebuilt dependency prefix
    MESON_OPTIONS: "-Dpkgconfig.relocatable=true -Drelocatable-bundle=yes"  # Build configuration
```

Queste variabili controllano il comportamento della build e garantiscono coerenza tra fasi e runner.

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

- Il file root `meson.build` configura l'ambiente generale di build
- I file `meson.build` delle sottodirectory gestiscono i dettagli di compilazione di componenti o moduli specifici
- Questo layout gerarchico mantiene la logica di build modulare e manutenibile

## Artefatti tra le fasi

Gli artefatti sono file generati da job necessari nelle fasi successive:

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

1. **Dependencies**: crea un ambiente di build containerizzato con tutti gli strumenti e le librerie richiesti
2. **Build Lumi**: compila Lumi usando Meson e Ninja nell'ambiente preparato
3. **AppImage**: impacchetta l'applicazione compilata in un formato AppImage distribuibile

**Dipendenze tra fasi:**
```yaml
build-lumi:
  needs: [deps-debian]  # Waits for dependency container

lumi-appimage:
  needs: [build-lumi] # Waits for application build
```

Ogni fase viene eseguita solo dopo il completamento corretto delle relative dipendenze, garantendo l'ordine di build appropriato e la disponibilità degli artefatti.

## Nomi dei job attuali

Il `.gitlab-ci.yml` di Lumi definisce attualmente questi nomi di job:

- `deps-debian`
- `build-lumi`
- `lumi-appimage`

## Riepilogo

- `.gitlab-ci.yml` definisce la struttura e la logica della pipeline
- I job contengono comandi shell o script esterni
- Strumenti come Meson e Ninja vengono usati all'interno dei job come parte del processo di build

Lumi usa GitLab CI per compilare automaticamente la sua AppImage per piattaforme basate su Debian. La pipeline compila le dipendenze, compila Lumi e crea il pacchetto AppImage.

Per i dettagli a livello sorgente, consulta:

- `.gitlab-ci.yml` nella radice del repository Lumi
- `build/linux/appimage/lumi-goappimage.sh`
- `build/linux/appimage/README-CI.md`

Per dettagli tecnici completi sul processo di build CI di Lumi, inclusa la configurazione dell'ambiente, l'architettura degli script e la risoluzione dei problemi, vedi [README-CI.md](https://gitlab.gnome.org/pixelmixer/lumi/-/blob/main/build/linux/appimage/README-CI.md).
