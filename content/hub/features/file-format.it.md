---
title: "Formato file (.lum)"
type: docs
---
Lumi utilizza un formato file aperto basato su directory (`.lum`) progettato per prestazioni, affidabilità e accessibilità a lungo termine.

## Panoramica

Un file `.lum` è in realtà una directory contenente:
- **Metadati** (livelli, metodi di fusione, proprietà).
- **Buffer dei livelli** (dati dei singoli pixel per ciascun livello).
- **Maschere** (dati in scala di grigi per le maschere di livello).
- **Cronologia di ripristino** (istantanee incrementali).

Questa struttura consente il salvataggio rapido, il caricamento lento di file di grandi dimensioni e il ripristino del lavoro anche dopo un arresto anomalo.

## Proprietà chiave

### Aperto e leggibile

Il formato `.lum` utilizza metadati XML e buffer binari compressi. Puoi controllare la struttura, le proprietà e i metodi di fusione dei livelli nel testo normale. Nessun codec proprietario; i dati pixel vengono archiviati nel formato buffer GEGL standard.

### Salvataggio incrementale

Il salvataggio incrementale deve essere abilitato per progetto nella **finestra di dialogo Salva con nome** (una casella di controllo **Salvataggio incrementale** e un pulsante di selezione **Risparmi massimi**). Una volta abilitato, Ctrl+S scrive solo i layer modificati anziché riscrivere l'intero progetto, riducendo drasticamente i tempi di salvataggio. L'impostazione viene archiviata con il progetto e persiste tra le sessioni.

### Caricamento lento

I grandi progetti si aprono velocemente. I pixel del livello vengono caricati dal disco solo quando:
- Il livello viene reso visibile.
- Dipingi sullo strato.
- Il livello viene esportato o composto.

I progetti molto grandi (oltre 500 livelli, più gigabyte di dati) rimangono reattivi. Il caricamento lento è abilitato per impostazione predefinita e può essere attivato in **Modifica → Preferenze → Prestazioni → Risorse di memoria**.

### Salvataggio automatico

Lumi salva automaticamente le modifiche in una **posizione cache separata** (`~/.cache/lumi/autosave/`) a intervalli regolari. I salvataggi automatici sono indipendenti dal file di lavoro e non lo modificano. L'intervallo e la posizione della cache sono configurabili in **Modifica → Preferenze → Prestazioni**.

## Accesso

### Salva e salva con nome

- **File** → **Salva** (Ctrl+S): salva nella directory `.lum` corrente.
- **File** → **Salva con nome** (Shift+Ctrl+S): salva in un nuovo file `.lum`. La finestra di dialogo Salva con nome include opzioni per il tipo di compressione e un interruttore **Salvataggio incrementale** (con un limite di **Salvataggi massimi**) per abilitare o disabilitare il salvataggio incrementale per questo progetto.

Le modifiche non salvate sono indicate da un asterisco (*) nel titolo della finestra.

### Esporta

- **File** → **Esporta come** (Shift+Ctrl+E): esporta in PNG, JPEG, TIFF o altri formati.
- **File** → **Sovrascrivi** (Ctrl+E): riesporta nell'ultimo file esportato.

L'esportazione appiattisce i livelli visibili e converte dallo spazio colore spettrale a sRGB.

### Importa

- **File** → **Apri** (Ctrl+O): carica un progetto `.lum`.
- **File** → **Apri come livelli** (Shift+Ctrl+O): importa file `.lum`, XCF o PSD come nuovi livelli.
- **File** → **File recenti**: accesso rapido ai progetti aperti di recente.

I file PSD e XCF vengono convertiti nel formato nativo di Lumi al momento dell'importazione.

## Compatibilità di importazione ed esportazione

### Formati di importazione supportati
- **.lum**: formato nativo Lumi.
- **.xcf**: formato nativo di GIMP (livelli e proprietà di base preservate).
- **.psd**: formato Photoshop (livelli e metodi di fusione conservati).
- **PNG, JPEG, TIFF, ecc.**: importazione di immagini appiattite.

### Formati di esportazione supportati
- **PNG**: senza perdita di dati, con trasparenza alfa.
- **JPEG**: con perdita, appiattito.
- **TIFF**: senza perdita di dati o compresso LZW.
- **XCF**: formato compatibile con GIMP. Solo esportazione; strati e proprietà di base preservate.

## Recupero del progettoLumi mantiene salvataggi automatici in background e checkpoint incrementali manuali, entrambi accessibili da **File** → **Recupera immagine**. Consulta la pagina [File Recovery](../recovery) per i dettagli completi.

## Organizzazione

Un file `.lum` è una directory con una struttura fissa:

```
my-painting.lum/
  ├── metadata.xml                       (image structure, layer tree, properties)
  ├── thumbnail-YYYYMMDD-HHMMSS.png      (last-saved thumbnail)
  ├── drawables/
  │   ├── layer-<name>.geglbuf           (pixel data per layer)
  │   └── mask-<name>.geglbuf            (mask data, shares layer name)
  ├── icc/                               (embedded colour profiles)
  ├── parasites/                         (per-image metadata)
  ├── paths/                             (vector paths as SVG)
  ├── configs/                           (non-destructive filter configurations)
  └── recovery/
      └── primary-01.lum/                (incremental save checkpoint)
          ├── metadata.xml
          ├── drawables/                 (only modified buffers)
          ├── delta-0001.lum/            (Ctrl+S checkpoint)
          └── delta-0002.lum/
```

I buffer dei livelli prendono il nome dal livello (`layer-Background.geglbuf`), non numerati in sequenza. Gli spazi nei nomi dei livelli vengono memorizzati come trattini bassi; i livelli di gruppo ottengono un suffisso `-GROUP`. Le maschere condividono il nome del livello (`mask-Background.geglbuf`).

Ogni `recovery/primary-NN.lum/` è un salvataggio di base completo. Le successive pressioni di Ctrl+S aggiungono `delta-NNNN.lum/` sottodirectory contenenti solo i buffer modificati dall'ultima linea di base, mantenendo rapidi i salvataggi del punto di controllo indipendentemente dalle dimensioni del progetto.

I salvataggi automatici seguono la stessa struttura ma vengono archiviati separatamente in `~/.cache/lumi/autosave/`, lasciando intatto il file di lavoro.
- **Progetti molto grandi**: un progetto con oltre 1000 livelli e terabyte di dati trarrà maggiori benefici dal caricamento lento; tuttavia, l'esportazione finale nel formato immagine flat potrebbe richiedere del tempo.
- **Unità di rete**: il salvataggio nelle directory montate in rete è supportato ma è più lento dell'archiviazione locale a causa della latenza I/O.