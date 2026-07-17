---
title: "Backup del sistema con Clonezilla"
type: docs
url: "hub/install-linux/System-Backup-Clonezilla"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: b3453289d7da56bb4fc9039616edb73e537acd6a722f0eb8a000e4a398016863
---

È normale eseguire il backup dei file importanti per tornare a versioni precedenti o sostituire dati danneggiati. Tuttavia, un altro tipo di backup essenziale è un **clone del disco**: un backup completo dello stato del sistema.

Una volta che il sistema è configurato e funziona bene, creare un backup completo è fondamentale per ripristinare l'ambiente in caso di disastro. Questo backup integra il salvataggio regolare dei dati di lavoro.

[Clonezilla](https://clonezilla.org/) è un software gratuito e open source per l'imaging e la clonazione del disco. Consente di creare e ripristinare backup completi del disco rigido del computer, ed è quindi uno strumento popolare sia per i professionisti IT sia per gli utenti domestici.

È sempre meglio avere un backup e non averne bisogno piuttosto che averne bisogno e non averlo.


## Caratteristiche principali di Clonezilla

- **Imaging del disco**: Clonezilla crea una copia esatta di un disco rigido, inclusi sistema operativo, applicazioni e dati.
- **Backup e ripristino**: consente di creare un'immagine di backup di un disco rigido e ripristinarla in caso di guasto o migrazione su una nuova unità.
- **Gratuito e open source**: Clonezilla è completamente gratuito e il codice sorgente è disponibile per modifiche e personalizzazioni.


## Backup con Clonezilla

### Passaggi di preparazione

Ti serviranno un'unità USB per Clonezilla e un disco rigido esterno più grande dell'unità interna che intendi clonare.

Questi passaggi semplificano il processo in base alla [guida ufficiale](https://clonezilla.org//fine-print-live-doc.php?path=./clonezilla-live/doc/01_Save_disk_image/00-boot-clonezilla-live-cd.doc#00-boot-clonezilla-live-cd.doc). Conviene consultare la guida completa, che include screenshot per maggiore chiarezza.

1. **Crea un USB o CD/DVD Clonezilla Live**: segui le istruzioni dettagliate sul [sito web di Clonezilla](https://clonezilla.org/liveusb.php) per creare un USB o CD/DVD avviabile.

2. **Collega l'unità di backup esterna**: collega l'unità esterna e assicurati che venga riconosciuta dal sistema. Sarà la destinazione del backup.

3. **Verifica il layout delle partizioni**: usa il comando `lsblk` in un terminale per verificare il layout delle partizioni del disco principale. Annota il nome del dispositivo principale.

4. **Avvia dall'unità USB Clonezilla Live**: riavvia il computer e avvia dal supporto Clonezilla che hai creato. Potrebbe essere necessario accedere alle impostazioni BIOS/UEFI (di solito premendo F2, F12, ESC o DEL durante l'avvio) e modificare l'ordine di avvio per dare priorità all'unità USB.



### Backup con Clonezilla

1. **Seleziona la modalità di backup**: una volta avviato Clonezilla, scegli la modalità "device-device". Questa modalità consente di clonare direttamente l'unità interna su un dispositivo esterno.

2. **Seleziona il dispositivo di origine**: scegli l'unità interna principale.

3. **Seleziona il dispositivo di destinazione**: scegli l'unità di backup esterna come dispositivo di destinazione. Fai attenzione nella selezione del dispositivo per evitare di sovrascrivere dati importanti. Assicurati che l'unità di destinazione abbia dimensioni uguali o superiori a quelle dell'unità di origine.

4. **Avvia il processo di backup**: Clonezilla avvierà il processo di backup. A seconda delle dimensioni della partizione e della velocità delle unità, l'operazione potrebbe richiedere da alcuni minuti ad alcune ore.

5. **Etichetta il backup**: al termine, etichetta l'unità USB e il disco rigido esterno con la data e il sistema di cui hai eseguito il backup. Conservali in un luogo sicuro.

---

### Ripristino dal backup

Se devi ripristinare il tuo sistema Debian dal backup, segui questi passaggi:

1. **Avvia dal supporto Clonezilla**: inserisci l'USB Clonezilla e avvia da esso, seguendo gli stessi passaggi del processo di backup.

2. **Seleziona la modalità di ripristino**: scegli di nuovo la modalità "device-device", ma questa volta ripristinerai dall'immagine di backup. Questo copierà tutti i dati dall'unità esterna all'unità interna.

3. **Seleziona il dispositivo di origine**: scegli l'unità esterna in cui è archiviato il backup.

4. **Seleziona il dispositivo di destinazione**: seleziona l'unità interna su cui vuoi ripristinare il backup.

5. **Avvia il processo di ripristino**: Clonezilla inizierà il processo di ripristino. Come per il backup, il tempo richiesto dipenderà dalle dimensioni dell'unità e dalla velocità dell'hardware.

---

## Note finali

I backup del disco con Clonezilla garantiscono che l'intero sistema —sistema operativo, impostazioni e applicazioni— venga preservato. Con uno sforzo minimo, puoi proteggere il sistema da guasti catastrofici e ridurre al minimo i tempi di inattività in caso di crash.

Ricorda: **i backup sono essenziali**. Aggiorna regolarmente i backup e testali periodicamente per assicurarti di poter ripristinare il sistema quando necessario.

Dopo l'avvio, puoi collegare l'unità di backup esterna e ispezionarne la struttura delle partizioni con l'utilità Dischi in Linux. L'unità di backup dovrebbe rispecchiare la struttura dell'unità interna, con le stesse partizioni e un po' di spazio inutilizzato se l'unità esterna è più grande.
