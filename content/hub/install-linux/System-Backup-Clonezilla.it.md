---
title: "Backup del sistema utilizzando Clonezilla"
type: docs
url: "hub/install-linux/System-Backup-Clonezilla"
---
È normale eseguire il backup dei file importanti per tornare alle versioni precedenti o sostituire i dati danneggiati. Tuttavia, un altro tipo di backup essenziale è un **clone del disco**, un backup completo dello stato del sistema.

Una volta che il sistema è configurato e funziona bene, la creazione di un backup completo è fondamentale per ripristinare l'ambiente in caso di disastro. Questo backup integra il salvataggio regolare dei dati di lavoro.

[Clonezilla](https://clonezilla.org/) è un software gratuito e open source per l'imaging e la clonazione del disco. Consente agli utenti di creare e ripristinare backup completi del disco rigido del proprio computer, rendendolo uno strumento popolare sia per i professionisti IT che per gli utenti domestici.

È sempre meglio avere un backup e non averne bisogno piuttosto che averne bisogno e non averlo.


## Caratteristiche principali di Clonezilla

- **Imaging disco**: Clonezilla crea una copia esatta di un disco rigido, incluso il sistema operativo, le applicazioni e i dati.
- **Backup e ripristino**: consente di creare un'immagine di backup di un disco rigido e ripristinarla in caso di guasto o migrazione su una nuova unità.
- **Gratuito e Open Source**: Clonezilla è completamente gratuito e il codice sorgente è disponibile per modifiche e personalizzazioni.


## Utilizzo di Clonezilla per il backup

### Passaggi di preparazione

Avrai bisogno di un'unità USB per Clonezilla e di un disco rigido esterno più grande dell'unità interna che intendi clonare.

Questi passaggi semplificano il processo basato su [official guide](https://clonezilla.org//fine-print-live-doc.php?path=./clonezilla-live/doc/01_Save_disk_image/00-boot-clonezilla-live-cd.doc#00-boot-clonezilla-live-cd.doc). È una buona idea rivedere la guida completa, che include screenshot per maggiore chiarezza.

1. **Crea un Clonezilla Live USB o un CD/DVD**: seguire le istruzioni dettagliate su Clonezilla [website](https://clonezilla.org/liveusb.php) per creare un USB o un CD/DVD avviabile.

2. **Collega l'unità di backup esterna**: collega l'unità esterna e assicurati che venga riconosciuta dal sistema. Questa sarà la destinazione del tuo backup.

3. **Verifica il layout della partizione**: utilizzare il comando `lsblk` in un terminale per verificare il layout della partizione del disco rigido primario. Prendere nota del nome del dispositivo principale.

4. **Avvia dall'unità USB Clonezilla Live**: riavvia il computer e avvia dal supporto Clonezilla che hai creato. Potrebbe essere necessario accedere alle impostazioni BIOS/UEFI (solitamente premendo F2, F12, ESC o CANC durante l'avvio) e modificare l'ordine di avvio per dare priorità all'unità USB.



### Backup con Clonezilla

1. **Seleziona la modalità di backup**: una volta avviato Clonezilla, scegli la modalità "dispositivo-dispositivo". Questa modalità ti consente di clonare direttamente l'unità interna su un dispositivo esterno.

2. **Seleziona il dispositivo di origine**: scegli l'unità interna primaria.

3. **Seleziona il dispositivo di destinazione**: scegli l'unità di backup esterna come dispositivo di destinazione. Fare attenzione quando si seleziona il dispositivo per evitare di sovrascrivere dati importanti. Assicurarsi che l'unità di destinazione abbia dimensioni uguali o superiori a quelle dell'unità di origine.

4. **Avvia il processo di backup**: Clonezilla avvierà il processo di backup. A seconda delle dimensioni della partizione e della velocità delle unità, l'operazione potrebbe richiedere da alcuni minuti ad alcune ore.

5. **Etichetta il backup**: una volta completato il backup, etichetta l'unità USB e il disco rigido esterno con la data e il sistema di cui hai eseguito il backup. Conservarli in un luogo sicuro.

---

### Ripristino da backup

Se hai bisogno di ripristinare il tuo sistema Debian dal backup, segui questi passaggi:

1. **Avvia da Clonezilla Media**: inserisci Clonezilla USB e avvia da esso, seguendo gli stessi passaggi del processo di backup.2. **Seleziona modalità di ripristino**: scegli di nuovo la modalità "dispositivo-dispositivo", ma questa volta ripristinerai dall'immagine di backup. Questo copierà tutti i dati dal tuo disco esterno al tuo disco interno.

3. **Seleziona il dispositivo di origine**: scegli l'unità esterna in cui è archiviato il backup.

4. **Seleziona il dispositivo di destinazione**: seleziona l'unità interna in cui desideri ripristinare il backup.

5. **Avvia il processo di ripristino**: Clonezilla inizierà il processo di ripristino. Come per il backup, il tempo richiesto dipenderà dalle dimensioni dell'unità e dalla velocità dell'hardware.

---

## Note finali

I backup del disco con Clonezilla garantiscono che l'intero sistema (sistema operativo, impostazioni e applicazioni) venga preservato. Con uno sforzo minimo, puoi proteggere il tuo sistema da guasti catastrofici e ridurre al minimo i tempi di inattività in caso di arresto anomalo.

Ricorda, **i backup sono essenziali**. Aggiorna regolarmente i tuoi backup e testali periodicamente per assicurarti di poter ripristinare il sistema quando necessario.

Dopo l'avvio, puoi collegare l'unità di backup esterna e ispezionarne la struttura delle partizioni utilizzando l'utilità Dischi in Linux. L'unità di backup dovrebbe rispecchiare la struttura dell'unità interna, con le stesse partizioni e un po' di spazio inutilizzato se l'unità esterna è più grande.