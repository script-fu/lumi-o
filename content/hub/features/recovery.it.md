---
title: "Recupero file"
type: docs
url: "hub/features/recovery"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 59495d24302cb3493b90bc61a6dd1ffb9bb9c30b179f7be388882fe4f45a5075
---

Il sistema di recupero di Lumi è progettato per proteggere il lavoro di pittura da arresti anomali, errori e sessioni interrotte. Offre ai progetti una rete di sicurezza senza costringere gli artisti a duplicare costantemente i file a mano.

Il recupero si basa su due idee: protezione automatica in background e checkpoint intenzionali. Insieme aiutano a preservare il lavoro recente consentendo comunque all'artista di tornare a momenti precedenti del progetto.

![recover](/images/screens/recover.jpg)

## Protezione automatica

Mentre un'immagine viene modificata, Lumi può mantenere i dati di recupero separati dal file di lavoro principale. Il progetto non deve quindi essere riscritto ogni volta che viene creata un'istantanea di sicurezza.

Se qualcosa va storto, lo stato di recupero automatico può fornire una versione recente dell'opera che potrebbe essere più aggiornata dell'ultimo salvataggio intenzionale. L'obiettivo è semplice: ridurre la quantità di lavoro perso quando una sessione termina inaspettatamente.

## Checkpoint intenzionali

Alcuni momenti di un dipinto meritano di essere preservati deliberatamente: prima di un importante cambiamento di colore, dopo uno schizzo riuscito, prima di decisioni di appiattimento o quando si prova una direzione rischiosa.

Lumi supporta checkpoint a livello di progetto per questi momenti. Sono più leggeri che conservare una copia completa separata per ogni esperimento, ma offrono comunque all'artista un modo per tornare a punti significativi nella storia dell'opera.

## Recupero contestualizzato

Gli stati di recupero vengono presentati come versioni dell'opera, non come file grezzi da cercare manualmente. L'artista può confrontare i recenti salvataggi automatici e i checkpoint deliberati, poi aprire lo stato che meglio corrisponde al lavoro da cui desidera proseguire.

Le immagini recuperate si aprono come documenti di lavoro, consentendo all'artista di esaminarle prima di decidere come salvarle o continuare.

## Mantenere il recupero pratico

Un sistema di recupero utile deve anche restare gestibile. Lumi è progettato per mantenere organizzati i dati di recupero e rendere rimovibili gli stati vecchi quando non servono più.

Così la sicurezza non diventa disordine. Il recupero può restare attivo in background, mentre gli artisti conservano il controllo su quanta cronologia viene mantenuta nel tempo.

## Sicurezza durante il lavoro

Lo scopo del recupero file non è sostituire il salvataggio, ma rendere il lavoro creativo meno fragile. Gli artisti possono dipingere, sperimentare e correre rischi sapendo che Lumi mantiene ulteriori vie di ritorno quando una sessione, un file o una decisione va storta.
