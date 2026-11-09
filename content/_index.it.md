---
title: "Pittura digitale open source per Linux"
toc: false
type: docs
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 4f453bf4100140e9a2779d4b9806a0ef8f27e68af8a9b6f680d01a6a5d2f2341
---
[![gui-01](/images/screens/gui-00.jpg)](/images/screens/gui-01.jpg)

Lumi-o è pensato per la pittura raster a strati: un software volutamente leggero e stabile. Il supporto per le tavolette Wacom offre l'input fluido e reattivo richiesto dall'illustrazione professionale.

Il sistema di colori di Lumi, basato sulle tavolozze, mescola spettralmente profili di pigmenti del mondo reale. Costruisci tavolozze a partire da relazioni cromatiche visualizzate, controlla i valori tonali e organizza i colori come voci con nome. Palette Map e Mixer ti aiutano a mantenere coerenti le scelte cromatiche in un dipinto o in un progetto.

Basato sull'architettura centrale di GIMP, Lumi-o è progettato e perfezionato da un artista. I suoi strumenti restano accessibili e offrono al contempo la personalizzazione necessaria per flussi di lavoro avanzati. Caricamento e salvataggio rapidi, insieme a un sistema di salvataggio automatico completamente integrato, proteggono il tuo lavoro senza interrompere il flusso creativo.

I tratti del pennello possono prendere una texture di superficie, con la grana e il dente di un supporto, compresi i tratti calligrafici. Un tratto nuovo, o una svolta nel tratto, può spostare quella grana, così l'ombreggiatura avanti e indietro si costruisce su una fase nuova a ogni inversione. I pennelli a setole si possono modellare nel dettaglio, e le setole possono spostarsi e rinnovarsi lungo il tratto. OpenGL disegna la pila dei livelli nella finestra dell'immagine e può dipingere i tratti di singole preimpostazioni dello strumento. Gli aerografi grandi e i timbri di texture grandi sono molto più rapidi, e la navigazione sulla tela resta fluida.

Il caricamento differito consente a Lumi di aprire progetti grandi e complessi utilizzando molta meno RAM. Quando un gruppo è compresso, Lumi carica l'immagine composita salvata, mentre i suoi livelli, le maschere e i gruppi annidati restano segnaposto leggeri finché il gruppo non viene espanso di nuovo.

Lumi-o è sviluppato su Debian 13 con Cinnamon su X11. GNOME su Wayland è stato provato per l'aspetto generale dell'applicazione e per il rendering OpenGL. Se provieni da Windows, installare Debian è il primo passo. Consulta [Come installare Linux](/hub/install-linux/).

{{< cards >}}
  {{< card link="hub/about/" title="Informazioni su Lumi-o" icon="information-circle" >}}
  {{< card link="hub/quick-start/" title="Avvio rapido" icon="play" >}}
  {{< card link="hub/features/" title="Funzionalità" icon="book-open" >}}
{{< /cards >}}
