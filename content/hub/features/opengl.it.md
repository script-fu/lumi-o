---
title: "OpenGL"
type: docs
url: "hub/features/opengl"
translation_provenance: ai-reviewed
translation_source_sha256: af2a57a988eced14801b7c4673253fe79e9a6b6ab4fcf53a075b62d4a9035351
translation_lock: true
---

Lumi può disegnare la pila dei livelli con OpenGL e dipingere i tratti del pennello allo stesso modo. La navigazione sulla tela resta fluida. Gli aerografi grandi e i timbri di texture grandi sono molto più rapidi. Se un dato strumento dipinge così, la scelta resta memorizzata nella sua preimpostazione.

## La pila dei livelli

La finestra dell'immagine mostra la pila dei livelli composta. Con OpenGL quell'immagine è mantenuta in tessere sul processore grafico, già pronta per lo schermo. Spostamento, zoom, rotazione e ribaltamento cambiano la vista di quelle tessere. Su una tela grande i gesti restano fluidi, perché la navigazione è un cambio di vista su un'immagine che è già lì.

Le modifiche aggiornano le tessere che toccano. Le guide, il cursore del pennello e il contorno della selezione sono disegnati sopra quell'immagine.

Finché la penna è abbassata, un tratto che usa OpenGL può comparire al suo posto tra i livelli che lo circondano, compresi i livelli sopra quello che si sta dipingendo. Il segno viene scritto nel livello quando la penna si solleva.

## Tratti per preimpostazione dello strumento

Dipingere con OpenGL è una scelta della preimpostazione dello strumento, accanto al pennello e alle dinamiche. Una preimpostazione può dipingere sul processore grafico. Un'altra può tenere il percorso di tratto ordinario. Tornare a uno strumento riporta con sé quella scelta.

Il guadagno è massimo dove i timbri sono grandi. Sul percorso ordinario un aerografo largo resta indietro rispetto allo stilo man mano che il segno cresce: i tocchi si mettono in coda e il tratto apre dei vuoti, anche mentre la penna resta abbassata e il pennello continua a emettere. Con OpenGL quell'aerografo continua a posare i segni al ritmo della mano.

Un timbro di texture grande mostra lo stesso cambiamento. Il percorso ordinario impiega più tempo su ogni tocco man mano che il timbro cresce, finché un tratto si inceppa. Il tratto OpenGL segue il gesto. I timbri che portano una grana di superficie si possono disegnare con OpenGL allo stesso modo.

Anche i segni delle setole si possono disegnare così. Un timbro di setole a punta tonda si può dipingere con OpenGL mentre le setole cambiano lungo il tratto. Un tratto calligrafico si può disegnare come un unico nastro continuo, e la texture di superficie può mascherare quel nastro. La [texture di superficie](/hub/features/paintbrush/#texture-di-superficie) e le [setole lungo il tratto](/hub/features/paintbrush/#setole-lungo-il-tratto) appartengono al pennello stesso.

Alcuni tratti restano sul percorso ordinario. La miscelazione spettrale, un filtro sul livello dipinto e alcuni metodi di fusione sono tra questi. Quei segni vengono comunque dipinti.

## Quando resta la tela ordinaria

OpenGL si usa quando l'hardware grafico può eseguirlo e l'immagine entra nella memoria riservata alla tela. Altrimenti Lumi disegna la finestra come prima. L'AppImage pubblicato porta con sé le proprie librerie grafiche. Su un driver che quelle librerie non possono usare, incluso il driver proprietario di NVIDIA, la tela resta su quel percorso ordinario.

## Cinnamon su X11 e GNOME su Wayland

Lumi è sviluppato su Debian con Cinnamon e X11. Quello è ancora l'ambiente di sviluppo. GNOME su Wayland è stato provato per l'aspetto generale dell'applicazione e per questo rendering OpenGL. Anche lì la navigazione sulla tela è fluida.
