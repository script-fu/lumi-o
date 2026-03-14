---
title: "Miscelazione dei colori spettrali"
type: docs
---
Il sistema di palette di Lumi utilizza un modello di colore spettrale per simulare il modo in cui i pigmenti reali si mescolano. L'obiettivo è far sì che l'esperienza di costruzione e selezione dei colori da una tavolozza digitale si comporti come la miscelazione di colori fisici. Una volta applicato un colore alla tela, è RGB standard.

## Cosa significa miscelazione spettrale

La miscelazione RGB tradizionale è additiva: la fusione di due valori RGB ne determina la media verso un punto medio. La miscelazione dei pigmenti è sottrattiva: ogni pigmento assorbe determinate lunghezze d'onda e il loro effetto combinato è più scuro e spesso cambia tonalità.

Lumi lo modella utilizzando una rappresentazione della riflettanza spettrale a 10 bande per i colori della tavolozza, anziché RGB.

Questo produce risultati simili alla pittura: mescolando blu e giallo si ottiene il verde, non il grigio. Mescolando due colori saturi si produce un colore che vira verso il neutro come fanno i pigmenti fisici.

Il calcolo spettrale viene eseguito durante la costruzione della tavolozza, quando si generano voci della tavolozza secondaria e terziaria e quando il Miscelatore tavolozza mescola due colori principali. Il colore risultante viene convertito in RGB lineare per la visualizzazione e la verniciatura.

## Profili dei pigmenti

Le voci della tavolozza possono basarsi su dati di pigmenti reali utilizzando i codici **Colour Index (CI)**. Ciascuna famiglia di pigmenti CI ha una caratteristica polarizzazione spettrale che influenza il modo in cui si miscela.

| Ruolo del pigmento | Comportamento di miscelazione | Esempio |
| :--- | :--- | :--- |
| **Primario** | Croma elevato, secondari puliti | PY3 (giallo limone), PR122 (magenta) |
| **Corpo** | Opaco, tono di massa forte, vira all'olivastro nelle miscele verdi | PY35 (giallo cadmio), PR108 (rosso cadmio) |
| **Neutralizzante** | Desatura e disattiva rapidamente l'audio | PBk11 (Marte Nero), PBr7 (Siena) |
| **Ancoraggio cromatico** | Elevato potere colorante, domina le miscele | PB29 (Blu oltremare), PG7 (Verde ftalo) |

L'aggiunta di primari con codici CI a una tavolozza fornisce al motore di miscelazione una distorsione spettrale accurata per tali colori, in modo che i mix secondari e terziari generati riflettano il comportamento di miscelazione del mondo reale.

## Pigmenti Lumi

La tavolozza Master viene fornita con i seguenti pigmenti. I campioni mostrano l'aspetto tipico della massa di ciascun pigmento (piena resistenza, non diluito).

### Arance e gialli

| Campione | Nome | Codice CI | Famiglia |
| :---: | :--- | :--- | :--- |
| {{< swatch "245,135,20" >}} | Arancio pirrolo | PO73 | Rosso (scarlatto) |
| {{< swatch "243,114,64" >}} | Arancio Cadmio | PO20| Giallo (corpo) |
| {{< swatch "240,180,80" >}} | Giallo Cadmio | PY35 | Giallo (corpo) |
| {{< swatch "245,210,25" >}} | Giallo di cadmio pallido | PY35:Pallido | Giallo (cadmio pallido) |
| {{< swatch "250,230,5" >}} | Giallo Limone | PY3 | Giallo (Limone) |
| {{< swatch "225,155,10" >}} | Nichel Azo Giallo | PY150 | Giallo (medio) |
| {{< swatch "180,175,45" >}} | Oro Verde | PY129 | Giallo-Verde (Oro) |

### I colori della terra

| Campione | Nome | Codice CI | Famiglia |
| :---: | :--- | :--- | :--- |
| {{< swatch "200,100,70" >}} | Terra di Siena bruciata | PBr7:Bruciato | Terra (Rosso Marrone) |
| {{< swatch "117,66,0" >}} | Terra d'ombra bruciata | PBr7:Umber | Terra (neutro) |
| {{< swatch "205,68,35" >}} | Terra di Siena cruda | PBr7:Grezzo | Terra (Giallo Marrone) |
| {{< swatch "187,124,25" >}} | Ocra gialla | PY42 | Terra (giallo) |

### Verdi

| Campione | Nome | Codice CI | Famiglia |
| :---: | :--- | :--- | :--- |
| {{< swatch "0,166,81" >}} | Verde ftalo (YS) | PG36 | Verde (ftalo giallo-ombra) |
| {{< swatch "64,130,109" >}} | Viridiano | PG18 | Verde (Viridiano) |
| {{< swatch "128,138,112" >}} | Terre Verte | PG23 | Verde (Terra Fresca) |
| {{< swatch "0,110,100" >}} | Winsor Verde (BS) | PG7 | Verde (phthalo Blue-Shade) |

### Blues e ciano

| Campione | Nome | Codice CI | Famiglia |
| :---: | :--- | :--- | :--- |
| {{< swatch "0,177,176" >}} | Turchese cobalto chiaro | PG50| Ciano (minerale) |
| {{< swatch "0,148,214" >}} | Blu ceruleo | PB35 | Ciano (minerale) |
| {{< swatch "0,100,110" >}} | Turchese ftalo | PB16 | Blu (ftalo) |
| {{< swatch "0,123,194" >}} | Blu cobalto | PB28 | Blu (viola-magro) |
| {{< swatch "0,75,115" >}} | Winsor Blu | PB15 | Blu (ftalo) |
| {{< swatch "27,63,148" >}} | Oltremare | PB29 | Blu (viola-magro) |

### Viola, magenta e rossi

| Campione | Nome | Codice CI | Famiglia |
| :---: | :--- | :--- | :--- |
| {{< swatch "124,65,153" >}} | Viola brillante | PV23 | Viola (diossazina) |
| {{< swatch "230,90,180" >}} | Rosa permanente | PV19:Rosa | Magenta (chinacridone) |
| {{< swatch "190,40,120" >}} | Chinacridone Magenta | PV19:Magenta | Magenta (chinacridone) |
| {{< swatch "160,30,65" >}} | Alizarina cremisi permanente | PV19:Cremisi | Magenta (chinacridone) |
| {{< swatch "120,35,65" >}} | Viola di perilene | PV29 | Magenta (chinacridone) |
| {{< swatch "135,10,45" >}} | Perilene Marrone | PR179 | Rosso (cremisi) |
| {{< swatch "215,30,60" >}} | Rosso pirrolo | PR254 | Rosso (scarlatto) |
| {{< swatch "225,55,65" >}} | Luce rossa pirrolo | PR255 | Rosso (luce pirrolica) |

### Neri e bianchi

| Campione | Nome | Codice CI | Famiglia |
| :---: | :--- | :--- | :--- |
| {{< swatch "22,15,10" >}} | Marte Nero (Caldo) | PBk11 | Nero (Marte) |
| {{< swatch "18,28,12" >}} | Verde perilene | PBk31 | Nero (verde perilene) |
| {{< swatch "10,18,19" >}} | Avorio Nero (Freddo) | PBk9 | Nero (Avorio) |
| {{< swatch "18,18,18" >}} | Lampada Nera (Neutro) | PBK7 | Nero (Lampada) |
| {{< swatch "255,249,235" >}} | Bianco titanio (caldo) | PW6:Caldo | Bianco (Titanio Caldo) |
| {{< swatch "255,255,255" >}} | Bianco titanio (neutro) | PW6 | Bianco (Titanio Neutro) |
| {{< swatch "245,250,255" >}} | Bianco zinco (freddo) | PW4 | Bianco (zinco freddo) |

### Controlla i grigi

I grigi di controllo sono neutralizzatori standardizzati utilizzati per desaturare in modo prevedibile le miscele.

| Campione | Nome | Codice CI |
| :---: | :--- | :--- |
| {{< swatch "135,128,120" >}} | Grigio caldo | N_CALDO |
| {{< swatch "128,128,128" >}} | Grigio neutro | N_NEUTRO |
| {{< swatch "120,128,135" >}} | Grigio freddo | N_COOL |

## La mappa della tavolozza

La Mappa tavolozza visualizza la tavolozza attiva come una ruota di tonalità: 36 settori di tonalità (incrementi di 10°) × 15 celle di luminosità. Quando vengono aggiunti i primari, il sistema genera mix secondari e terziari e li posiziona nelle posizioni appropriate sulla mappa.

Facendo clic su una cella si seleziona un colore come primo piano. Fare clic tenendo premuto Maiusc per assegnarlo come endpoint principale nel Mixer tavolozza.

## Il miscelatore di tavolozze

Il Mixer tavolozza ricava nuovi colori da due voci principali utilizzando una pipeline fissa a tre fasi:

1. **Miscela**: WGM spettrale tra Genitore A (CCW) e Genitore B (CW).
2. **Croma**: sfuma verso lo spettro neutro della tavolozza, riducendo la saturazione.
3. **Tonalità**: sfuma verso la miscelazione del bianco o la miscelazione del nero, regolando la luminosità.

Il tono viene applicato per ultimo in modo che le regolazioni della luminosità non vengano diluite dalle modifiche della crominanza. I controlli Blocco valore e Blocca banda vincolano i risultati a un livello di luminosità o una banda di valori specifici.

I colori misti possono essere salvati nella tavolozza come voci **Personalizzate**, memorizzando la ricetta completa (UID principali, fattore di fusione, tono, valori di crominanza) per un successivo recupero.

## I pixel della tela sono RGB

Il sistema spettrale opera interamente nell'ambito della costruzione della tavolozza e della selezione del colore. Quando viene applicato un tratto di pennello, il colore di primo piano (già convertito in RGB lineare) è ciò che viene dipinto. La tela memorizza i dati dei pixel RGB standard.La miscelazione spettrale migliora l'esperienza di creazione di una tavolozza e di scelta dei colori in modo coerente con il comportamento fisico dei pigmenti, senza modificare il modo in cui i dati dell'immagine vengono archiviati o compositi.