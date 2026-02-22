---
title: "Miscelazione dei colori spettrali"
type: docs
---
Il sistema di palette di Lumi utilizza un modello di colore spettrale per simulare il modo in cui i pigmenti reali si mescolano. L'obiettivo è far sì che l'esperienza di costruzione e selezione dei colori da una tavolozza digitale si comporti come la miscelazione di colori fisici. Una volta applicato un colore alla tela, è RGB standard.

## Cosa significa miscelazione spettrale

La miscelazione RGB tradizionale è additiva: la fusione di due valori RGB ne determina la media verso un punto medio. La miscelazione dei pigmenti è sottrattiva: ogni pigmento assorbe determinate lunghezze d'onda e il loro effetto combinato è più scuro e spesso cambia tonalità.

Lumi lo modella utilizzando una rappresentazione della riflettanza spettrale a 10 bande per i colori della tavolozza, anziché RGB.

Questo produce risultati simili alla pittura: mescolando blu e giallo si ottiene il verde, non il grigio. Mescolando due colori saturi si produce un colore che vira verso il neutro come fanno i pigmenti fisici.

Il calcolo spettrale viene eseguito durante la costruzione della tavolozza, quando si generano voci della tavolozza secondaria e terziaria e quando il Miscelatore tavolozza fonde due colori principali. Il colore risultante viene convertito in RGB lineare per la visualizzazione e la verniciatura.

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

### Arance e gialli| Campione | Nome | Codice CI | Famiglia |
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px; background:rgb(245,135,20);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Arancio pirrolo | PO73 | Rosso (scarlatto) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px; background:rgb(243,114,64);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Arancio Cadmio | PO20| Giallo (corpo) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px; background:rgb(240,180,80);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Giallo Cadmio | PY35 | Giallo (corpo) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px; background:rgb(245,210,25);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Giallo di cadmio pallido | PY35:Pallido | Giallo (cadmio pallido) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px; background:rgb(250,230,5);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Giallo Limone | PY3 | Giallo (Limone) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px; background:rgb(225,155,10);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Nichel Azo Giallo | PY150 | Giallo (medio) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px; background:rgb(180,175,45);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Oro Verde | PY129 | Giallo-Verde (Oro) |

### I colori della terra

| Campione | Nome | Codice CI | Famiglia |
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px; background:rgb(200,100,70);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Terra di Siena bruciata | PBr7:Bruciato | Terra (Rosso Marrone) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px; background:rgb(117,66,0);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Terra d'ombra bruciata | PBr7:Umber | Terra (neutro) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px; background:rgb(205,68,35);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Terra di Siena cruda | PBr7:Grezzo | Terra (Giallo Marrone) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px; background:rgb(187,124,25);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Ocra gialla | PY42 | Terra (giallo) |

### Verdi

| Campione | Nome | Codice CI | Famiglia |
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px; background:rgb(0,166,81);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Verde ftalo (YS) | PG36 | Verde (ftalo giallo-ombra) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px; background:rgb(64,130,109);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Viridiano | PG18 | Verde (Viridiano) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px; background:rgb(128,138,112);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Terre Verte | PG23 | Verde (Terra Fresca) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px; background:rgb(0,110,100);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Winsor Verde (BS) | PG7 | Verde (phthalo Blue-Shade) |### Blues e ciano

| Campione | Nome | Codice CI | Famiglia |
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px; background:rgb(0,177,176);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Turchese cobalto chiaro | PG50| Ciano (minerale) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px; background:rgb(0,148,214);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Blu ceruleo | PB35 | Ciano (minerale) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px; background:rgb(0,100,110);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Turchese ftalo | PB16 | Blu (ftalo) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px; background:rgb(0,123,194);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Blu cobalto | PB28 | Blu (viola-magro) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px; background:rgb(0,75,115);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Winsor Blu | PB15 | Blu (ftalo) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px; background:rgb(27,63,148);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Oltremare | PB29 | Blu (viola-magro) |

### Viola, magenta e rossi

| Campione | Nome | Codice CI | Famiglia |
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px; background:rgb(124,65,153);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Viola brillante | PV23 | Viola (diossazina) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px; background:rgb(230,90,180);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Rosa permanente | PV19:Rosa | Magenta (chinacridone) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px; background:rgb(190,40,120);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Chinacridone Magenta | PV19:Magenta | Magenta (chinacridone) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px; background:rgb(160,30,65);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Alizarina cremisi permanente | PV19:Cremisi | Magenta (chinacridone) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px; background:rgb(120,35,65);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Viola di perilene | PV29 | Magenta (chinacridone) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px; background:rgb(135,10,45);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Perilene Marrone | PR179 | Rosso (cremisi) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px; background:rgb(215,30,60);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Rosso pirrolo | PR254 | Rosso (scarlatto) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px; background:rgb(225,55,65);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Luce rossa pirrolo | PR255 | Rosso (luce pirrolica) |

### Neri e bianchi| Campione | Nome | Codice CI | Famiglia |
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px; background:rgb(22,15,10);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Marte Nero (Caldo) | PBk11 | Nero (Marte) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px; background:rgb(18,28,12);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Verde perilene | PBk31 | Nero (verde perilene) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px; background:rgb(10,18,19);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Avorio Nero (Freddo) | PBk9 | Nero (Avorio) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px; background:rgb(18,18,18);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Lampada Nera (Neutro) | PBK7 | Nero (Lampada) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px; background:rgb(255,249,235);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Bianco titanio (caldo) | PW6:Caldo | Bianco (Titanio Caldo) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px; background:rgb(255,255,255);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Bianco titanio (neutro) | PW6 | Bianco (Titanio Neutro) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px; background:rgb(245,250,255);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Bianco zinco (freddo) | PW4 | Bianco (zinco freddo) |

### Controlla i grigi

I grigi di controllo sono neutralizzatori standardizzati utilizzati per desaturare in modo prevedibile le miscele.

| Campione | Nome | Codice CI |
| :---: | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px; background:rgb(135,128,120);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Grigio caldo | N_CALDO |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px; background:rgb(128,128,128);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Grigio neutro | N_NEUTRO |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px; background:rgb(120,128,135);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Grigio freddo | N_COOL |

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

## I pixel della tela sono RGBIl sistema spettrale opera interamente nell'ambito della costruzione della tavolozza e della selezione del colore. Quando viene applicato un tratto di pennello, il colore di primo piano, già convertito in RGB lineare, è ciò che viene dipinto. La tela memorizza i dati dei pixel RGB standard.

La miscelazione spettrale migliora l'esperienza di creazione di una tavolozza e di scelta dei colori in modo coerente con il comportamento fisico dei pigmenti, senza modificare il modo in cui i dati dell'immagine vengono archiviati o compositi.