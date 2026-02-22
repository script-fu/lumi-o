---
title: "Navigatore di utilità"
type: docs
---
Il browser delle utilità ti consente di esplorare l'utilità Scheme integrata stdlib fornita con Lumi, senza dover uscire dall'app o scavare nei file sorgente.

## Apertura del browser delle utilità

Vai a **Aiuto → Programmazione → Browser delle utilità**.

La finestra si apre immediatamente; non è necessario caricare alcun plug-in in anticipo.

## Cosa mostra

Il browser elenca ogni procedura, variabile e forma di sintassi esportata dalle sette librerie di utilità che Lumi carica automaticamente all'avvio:

| Biblioteca | Cosa copre |
|---|---|
| `common.scm` | Aiutanti di uso generale (stringa, numero, utilità di elenco) |
| `files.scm` | Helper di file e percorsi |
| `gegl.scm` | Buffer GEGL e aiutanti del colore |
| `images.scm` | Aiutanti a livello di immagine (`image-get-open-list`, ecc.) |
| `layers.scm` | Aiutanti di livello e disegnabili |
| `parasites.scm` | Aiutanti di lettura/scrittura del parassita |
| `paths.scm` | Aiutanti di percorsi e vettori |

Tutti questi sono disponibili in qualsiasi plug-in di Scheme o nella Console di Scheme.

## Ricerca e filtraggio

- **Casella di ricerca**: filtra per nome durante la digitazione (corrispondenza della sottostringa senza distinzione tra maiuscole e minuscole).
- **Filtro tipo**: restringe i risultati a `procedure`, `variable` o `syntax`.

Facendo clic su una voce viene mostrata la sua docstring completa e la libreria da cui proviene.

## Lo Stdlib come wrapper

Le librerie di utilità sono un'applicazione pratica del modello di wrapper: ogni helper dà un nome chiaro a un'operazione di basso livello, nasconde il boilerplate e fornisce un unico posto in cui aggiornare se il comando sottostante cambia. Se vuoi comprendere l'approccio progettuale alla base di questi, guarda il tutorial **[Wrapping](@@LUMI_TOKEN_11@@)**.

## Relazione con il browser delle procedure

Il browser delle utilità è separato da **Filtri → Script-Fu → Console → Sfoglia** (il browser delle procedure). Il Browser delle procedure elenca le procedure registrate nel PDB. L'Utility Browser elenca le definizioni helper che vivono intenzionalmente *all'esterno* del PDB: sono solo Scheme e non hanno associazione C.