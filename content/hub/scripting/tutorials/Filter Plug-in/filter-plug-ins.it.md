---
title: "Il plugin filtro"
type: docs
weight: 2
---
We used a _procedure_ plug-in for the [First Step](../../first-step/) tutorial. Those types of plug-ins work without needing an image or drawable as input. Usually, we use a plug-in to change an image and its drawables. Plug-ins like these are called _filter_ plug-ins.

### Cos'è un Drawable?

A **drawable** in Lumi refers to an image element that can be drawn on, such as a layer or channel. Filter plug-ins typically operate on these elements.

### Un semplice esempio di plug-in di filtro

```scheme
# !/usr/bin/env lumi-scheme-interpreter-0.1

(define (scheme-simple-filter-plug-in image drawables)
  ;; Use a let statement to define a message variable and core code
  (let ((message "hello, world"))
    ;; Display the message in Lumi's error console
    (lumi-message message)
    ;; Invert the colors of the first selected drawable
    (lumi-drawable-invert (vector-ref drawables 0) 1)))

;; Register the plug-in
(scheme-register-filter
  "scheme-simple-filter-plug-in"           ;; Main procedure name
  "Simple Filter Plug-in Demo"             ;; The name as it appears in the Lumi menu
  "Tests a basic Scheme filter plug-in"    ;; Tool-tip description
  "Author Name"                            ;; Give yourself some credit
  "License"                                ;; License
  "Date written"                           ;; Date written
  "*"                                      ;; Indicates this plug-in requires an image
  SF-ONE-OR-MORE-DRAWABLE)                 ;; Requires one or more selected drawables

;; Specify the menu location for the plug-in
(scheme-menu-register
  "scheme-simple-filter-plug-in"
  "<Image>/Plug-in")
```

Copy the text and save it as `simple-filter-plug-in.scm` in a folder called `simple-filter-plug-in` within one of Lumi's plug-ins folders. A Lumi plug-ins folder is _any_ folder listed under:
 **Lumi > Edit > Preferences > Folders > Plug-ins**

In Linux, right-click the `simple-filter-plug-in.scm` file, go to **Properties > Permissions**, and check **Allow executing file as program**. Una volta che il file è nel posto giusto, eseguibile e privo di errori di sintassi, al riavvio di Lumi apparirà nella barra di intestazione del menu in alto, all'interno di un menu chiamato **Plug-in**.

### Esecuzione del plug-in

1. Open an image (this filter plug-in requires an image to work).
2. Open **Tools > Debug > Message console** to see a message.
3. Select **Simple Filter Plug-in Demo** from the **Plug-in** menu.
4. One of the selected layers will have its colors inverted and a message will be printed to the error console.

### Modifica del plug-in

You can customize the plug-in by editing its `.scm` file. Ad esempio, per modificare il messaggio visualizzato:

1. Open the file and locate the line defining `message`.
2. Replace `"hello, world"` with your custom text.
3. Salvare il file.

In Lumi version 3, plug-ins do not need refreshing for saved changes to take effect. Simply re-run the plug-in to see the updated message.

### Esame dei plug-in

#### Shebang Line

The first line ensures the script works as a plug-in in Lumi 3:

```scheme
# !/usr/bin/env lumi-scheme-interpreter-0.1

```

#### Definizione della procedura

The procedure accepts two arguments: the active image and the selected drawables.

```scheme
(define (scheme-simple-filter-plug-in image drawables)
```

#### Logica fondamentale

A `let` statement defines a variable and performs operations on the drawable.

```scheme
(let ((message "hello, world"))
  (lumi-message message) ;; Displays a message in Lumi's error console
  (lumi-drawable-invert (vector-ref drawables 0) 1)) ;; Inverts the colors of the first selected drawable
```

### Registrazione del plug-in

The plug-in is registered with Lumi as a filter plug-in:

```scheme
(scheme-register-filter
  "scheme-simple-filter-plug-in"           ;; Register the main procedure
  "Simple Filter Plug-in Demo"             ;; The name as it appears in the Lumi menu
  "Tests a basic Scheme filter plug-in"    ;; Tool-tip description
  "Author Name"                            ;; Author's name
  "License"                                ;; License type
  "Date written"                           ;; Date written
  "*"                                      ;; Indicates the plug-in requires an image
  SF-ONE-OR-MORE-DRAWABLE)                 ;; Requires one or more selected drawables
```

#### Registrazione nel menu

This line specifies the menu location for the plug-in:

```scheme
(scheme-menu-register
  "scheme-simple-filter-plug-in"
  "<Image>/Plug-in")
```

### Risoluzione dei problemi

If a plug-in does not appear, check its location, name, and executable property.

La posizione deve trovarsi in un percorso di ricerca del plug-in.
The file name must match the name of the containing folder.
Il file deve essere impostato come eseguibile.


The **Message console** is a valuable tool for troubleshooting custom plug-ins. If your plug-in doesn't behave as expected, check here for error messages or logs. The **Terminal** window can also provide debugging information and report loading issues.