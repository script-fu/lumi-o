---
title: "Berichtenbibliotheek"
type: docs
weight: 6
---
Over time, what began as a single function to send messages has evolved into a collection of related functions. Deze functies vormen nu de basis van een **Berichtenbibliotheek**, ontworpen om uitvoer naar verschillende bestemmingen te verwerken, zoals de GUI, Berichtenconsole en OS-terminal.

### Waarom een ​​berichtenbibliotheek?

As our needs grow, handling messages across multiple outputs requires a more modular and extensible approach. Instead of a single function doing everything, we’ve broken the process down into reusable components, allowing for greater flexibility. This library can now be used as a general-purpose messaging tool that other plug-ins or functions can borrow from.

### Wat doet de berichtenbibliotheek?

The Messaging Library currently includes the following functions:

- **send-to-gui**: Sends messages to the Lumi GUI dialog box.
- **send-to-error-console**: Sends messages to the Lumi Message console.
- **send-to-terminal**: Sends messages to the terminal window.
- **send-message**: A dispatcher function that directs messages to the appropriate output.
- **validate-message**: Ensures that the message and output are valid before sending.

### De bibliotheek uitbreiden

The **Messaging Library** can easily be extended to support additional outputs. Bijvoorbeeld:

- **verzenden naar bestand**: berichten opslaan in een logbestand.
- **send-to-logger**: Integrate with an external logging system.
- **send-to-notification**: Display messages as system notifications.

Door hetzelfde patroon van modulair ontwerp en herbruikbare functies te volgen, kan deze bibliotheek uitgroeien tot een alomvattend hulpmiddel voor het verwerken van allerlei soorten berichttaken.

## Voordelen van een berichtenbibliotheek

- **Reusability**: The functions can be reused across different plug-ins or projects.
- **Modularity**: Each function handles one specific task, making the code easier to maintain and extend.
- **Consistency**: Using the same validation and message-handling functions ensures consistent behavior across the application.

The **Messaging Library** is the beginning of a broader framework that could simplify how messages are managed in your project. As the library grows, new plug-ins can easily tap into it to send messages wherever they need to go.

We kunnen de bestandsstructuur aanpassen:

```plaintext
/home/your-username/code/
  ├── script-fu/
      ├── library/
      │     └── send-message.scm -> messaging.scm
      └── plug-ins/
            └── hello-world/
                  └── hello-world.scm
```

And remember to adjust the `load` in the main plug-in:

```scheme
# !/usr/bin/env lumi-scheme-interpreter-0.1

(load "/home/mark/code/github/script-plugins/funky-library/messaging.scm")

(define (scheme-hello-world)
  (let ((message "Hello world!\n"))
    (send-message message 'gui)
    (send-message message 'error-console)
    (send-message message 'terminal)))

(scheme-register-procedure "scheme-hello-world"
  "Hello world!"
  "A Scheme procedure plug-in refactored"
  "Mark Sweeney"
  "Under GNU GENERAL PUBLIC LICENSE Version 3"
  "2024")

(scheme-menu-register
  "scheme-hello-world"
  "<Image>/Funky")
```