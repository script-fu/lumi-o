---
title: "Panoramica del sistema Linux"
type: docs
url: "hub/install-linux/A-Linux-System-Overview"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 57573497133d7364dcc0acf023b2cb3b098b2a931ed245e846a539b96adb78b0
---

Linux è un sistema operativo potente e versatile, con una vasta comunità di sviluppatori. Fondamentalmente, un sistema Linux è costituito da diversi componenti chiave che lavorano insieme per offrire un'esperienza utente fluida. Questa panoramica descrive le parti essenziali di un sistema Linux: kernel, distribuzione, gestore di pacchetti, display manager, ambiente desktop e server grafico (X11 o Wayland).

Lumi dà il meglio di sé su Debian con Cinnamon (X11) ed è sviluppato e testato in quell'ambiente.

**Impostazioni predefinite comuni nelle distribuzioni Linux attuali**

| **Distribuzione**   | **Gestore di pacchetti** | **Display manager** | **Ambiente desktop** | **Server grafico** |
|---------------------|--------------------------|-----------------------|----------------------|--------------------|
| Debian              | APT                      | GDM                   | GNOME                | Wayland            |
| Ubuntu              | APT                      | GDM                   | GNOME                | Wayland            |
| Debian              | APT                      | GDM                   | Cinnamon             | X11                |
| Fedora              | DNF                      | GDM                   | GNOME                | Wayland            |
| Arch Linux          | Pacman                   | Scelta dell'utente    | Scelta dell'utente   | Scelta dell'utente |

### Termini chiave

#### Kernel

Il nucleo del sistema operativo che interagisce direttamente con l'hardware; di solito Linux.

#### Distribuzione

La distribuzione Linux impacchetta il kernel insieme a strumenti, librerie e software dello spazio utente. Tra gli esempi ci sono Debian, Arch Linux e Fedora.

#### Gestore di pacchetti

Strumento usato per installare, aggiornare e rimuovere applicazioni dai repository. Tra gli esempi ci sono APT per le distribuzioni basate su Debian, DNF per Fedora e Pacman per Arch Linux.

#### Display manager

Gestisce la schermata di accesso grafica e l'avvio della sessione. Tra gli esempi ci sono GDM (GNOME Display Manager), LightDM e SDDM (Simple Desktop Display Manager).

#### Ambiente desktop

Fornisce l'interfaccia utente grafica (GUI) e gestisce l'aspetto generale e l'esperienza d'uso. Tra gli esempi ci sono GNOME, Cinnamon e KDE Plasma.

#### Server grafico

Gestisce l'output del display e gli eventi di input. Tra gli esempi ci sono X11 (X Window System) e Wayland. X11 è un server grafico tradizionale, mentre Wayland è un'alternativa più recente e sicura.
