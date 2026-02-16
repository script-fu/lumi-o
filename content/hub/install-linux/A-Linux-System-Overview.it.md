---
title: "Una panoramica del sistema Linux"
type: docs
url: "hub/install-linux/A-Linux-System-Overview"
---
Linux è un sistema operativo potente e versatile con una vasta comunità di sviluppatori. Fondamentalmente, un sistema Linux è costituito da diversi componenti chiave che lavorano insieme per fornire un'esperienza utente fluida. Questa panoramica delineerà le parti essenziali di un sistema Linux, inclusi kernel, distribuzione, gestore pacchetti, display manager, ambiente desktop e server di visualizzazione (X11 o Wayland).

**Esempi di configurazioni di distribuzione Linux**

| **Distribuzione** | **Gestore pacchetti** | **Gestione visualizzazioni** | **Ambiente desktop** | **Server di visualizzazione** |
|--------------------|---------------------|----------------------|-------------------------|--------------------|
| Debian | APT | GDM | GNOMO | X11 |
| Debian | APT | LuceDM | Cannella | X11 |
| Test Debian | APT | GDM | GNOMO | Wayland |
| Fedora | DNF | GDM | GNOMO | Wayland |
| ArcoLinux | Pacman | SDDM | KDE Plasma | X11 |

### Termini chiave

#### Kernel
Il nucleo del sistema operativo che si interfaccia direttamente con l'hardware, solitamente Linux.

#### Distribuzione
La distribuzione Linux, che impacchetta il kernel insieme a strumenti, librerie e software dello spazio utente. Gli esempi includono Debian, Arch Linux e Fedora.

#### Gestore pacchetti
Uno strumento utilizzato per installare, aggiornare e rimuovere applicazioni software dai repository. Gli esempi includono APT per distribuzioni basate su Debian, DNF per Fedora e Pacman per Arch Linux.

#### Gestione visualizzazioni
Gestisce la schermata di accesso grafica e l'avvio della sessione. Gli esempi includono GDM (GNOME Display Manager), LightDM e SDDM (Simple Desktop Display Manager).

#### Ambiente desktop
Fornisce l'interfaccia utente grafica (GUI) e gestisce l'aspetto generale e l'esperienza dell'utente. Gli esempi includono GNOME, Cinnamon e KDE Plasma.

#### Visualizza server
Gestisce gli eventi di output e input del display. Gli esempi includono X11 (X Window System) e Wayland. X11 è un server di visualizzazione tradizionale, mentre Wayland è un'alternativa più nuova e più sicura.