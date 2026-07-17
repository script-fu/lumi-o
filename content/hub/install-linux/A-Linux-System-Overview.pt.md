---
title: "Visão geral de um sistema Linux"
type: docs
url: "hub/install-linux/A-Linux-System-Overview"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 57573497133d7364dcc0acf023b2cb3b098b2a931ed245e846a539b96adb78b0
---
Linux é um sistema operativo poderoso e versátil, com uma vasta comunidade de programadores. No seu núcleo, um sistema Linux consiste em vários componentes principais que trabalham em conjunto para proporcionar uma experiência de utilizador fluida. Esta visão geral descreve as partes essenciais de um sistema Linux, incluindo o kernel, a distribuição, o gestor de pacotes, o gestor de ecrã, o ambiente gráfico e o servidor gráfico (X11 ou Wayland).

O Lumi funciona melhor no Debian com Cinnamon (X11) e é desenvolvido e testado nesse ambiente.

**Predefinições actuais comuns das distribuições Linux**

| **Distribuição** | **Gestor de pacotes** | **Gestor de ecrã** | **Ambiente gráfico** | **Servidor gráfico** |
|--------------------|----------------------|----------------------|-------------------------|--------------------|
| Debian | APT | GDM | GNOME | Wayland |
| Ubuntu | APT | GDM | GNOME | Wayland |
| Debian | APT | GDM | Cinnamon | X11 |
| Fedora | DNF | GDM | GNOME | Wayland |
| Arch Linux | Pacman | À escolha do utilizador | À escolha do utilizador | À escolha do utilizador |

### Termos-chave

#### Kernel

O núcleo do sistema operativo que comunica directamente com o hardware, normalmente Linux.

#### Distribuição

A distribuição Linux, que empacota o kernel juntamente com ferramentas, bibliotecas e software de espaço de utilizador. Exemplos incluem Debian, Arch Linux e Fedora.

#### Gestor de pacotes

Uma ferramenta usada para instalar, actualizar e remover aplicações a partir de repositórios. Exemplos incluem APT para distribuições baseadas em Debian, DNF para Fedora e Pacman para Arch Linux.

#### Gestor de ecrã

Gere o ecrã gráfico de início de sessão e o arranque da sessão. Exemplos incluem GDM (GNOME Display Manager), LightDM e SDDM (Simple Desktop Display Manager).

#### Ambiente gráfico

Fornece a interface gráfica (GUI) e gere a aparência geral e a experiência do utilizador. Exemplos incluem GNOME, Cinnamon e KDE Plasma.

#### Servidor gráfico

Gere a saída de vídeo e os eventos de entrada. Exemplos incluem X11 (X Window System) e Wayland. O X11 é um servidor gráfico tradicional, enquanto o Wayland é uma alternativa mais recente e segura.
