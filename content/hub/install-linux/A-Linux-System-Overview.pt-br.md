---
title: "Visão geral do sistema Linux"
type: docs
url: "hub/install-linux/A-Linux-System-Overview"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: a38745c31470f2662ed8b220af2cc2065e981b8c1435fce2efae41886a2a7996
---

Linux é um sistema operacional poderoso e versátil, com uma vasta comunidade de desenvolvedores. Em essência, um sistema Linux consiste em vários componentes principais que trabalham juntos para oferecer uma experiência de usuário fluida. Esta visão geral descreve as partes essenciais de um sistema Linux: kernel, distribuição, gerenciador de pacotes, gerenciador de exibição, ambiente de desktop e servidor de exibição (X11 ou Wayland).

O Lumi é desenvolvido e testado no Debian com Cinnamon (X11). Esse continua sendo o ambiente de desenvolvimento. O GNOME no Wayland foi testado junto com ele, quanto à aparência geral do aplicativo e à renderização OpenGL.

**Padrões comuns atuais das distribuições Linux**

| **Distribuição** | **Gerenciador de pacotes** | **Display Manager** | **Ambiente de desktop** | **Servidor de exibição** |
|--------------------|----------------------|----------------------|-------------------------|--------------------|
| Debian             | APT                  | GDM                  | GNOME                   | Wayland            |
| Ubuntu             | APT                  | GDM                  | GNOME                   | Wayland            |
| Debian             | APT                  | GDM                  | Cinnamon                | X11                |
| Fedora             | DNF                  | GDM                  | GNOME                   | Wayland            |
| Arch Linux         | Pacman               | Escolha do usuário   | Escolha do usuário      | Escolha do usuário |

### Termos-chave

#### Kernel

O núcleo do sistema operacional que se comunica diretamente com o hardware — em geral, Linux.

#### Distribuição

A distribuição Linux empacota o kernel junto com ferramentas, bibliotecas e software de espaço de usuário. Exemplos: Debian, Arch Linux e Fedora.

#### Gerenciador de pacotes

Ferramenta usada para instalar, atualizar e remover aplicativos a partir de repositórios. Exemplos: APT para distribuições baseadas em Debian, DNF para Fedora e Pacman para Arch Linux.

#### Gerenciador de exibição

Gerencia a tela gráfica de login e o início da sessão. Exemplos: GDM (GNOME Display Manager), LightDM e SDDM (Simple Desktop Display Manager).

#### Ambiente de desktop

Fornece a interface gráfica do usuário (GUI) e gerencia a aparência geral e a experiência do usuário. Exemplos: GNOME, Cinnamon e KDE Plasma.

#### Servidor de exibição

Gerencia a saída de vídeo e os eventos de entrada. Exemplos: X11 (X Window System) e Wayland. O X11 é um servidor de exibição tradicional; o Wayland é uma alternativa mais recente e segura.
