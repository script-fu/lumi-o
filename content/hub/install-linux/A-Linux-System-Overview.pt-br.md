---
title: "Uma visão geral do sistema Linux"
type: docs
url: "hub/install-linux/A-Linux-System-Overview"
---
Linux é um sistema operacional poderoso e versátil com uma vasta comunidade de desenvolvedores. Basicamente, um sistema Linux consiste em vários componentes principais que trabalham juntos para fornecer uma experiência de usuário perfeita. Esta visão geral descreverá as partes essenciais de um sistema Linux, incluindo o kernel, distribuição, gerenciador de pacotes, gerenciador de exibição, ambiente de desktop e servidor de exibição (X11 ou Wayland).

**Exemplos de configurações de distribuição Linux**

| **Distribuição** | **Gerenciador de Pacotes** | **Gerenciador de exibição** | **Ambiente de área de trabalho** | **Servidor de exibição** |
|--------------------|----------------------|----------------------|-----------------------------------|--------------------|
| Debian | APTO | GDM | GNOME | X11 |
| Debian | APTO | LightDM | Canela | X11 |
| Teste Debian | APTO | GDM | GNOME | Wayland |
| Fedora | DNF | GDM | GNOME | Wayland |
| Arco Linux | Pacman | SDDM | Plasma KDE | X11 |

### Termos-chave

#### Núcleo
O núcleo do sistema operacional que faz interface direta com o hardware, geralmente Linux.

#### Distribuição
A distribuição Linux, que empacota o kernel junto com ferramentas, bibliotecas e software de espaço do usuário. Exemplos incluem Debian, Arch Linux e Fedora.

#### Gerenciador de Pacotes
Uma ferramenta usada para instalar, atualizar e remover aplicativos de software de repositórios. Os exemplos incluem APT para distribuições baseadas em Debian, DNF para Fedora e Pacman para Arch Linux.

#### Gerenciador de exibição
Gerencia a tela gráfica de login e o início da sessão. Os exemplos incluem GDM (GNOME Display Manager), LightDM e SDDM (Simple Desktop Display Manager).

#### Ambiente de Trabalho
Fornece a interface gráfica do usuário (GUI) e gerencia a aparência geral e a experiência do usuário. Os exemplos incluem GNOME, Cinnamon e KDE Plasma.

#### Servidor de exibição
Gerencia a saída de exibição e os eventos de entrada. Os exemplos incluem X11 (sistema X Window) e Wayland. O X11 é um servidor de exibição tradicional, enquanto o Wayland é uma alternativa mais nova e segura.