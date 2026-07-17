---
title: "Instalar o Debian"
type: docs
url: "hub/install-linux/Installing-Debian"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 42b2f95f8ff71be8eff6777dfd9808855f99f943c55575079566588a08fd8fcd
---
Este documento descreve o processo usado para instalar o Debian Stable como sistema operativo de desenvolvimento do Lumi-o. Pode ser útil para quem configure um ambiente semelhante.

O Debian Stable foi seleccionado porque o Lumi pretende compilar de forma fiável numa plataforma previsível a longo prazo. O desenvolvimento do GIMP visa o Debian Testing, o que torna o Debian Stable uma base muito próxima.

O Lumi funciona melhor no Debian com Cinnamon (X11) e é desenvolvido e testado nesse ambiente. O Cinnamon oferece um fluxo de trabalho de secretária familiar, semelhante ao do Windows, enquanto o X11 tem sido o ambiente mais estável para o desenvolvimento do Lumi.

Se vier do Windows, a principal mudança conceptual é que a maior parte da instalação e configuração de software passa por gestores de pacotes e comandos simples de terminal, em vez de instaladores descarregáveis.

## Para quem é este guia

Este guia documenta uma configuração funcional do Debian Stable usada no desenvolvimento do Lumi. Não é um tutorial geral de instalação do Linux.

É mais útil para:

- artistas que migram do Windows e pretendem uma configuração previsível do Linux
- programadores que compilam o Lumi a partir do código-fonte
- utilizadores que preferem reproduzir um ambiente funcional conhecido em vez de desenhar a própria configuração do sistema

Assume-se familiaridade básica com particionamento de disco e uso simples da linha de comandos.

## Faça uma cópia de segurança dos dados

Antes de instalar o Debian, crie uma cópia de segurança completa da pasta pessoal (Home) num disco externo. Inclua quaisquer pastas de dados adicionais que queira preservar.

Nota: no Linux, `~` representa a pasta pessoal.

Se usar repositórios Git, envie alterações importantes para as origens remotas, para que possam ser restauradas facilmente após a instalação. Este passo só é relevante se já usar Git.

## Crie uma partição

Reserve espaço no disco principal para o Debian. Existem muitos guias e ferramentas para este passo, incluindo o GParted. Consoante a configuração, pode:

- reduzir uma partição Windows existente para arranque dual
- reutilizar uma partição Linux existente
- preparar novas partições Linux e swap

Se tiver dúvidas, consulte guias específicos do hardware antes de fazer alterações, pois os passos de particionamento variam significativamente entre sistemas.


## Crie uma pen USB de instalação do Debian

Assumindo que já existem uma partição de destino e espaço swap:

1. Descarregue a ISO do Debian a partir do site oficial: https://www.debian.org/
2. No Windows, use o BalenaEtcher para gravar a ISO numa pen USB.
3. No Linux, use uma ferramenta de linha de comandos como `dd` para criar uma pen USB de arranque.

## Instale o Debian

1. Insira a pen USB.
2. Reinicie e prima a tecla do menu de arranque (normalmente `F2`, `F12`, `Esc` ou `Del`) durante o arranque.
3. Seleccione o dispositivo USB.
4. Escolha um instalador não gráfico.
5. Deixe a palavra-passe root em branco quando solicitado, para que o instalador conceda acesso sudo à conta de utilizador.
6. Particione manualmente:

   - Sistema de ficheiros: ext4 (com journal)
   - Swap: partição swap existente
   - Ponto de montagem: `/`
   - Etiqueta: `linux`
   - Nome do anfitrião: nome do sistema mostrado como `user@hostname`
   - Conta de utilizador: nome completo
   - Nome de utilizador: nome de início de sessão no terminal

7. O instalador Debian oferece uma escolha de ambiente gráfico nesta fase; seleccione **Cinnamon** para a configuração recomendada pelo Lumi.
8. Conclua a instalação e reinicie no Debian Stable.

## Configuração do sistema

### Escala do ecrã

O Debian Stable trata actualmente a escala fraccionária de forma inconsistente, especialmente em monitores 4K. Em vez de reduzir a resolução do ecrã, ajuste os elementos da interface directamente.

Ajustes recomendados:

- Evite escala fraccionária do ecrã.
- Menu → Selecção de tipo de letra → Definições de tipo de letra → Factor de escala do texto: `2.5`
- Tipo de letra da secretária: `14`
- Painel → Personalizar → Altura do painel: `60`
- Aparência do painel → Tamanho do ícone simbólico da zona direita: `48px`
- Rato e touchpad → Ajuste do tamanho do ponteiro
- Secretária (clique com o botão direito) → Personalizar → Tamanho maior dos ícones

Ajuste no Firefox:

- Barra de endereços → `about:config`
- Defina `layout.css.devPixelsPerPx` para `1`

### Terminal

Configure as preferências do terminal:

1. Menu → Terminal → Editar → Preferências
2. Texto → Tamanho inicial: `140 columns`, `40 rows`
3. Texto → Tipo de letra personalizado: `Monospace 10`
4. Cores → Esquemas integrados → Solarized Dark

## Restaurar dados

Restaure os ficheiros da cópia de segurança para a pasta pessoal conforme necessário, por exemplo:

- `Backup/Home/Artwork` → `~/Artwork`
- `Backup/Home/code` → `~/code`
- `Backup/Home/Desktop` → `~/Desktop`
- `Backup/Home/.ssh` → `~/.ssh`
- `Backup/Home/.config/lumi` → `~/.config/lumi`

Nota: pastas que começam por `.` são directórios de configuração ocultos no Linux.

## Opcional: configuração do Git

Necessário apenas se planear compilar o Lumi ou restaurar repositórios.

### Instalar o Git

```bash
sudo apt install git
```

Configure a identidade:

```bash
git config --global --edit
```

#### Acesso ao GitLab

Restaure o acesso aos repositórios no GitLab ou GitHub:

1. Altere as permissões do ficheiro de chave SSH: `chmod 600 ~/.ssh/id_rsa`
2. Adicione a chave à nova instalação do Git: `ssh-add ~/.ssh/id_rsa`
3. Teste a ligação: `ssh -T git@ssh.gitlab.gnome.org` ou `ssh -T git@github.com`

Para cada repositório, obtenha as origens e repõe o ramo local para corresponder:

```bash
git reset --hard remote-name/branch-name
git clean -df
```

Execute `git status` para confirmar que os repositórios estão limpos.

Fica então um novo sistema operativo com dados e repositórios restaurados. Esta configuração reflecte um ambiente funcional conhecido usado no desenvolvimento do Lumi e pode ser adaptada a fluxos de trabalho individuais.

## Compilar o Lumi após a configuração do sistema operativo

Os scripts de compilação do Lumi estão em:

`~/code/lumi-dev/build/lumi/scripts`.

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# Instalar dependências uma vez
sudo bash lumi-install-packages.sh

# Primeira compilação completa de configuração
bash lumi-build-script.sh --scope setup --dir lumi-dev

# Recompilação normal após alterações ao código
bash lumi-build-script.sh --scope build --dir lumi-dev

# Caminho rápido de compilação
bash lumi-build-script.sh --scope compile --dir lumi-dev

# Lançar o Lumi
bash lumi-launch-active.sh lumi-dev
```
