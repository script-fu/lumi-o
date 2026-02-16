---
title: "Instalando o Debian"
type: docs
url: "hub/install-linux/Installing-Debian"
---
Este documento descreve o processo usado para instalar o Debian Stable como sistema operacional de desenvolvimento do Lumi·o. Pode ser útil para outras pessoas que criem um ambiente semelhante.

O Debian Stable foi selecionado porque a Lumi pretende construir de forma confiável em uma plataforma previsível de longo prazo. O desenvolvimento do GIMP tem como alvo o Debian Testing, tornando o Debian Stable um sistema base estreitamente alinhado.

Se você vem do Windows, a principal mudança conceitual é que a maior parte da instalação e configuração do software ocorre por meio de gerenciadores de pacotes e comandos de terminal simples, em vez de instaladores para download.

## Para quem é este guia

Este guia documenta uma configuração estável do Debian usada para desenvolvimento do Lumi. Não é um tutorial geral de instalação do Linux.

É mais útil para:

- artistas migrando do Windows que desejam uma configuração previsível do Linux
- desenvolvedores construindo Lumi a partir da fonte
- usuários que preferem reproduzir um ambiente de trabalho conhecido em vez de projetar a própria configuração do sistema

Presume-se familiaridade básica com particionamento de disco e uso simples de linha de comando.

## Faça backup dos seus dados

Antes de instalar o Debian, crie um backup completo do seu diretório Home em uma unidade externa. Inclua quaisquer pastas de dados adicionais que você deseja preservar.

Nota: No Linux, `~` representa seu diretório inicial.

Se você usa repositórios Git, envie quaisquer alterações importantes para suas origens para que possam ser restauradas facilmente após a instalação. Esta etapa só é relevante se você já usa Git.

## Crie uma partição

Crie espaço em sua unidade principal para o Debian. Existem muitos guias e ferramentas para esta etapa, incluindo o GParted. Dependendo da sua configuração, você pode:

- reduzir uma partição existente do Windows para inicialização dupla
- reutilizar uma partição Linux existente
- preparar novo Linux e trocar partições

Se não tiver certeza, consulte os guias específicos de hardware antes de fazer alterações, pois as etapas de particionamento variam significativamente entre os sistemas.


## Crie um USB de instalação do Debian

Supondo que já exista uma partição de destino e um espaço de troca:

1. Baixe o ISO do Debian do site oficial: https://www.debian.org/
2. No Windows, use BalenaEtcher para gravar o ISO em uma unidade USB.
3. No Linux, use uma ferramenta de linha de comando como `dd` para criar um USB inicializável.

## Instale o Debian

1. Insira a unidade USB.
2. Reinicie e pressione a tecla do menu de inicialização (normalmente `F2`, `F12`, `Esc` ou `Del`) durante a inicialização.
3. Selecione o dispositivo USB.
4. Escolha um instalador não gráfico.
5. Deixe a senha root em branco quando solicitado para que o instalador conceda acesso sudo à sua conta de usuário.
6. Particione manualmente:

   - Sistema de arquivos: ext4 (diário)
   - Swap: partição swap existente
   - Ponto de montagem: `/`
   - Etiqueta: `linux`
   - Hostname: nome do sistema mostrado como `user@hostname`
   - Conta de usuário: seu nome completo
   - Nome de usuário: nome de login do terminal

7. Selecione **Cinnamon** como ambiente de área de trabalho.
8. Conclua a instalação e reinicie no Debian Stable.

## Configuração do sistema

### Escala de exibição

O Debian Stable atualmente lida com escalamento fracionário de forma inconsistente, especialmente em monitores 4K. Em vez de reduzir a resolução da tela, ajuste os elementos da interface diretamente.

Ajustes recomendados:- Evite escala de exibição fracionária.
- Menu → Seleção de fonte → Configurações de fonte → Fator de escala de texto: `2.5`
- Fonte da área de trabalho: `14`
- Painel → Personalizar → Altura do painel: `60`
- Aparência do painel → Tamanho do ícone simbólico da zona direita: `48px`
- Mouse e Touchpad → Ajuste do tamanho do ponteiro
- Área de trabalho (clique com o botão direito) → Personalizar → Tamanho maior do ícone

Ajuste do Firefox:

- Barra de endereço → `about:config`
- Defina `layout.css.devPixelsPerPx` para `1`

### Terminal

Configure as preferências do terminal:

1. Menu → Terminal → Editar → Preferências
2. Texto → Tamanho inicial: `140 columns`, `40 rows`
3. Texto → Fonte personalizada: `Monospace 10`
4. Cores → Esquemas integrados → Escuro Solarizado

## Restaurar dados

Restaure os arquivos de backup no diretório inicial conforme necessário, por exemplo:

- `Backup/Home/Artwork` → `~/Artwork`
- `Backup/Home/code` → `~/code`
- `Backup/Home/Desktop` → `~/Desktop`
- `Backup/Home/.ssh` → `~/.ssh`
- `Backup/Home/.config/lumi` → `~/.config/lumi`

Nota: Pastas que começam com `.` são diretórios de configuração ocultos no Linux.

## Opcional: configuração do Git

Obrigatório apenas se você planeja construir o Lumi ou restaurar repositórios.

### Instale o Git

```bash
sudo apt install git
```

Configure sua identidade:

```bash
git config --global --edit
```

#### Acesso GitLab

Restaure o acesso ao repositório no GitLab ou GitHub:

1. Altere as permissões no arquivo de chave SSH: `chmod 600 ~/.ssh/id_rsa`
2. Adicione o usuário à nova instalação do Git: `ssh-add ~/.ssh/id_rsa`
3. Teste a conexão: `ssh -T git@ssh.gitlab.gnome.org` ou `ssh -T git@github.com`

Para cada repositório, busque as origens e redefina a ramificação local para corresponder:

```bash
git reset --hard remote-name/branch-name
git clean -df
```

Execute `git status` para confirmar que os repositórios estão limpos.

Agora temos um novo sistema operacional com todos os dados e repositórios restaurados. Esta configuração reflete um ambiente de trabalho conhecido usado para o desenvolvimento do Lumi e pode ser adaptado a fluxos de trabalho individuais conforme necessário.

## Construa o Lumi após a configuração do sistema operacional

Os scripts de construção do Lumi estão localizados em:

`~/code/lumi-dev/build/lumi/scripts`.

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# Install dependencies once
sudo bash lumi-install-packages.sh

# First full setup build
bash lumi-build-script.sh --scope setup --dir lumi-dev

# Regular rebuild after code changes
bash lumi-build-script.sh --scope build --dir lumi-dev

# Quick compile path
bash lumi-build-script.sh --scope compile --dir lumi-dev

# Launch Lumi
bash lumi-launch-active.sh lumi-dev
```