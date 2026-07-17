---
title: "Instalando o Debian"
type: docs
url: "hub/install-linux/Installing-Debian"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 42b2f95f8ff71be8eff6777dfd9808855f99f943c55575079566588a08fd8fcd
---

Este documento descreve o processo usado para instalar o Debian Stable como sistema operacional de desenvolvimento do Lumi-o. Pode ser útil para quem estiver configurando um ambiente semelhante.

O Debian Stable foi escolhido porque o Lumi precisa compilar de forma confiável em uma plataforma previsível de longo prazo. O desenvolvimento do GIMP tem como alvo o Debian Testing, o que torna o Debian Stable um sistema base muito alinhado.

O Lumi funciona melhor no Debian com Cinnamon (X11) e é desenvolvido e testado nesse ambiente. O Cinnamon oferece um fluxo de trabalho de desktop familiar, semelhante ao do Windows, enquanto o X11 proporciona o ambiente mais estável para o desenvolvimento do Lumi.

Se você vem do Windows, a principal mudança conceitual é que a maior parte da instalação e configuração de software acontece por meio de gerenciadores de pacotes e comandos simples de terminal, em vez de instaladores para download.

## Para quem é este guia

Este guia documenta uma configuração funcional do Debian Stable usada para o desenvolvimento do Lumi. Não é um tutorial geral de instalação do Linux.

É mais útil para:

- artistas migrando do Windows que desejam uma configuração previsível do Linux
- desenvolvedores que compilam o Lumi a partir do código-fonte
- usuários que preferem reproduzir um ambiente de trabalho conhecido em vez de projetar a própria configuração do sistema

Pressupõe-se familiaridade básica com particionamento de disco e uso simples da linha de comando.

## Faça backup dos seus dados

Antes de instalar o Debian, crie um backup completo do seu diretório Home em uma unidade externa. Inclua quaisquer pastas de dados adicionais que você queira preservar.

Nota: no Linux, `~` representa seu diretório Home.

Se você usa repositórios Git, envie alterações importantes para os remotes para que possam ser restauradas facilmente após a instalação. Esta etapa só é relevante se você já usa Git.

## Crie uma partição

Prepare espaço no disco principal para o Debian. Existem muitos guias e ferramentas para esta etapa, incluindo o GParted. Dependendo da sua configuração, você pode:

- reduzir uma partição existente do Windows para dual boot
- reutilizar uma partição Linux existente
- preparar novas partições Linux e swap

Se não tiver certeza, consulte guias específicos de hardware antes de fazer alterações, pois as etapas de particionamento variam significativamente entre os sistemas.


## Crie um USB de instalação do Debian

Supondo que já existam uma partição de destino e espaço de swap:

1. Baixe a ISO do Debian no site oficial: https://www.debian.org/
2. No Windows, use o BalenaEtcher para gravar a ISO em uma unidade USB.
3. No Linux, use uma ferramenta de linha de comando como `dd` para criar um USB inicializável.

## Instale o Debian

1. Insira a unidade USB.
2. Reinicie e pressione a tecla do menu de inicialização (geralmente `F2`, `F12`, `Esc` ou `Del`) durante a inicialização.
3. Selecione o dispositivo USB.
4. Escolha um instalador não gráfico.
5. Deixe a senha root em branco quando solicitado, para que o instalador conceda acesso sudo à sua conta de usuário.
6. Particione manualmente:

   - Sistema de arquivos: ext4 (journaling)
   - Swap: partição swap existente
   - Ponto de montagem: `/`
   - Rótulo: `linux`
   - Hostname: nome do sistema exibido como `user@hostname`
   - Conta de usuário: seu nome completo
   - Nome de usuário: nome de login no terminal

7. O instalador do Debian oferece uma escolha de ambiente de desktop nesta etapa; selecione **Cinnamon** para a configuração recomendada pelo Lumi.
8. Conclua a instalação e reinicie no Debian Stable.

## Configuração do sistema

### Escala de exibição

Atualmente, o Debian Stable lida com escalonamento fracionário de forma inconsistente, especialmente em monitores 4K. Em vez de reduzir a resolução da tela, ajuste os elementos da interface diretamente.

Ajustes recomendados:

- Evite escalonamento fracionário da tela.
- Menu → Font Selection → Font Settings → Text Scaling Factor: `2.5`
- Desktop Font: `14`
- Panel → Customize → Panel Height: `60`
- Panel Appearance → Right Zone Symbolic Icon Size: `48px`
- Mouse and Touchpad → Pointer Size adjustment
- Desktop (right-click) → Customize → Larger icon size

Ajuste do Firefox:

- Address bar → `about:config`
- Defina `layout.css.devPixelsPerPx` como `1`

### Terminal

Configure as preferências do terminal:

1. Menu → Terminal → Edit → Preferences
2. Text → Initial size: `140 columns`, `40 rows`
3. Text → Custom font: `Monospace 10`
4. Colours → Built-in schemes → Solarized Dark

## Restaure os dados

Restaure os arquivos de backup no diretório Home conforme necessário, por exemplo:

- `Backup/Home/Artwork` → `~/Artwork`
- `Backup/Home/code` → `~/code`
- `Backup/Home/Desktop` → `~/Desktop`
- `Backup/Home/.ssh` → `~/.ssh`
- `Backup/Home/.config/lumi` → `~/.config/lumi`

Nota: pastas que começam com `.` são diretórios de configuração ocultos no Linux.

## Opcional: configuração do Git

Necessário apenas se você planeja compilar o Lumi ou restaurar repositórios.

### Instale o Git

```bash
sudo apt install git
```

Configure sua identidade:

```bash
git config --global --edit
```

#### Acesso ao GitLab

Restaure o acesso aos repositórios no GitLab ou GitHub:

1. Altere as permissões do arquivo de chave SSH: `chmod 600 ~/.ssh/id_rsa`
2. Adicione a chave ao agente SSH: `ssh-add ~/.ssh/id_rsa`
3. Teste a conexão: `ssh -T git@ssh.gitlab.gnome.org` ou `ssh -T git@github.com`

Para cada repositório, busque os remotes e redefina o branch local para corresponder:

```bash
git reset --hard remote-name/branch-name
git clean -df
```

Execute `git status` para confirmar que os repositórios estão limpos.

Agora você tem um novo sistema operacional com dados e repositórios restaurados. Esta configuração reflete um ambiente de trabalho conhecido usado para o desenvolvimento do Lumi e pode ser adaptada a fluxos de trabalho individuais conforme necessário.

## Compile o Lumi após a configuração do sistema

Os scripts de compilação do Lumi estão em:

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

