---
title: "Instalação"
type: docs
---
Você precisa do Git para a etapa inicial de clonagem abaixo. Se o Git ainda não estiver instalado, instale-o primeiro (Debian/Ubuntu: `sudo apt install git`) ou siga: [Using Git on Linux](/hub/technical-guides/Using-Git-on-Linux/)

## 1) Clone Lumi (configuração inicial)

Crie o diretório do Lumi e use o Git para clonar o código fonte.

```bash
sudo apt install git

mkdir -p ~/code
cd ~/code

# Clone via SSH (matches the Git guide above)
git clone git@ssh.gitlab.gnome.org:pixelmixer/lumi-dev.git lumi-dev

# Or clone via HTTPS (no SSH key setup)
# git clone https://gitlab.gnome.org/pixelmixer/lumi-dev.git lumi-dev
```

## 2) Instalar dependências (configuração inicial)

```bash
cd ~/code/lumi-dev/build/lumi/scripts
sudo bash lumi-install-packages.sh
```

## 3) Construir Lumi (configuração inicial)

Primeira compilação de configuração completa (primeira vez ou após grandes alterações):

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope setup --dir lumi-dev
```

## 4) Inicie o Lumi

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-launch-active.sh lumi-dev
```

## Opcional: Reconstruir / Compilar

Reconstrução normal após alterações de código:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope build --dir lumi-dev
```

Caminho rápido somente de compilação:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope compile --dir lumi-dev
```

Crie um único componente integrado (substitua `babl` por `gegl` ou `gtk3`):

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope build --dir lumi-dev --component babl
```

## Opcional: Tipos de compilação

Use `--type` quando necessário:

- `debug` – depuração de fluxos de trabalho
- `debugoptimized` – padrão balanceado para desenvolvimento
- `release` – tempo de execução mais rápido

Exemplo:

```bash
bash lumi-build-script.sh --scope build --dir lumi-dev --type release
```