---
title: "Instalação"
type: docs
url: "hub/technical-guides/Installation"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: ff429321515ea8c3b77a6f1f0cfd2486c8042e168032b9b0bec97b497930e25e
---
Precisa do Git para o passo inicial de clonagem abaixo. Se o Git ainda não estiver instalado, instale-o primeiro (Debian/Ubuntu: `sudo apt install git`) ou siga: [Usar Git no Linux](/hub/technical-guides/Using-Git-on-Linux/)

## 1) Clonar o Lumi (configuração inicial)

Crie o directório do Lumi e use o Git para clonar o código-fonte.

```bash
sudo apt install git

mkdir -p ~/code
cd ~/code

# Clonar via SSH (corresponde ao guia Git acima)
git clone git@ssh.gitlab.gnome.org:pixelmixer/lumi-dev.git lumi-dev

# Ou clonar via HTTPS (sem configuração de chave SSH)
# git clone https://gitlab.gnome.org/pixelmixer/lumi-dev.git lumi-dev
```

## 2) Instalar dependências (configuração inicial)

```bash
cd ~/code/lumi-dev/build/lumi/scripts
sudo bash lumi-install-packages.sh
```

## 3) Compilar o Lumi (configuração inicial)

Primeira compilação completa de configuração (primeira vez ou após alterações importantes):

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope setup --dir lumi-dev
```

## 4) Lançar o Lumi

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-launch-active.sh lumi-dev
```

## Opcional: recompilar

Recompilação normal após alterações ao código:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope build --dir lumi-dev
```

Caminho rápido apenas de compilação:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope compile --dir lumi-dev
```

Compilar um único componente integrado (substitua `babl` por `gegl` ou `gtk3`):

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope build --dir lumi-dev --component babl
```

## Opcional: tipos de compilação

Use `--type` quando necessário:

- `debug` – fluxos de trabalho de depuração
- `debugoptimized` – predefinição equilibrada para desenvolvimento
- `release` – execução mais rápida

Exemplo:

```bash
bash lumi-build-script.sh --scope build --dir lumi-dev --type release
```
