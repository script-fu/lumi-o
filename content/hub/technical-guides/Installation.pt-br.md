---
title: "Instalação"
type: docs
url: "hub/technical-guides/Installation"
---
Este guia usa os scripts de construção Lumi atuais em:

`~/code/lumi-dev/build/lumi/scripts`

## 1) Instalar dependências (configuração inicial)

```bash
cd ~/code/lumi-dev/build/lumi/scripts
sudo bash lumi-install-packages.sh
```

## 2) Construir Lumi

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# First full setup build (BABL + lumi-gtk3 + GEGL + Lumi)
bash lumi-build-script.sh --scope setup --dir lumi-dev

# Normal rebuild after code changes
bash lumi-build-script.sh --scope build --dir lumi-dev

# Quick compile-only path
bash lumi-build-script.sh --scope compile --dir lumi-dev
```

## 3) Inicie o Lumi

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-launch-active.sh lumi-dev
```

## Tipos de compilação

Use `--type` quando necessário:

- `debug` – fluxos de trabalho de depuração
- `debugoptimized` – padrão balanceado para desenvolvimento
- `release` – tempo de execução mais rápido

Exemplo:

```bash
bash lumi-build-script.sh --scope build --dir lumi-dev --type release
```