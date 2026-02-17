---
title: "Processo em lote"
type: docs
---
Um exemplo prático e completo para processar muitos arquivos de uma só vez.

## Onde mora

- [View the source](https://gitlab.gnome.org/pixelmixer/lumi-dev/-/blob/main/plug-ins/lumi/batch-process/batch-process.scm)

## Onde aparece no Lumi

- **Arquivo → Processo em lote**

## O que demonstra

- Parâmetros `SF-DIRNAME` para diretórios de origem/destino
- Validando caminhos GUI com substitutos (`validate-path-and-dir`)
- Verificação e iteração recursiva de diretórios
- Relatórios de progresso para operações de longa duração