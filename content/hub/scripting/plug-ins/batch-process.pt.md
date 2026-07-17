---
title: "Processo em lote"
type: docs
translation_provenance: ai-reviewed
translation_source_sha256: 141b5ee23e77ecfc8ef5e8112706cfaebc1a2a528518218296dffda5b9dee6d1
url: "hub/scripting/plug-ins/batch-process"
translation_lock: true
---
Um exemplo prático e completo para processar muitos ficheiros de uma só vez.

## Código-fonte

- [Ver o código-fonte](https://gitlab.gnome.org/pixelmixer/lumi-dev/-/blob/main/plug-ins/lumi/batch-process/batch-process.scm)

## Menu no Lumi

- **Arquivo → Processo em lote**

## O que demonstra

- Parâmetros `SF-DIRNAME` para diretórios de origem/destino
- Validação de caminhos GUI com substitutos (`validate-path-and-dir`)
- Análise e iteração recursiva de diretórios
- Relatórios de progresso para operações de longa duração