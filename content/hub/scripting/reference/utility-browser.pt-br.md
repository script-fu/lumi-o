---
title: "Navegador de utilitários"
type: docs
---
O Navegador de Utilitários permite que você explore o utilitário Scheme stdlib integrado que acompanha o Lumi - sem ter que sair do aplicativo ou vasculhar os arquivos de origem.

## Abrindo o navegador de utilitários

Vá para **Ajuda → Programação → Navegador de Utilitários**.

A janela abre imediatamente; nenhum plug-in precisa ser carregado antecipadamente.

## O que mostra

O navegador lista todos os procedimentos, variáveis e formulários de sintaxe exportados pelas sete bibliotecas de utilitários que o Lumi carrega automaticamente na inicialização:

| Biblioteca | O que cobre |
|---|---|
| `common.scm` | Auxiliares de uso geral (string, número, utilitários de lista) |
| `files.scm` | Ajudantes de arquivo e caminho |
| `gegl.scm` | Buffer GEGL e ajudantes de cores |
| `images.scm` | Auxiliares de nível de imagem (`image-get-open-list`, etc.) |
| `layers.scm` | Ajudantes de camada e drawable |
| `parasites.scm` | Ajudantes de leitura/gravação de parasitas |
| `paths.scm` | Ajudantes de caminho e vetor |

Tudo isso está disponível em qualquer plug-in do Scheme ou no Scheme Console.

## Pesquisa e filtragem

- **Caixa de pesquisa** — filtra por nome conforme você digita (correspondência de substring sem distinção entre maiúsculas e minúsculas).
- **Filtro de tipo** — restrinja os resultados para `procedure`, `variable` ou `syntax`.

Clicar em uma entrada mostra sua documentação completa e a biblioteca de onde ela vem.

## O Stdlib como wrappers

As bibliotecas de utilitários são uma aplicação prática do padrão de encapsulamento: cada auxiliar fornece um nome claro para uma operação de baixo nível, oculta o padrão e fornece um único local para atualização se o comando subjacente for alterado. Se você quiser entender a abordagem de design por trás deles, consulte o tutorial **[Wrapping](@@LUMI_TOKEN_11@@)**.

## Relacionamento com o navegador de procedimentos

O Navegador de Utilitários é separado de **Filtros → Script-Fu → Console → Navegar** (o Navegador de Procedimento). O Navegador de procedimentos lista os procedimentos registrados no PDB. O Navegador de Utilitários lista definições auxiliares que residem intencionalmente *fora* do PDB — elas são apenas do esquema e não possuem ligação C.