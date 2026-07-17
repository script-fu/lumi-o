---
title: "Navegador de utilitários"
type: docs
translation_provenance: ai-reviewed
translation_source_sha256: 99abaafdc68cf3433959e5db87130b22c51cfbd5a98697fa807732b9fdae9ff0
url: "hub/scripting/reference/utility-browser"
translation_lock: true
---
O Navegador de utilitários permite explorar o utilitário de Scheme integrado stdlib que acompanha o Lumi, sem precisar sair do aplicação ou vasculhar os ficheiros de origem.

## Abrindo o navegador de utilitários

Vá para **Ajuda → Programação → Navegador de Utilitários**.

A janela abre imediatamente; nenhum plug-in precisa ser carregado antecipadamente.

## O que mostra

O navegador lista todos os procedimentos, variáveis e formulários de sintaxe exportados pelas sete bibliotecas de utilitários que o Lumi carrega automaticamente na inicialização:

| Biblioteca | O que cobre |
|---|---|
| `common.scm` | Auxiliares de uso geral (string, número, utilitários de lista) |
| `files.scm` | Ajudantes de ficheiro e caminho |
| `gegl.scm` | Buffer GEGL e ajudantes de cores |
| `images.scm` | Auxiliares de nível de imagem (`image-get-open-list`, etc.) |
| `layers.scm` | Ajudantes de camada e drawable |
| `parasites.scm` | Ajudantes de leitura/gravação de parasitas |
| `paths.scm` | Ajudantes de caminho e vetor |

Tudo isso está disponível em qualquer plug-in do Scheme ou no Scheme Console.

## Pesquisa e filtragem

- **Caixa de pesquisa**: filtra por nome conforme digita (correspondência de substring sem distinção entre maiúsculas e minúsculas).
- **Filtro de tipo**: restrinja os resultados para `procedure`, `variable` ou `syntax`.

Clicar em uma entrada mostra a documentação completa e a biblioteca de onde ela vem.

## O Stdlib como wrappers

As bibliotecas de utilitários são uma aplicação prática do padrão de encapsulamento: cada auxiliar fornece um nome claro para uma operação de baixo nível, oculta o padrão e fornece um único local para atualização se o comando subjacente for alterado. Se quiser entender a abordagem de design por trás deles, consulte o tutorial **[Wrapping]({{< ref "/hub/scripting/tutorials/Wrapping/wrapping" >}})**.

## Relacionamento com o navegador de procedimentos

O Navegador de Utilitários é separado de **Filtros → Script-Fu → Console → Navegar** (o Navegador de Procedimento). O Navegador de procedimentos lista os procedimentos registados no PDB. O Navegador de Utilitários lista definições auxiliares que residem intencionalmente *fora* do PDB: elas são apenas de Scheme e não possuem ligação C.