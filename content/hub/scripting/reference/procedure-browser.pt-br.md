---
title: "Navegador de procedimentos"
type: docs
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: f2593585be79d09f94dee166e7003ceddc532b4d2f7c1060222fe5f5c758ef27
---
O Procedure Browser é a principal ferramenta de referência para descobrir as centenas de funções disponíveis no Procedural Database (PDB) do Lumi. Como cada ferramenta, filtro e script no Lumi deve ser registrado no PDB para poder ser chamado, este navegador é efetivamente um explorador de PDB completo.

## Abrindo o navegador de procedimentos

Vá para **Ajuda → Programação → Navegador de procedimentos**.

Você também pode acessá-lo no Scheme Console via **Browse**.

## O que mostra

O Navegador de Procedimentos pode listar todos os procedimentos atualmente registrados no PDB, independentemente de sua origem. O padrão é pesquisar por "interno" para mostrar os procedimentos principais registrados internamente.

- **Procedimentos Internos**: Funções principais para manipulação de imagens, gerenciamento de camadas e controle de ferramentas.
- **Plug-ins Externos**: Procedimentos fornecidos por plug-ins C/C++ compilados ou extensões persistentes.

## Pesquisa e filtragem

- **Caixa de pesquisa**: Filtra procedimentos por nome, descrição ou autor. Limpar o campo de pesquisa mostra todos os procedimentos disponíveis.
- **Tipo de pesquisa**: o menu suspenso de pesquisa permite filtrar por campos específicos. Se você definir como **por tipo** e procurar por "interno", a lista será reduzida para mostrar apenas os procedimentos principais registrados internamente.
- **Visualização detalhada**: clicar em um procedimento mostra seus parâmetros, valores de retorno, autor, data e uma descrição do que ele faz.

Isso é essencial para encontrar o nome exato e a assinatura do argumento de uma função que você deseja chamar em seu script.
