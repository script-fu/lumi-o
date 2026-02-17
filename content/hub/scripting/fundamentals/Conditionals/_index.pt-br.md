---
title: "Condicionais"
type: docs
weight: 2
---
As condicionais são uma parte fundamental da programação, permitindo que os scripts tomem decisões e controlem seu fluxo com base em critérios específicos. No Scheme, que é baseado na linguagem de programação Scheme, as condicionais permitem criar scripts dinâmicos e inteligentes que se adaptam às mudanças de entradas, ambientes ou ações do usuário.

### O papel dos condicionais no esquema

Condicionais atendem a vários propósitos importantes em seus scripts:
- **Lógica de direção:** Eles permitem que você execute diferentes partes de código dependendo se certas condições são verdadeiras ou falsas.
- **Melhorando a flexibilidade:** Ao responder dinamicamente a entradas ou estados, os condicionais ajudam seu script a lidar com uma variedade de cenários.
- **Simplificando a complexidade:** Eles dividem a tomada de decisões em estruturas gerenciáveis, tornando o código mais fácil de ler, depurar e manter.

### Tipos de condicionais disponíveis

Scheme fornece diversas construções condicionais, cada uma adequada a diferentes necessidades lógicas:
- **`if`:** Para tomar decisões binárias simples, executando um bloco de código se uma condição for verdadeira e outro se for falsa.
- **`cond`:** Uma poderosa construção multi-ramificada para lidar com múltiplas condições de forma clara e estruturada.
- **`and` / `or`:** Operadores lógicos que avaliam combinações de condições, possibilitando tomadas de decisões mais complexas.
- **`else`:** Um genérico que define o comportamento de fallback quando nenhuma das condições especificadas é atendida.

### Como funcionam as condicionais

Condicionais normalmente envolvem:
1. **Avaliando uma condição:** Uma expressão de teste determina se uma condição é verdadeira ou falsa.
2. **Execução de ramificação:** Com base na avaliação, o script seleciona qual bloco de código será executado.
3. **Retornando um valor (opcional):** Em alguns casos, condicionais também podem produzir um valor que outras partes do script podem usar.