---
title: "Condicionais"
type: docs
weight: 2
translation_provenance: ai-reviewed
translation_source_sha256: 8e9a64dd1bc1445c996fe17ce6b666b8d597ab16040cf0ef0876232026ff11b2
translation_lock: true
url: "hub/scripting/fundamentals/Conditionals"
---
As condicionais são uma parte fundamental da programação, permitindo que os scripts tomem decisões e controlem o fluxo com base em critérios específicos. No Scheme, as condicionais permitem criar scripts dinâmicos e inteligentes que se adaptam às mudanças de entradas, ambientes ou ações do utilizador.

### O papel dos condicionais no Scheme

As condicionais cumprem várias funções importantes nos scripts:
- **Controlo de fluxo:** Permitem executar diferentes partes de código dependendo se certas condições são verdadeiras ou falsas.
- **Mais flexibilidade:** Ao responder dinamicamente a entradas ou estados, os condicionais ajudam o script a lidar com uma variedade de cenários.
- **Simplificar a complexidade:** Dividem a tomada de decisões em estruturas geríveis, tornando o código mais fácil de ler, depurar e manter.

### Tipos de condicionais disponíveis

Scheme fornece diversas construções condicionais, cada uma adequada a diferentes necessidades lógicas:
- **`if`:** Para tomar decisões binárias simples, executando um bloco de código se uma condição for verdadeira e outro se for falsa.
- **`cond`:** Uma poderosa construção multi-ramificada para lidar com múltiplas condições de forma clara e estruturada.
- **`and` / `or`:** Operadores lógicos que avaliam combinações de condições, possibilitando tomadas de decisões mais complexas.
- **`else`:** Um fallback que define o comportamento alternativo quando nenhuma das condições especificadas é atendida.

### Como funcionam as condicionais

Condicionais normalmente envolvem:
1. **Avaliar uma condição:** Uma expressão de teste determina se uma condição é verdadeira ou falsa.
2. **Execução ramificada:** Com base na avaliação, o script seleciona qual bloco de código será executado.
3. **Devolver um valor (opcional):** Em alguns casos, condicionais também podem produzir um valor que outras partes do script podem usar.