---
title: "Condicionais"
type: docs
weight: 2
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 8e9a64dd1bc1445c996fe17ce6b666b8d597ab16040cf0ef0876232026ff11b2
url: "hub/scripting/fundamentals/Conditionals/_index"
---
Condicionais são um elemento fundamental da programação: permitem que scripts tomem decisões e controlem seu fluxo com base em critérios específicos. Em Scheme, baseado na linguagem de programação Scheme, os condicionais ajudam a criar scripts dinâmicos e inteligentes que se adaptam a entradas, ambientes ou ações do usuário em mudança.

### O papel dos condicionais em Scheme

Os condicionais cumprem várias funções essenciais nos seus scripts:
- **Direcionar a lógica:** Executam trechos de código diferentes conforme certas condições são verdadeiras ou falsas.
- **Mais flexibilidade:** Ao responder dinamicamente a entradas ou estados, ajudam o script a lidar com diversos cenários.
- **Simplificar a complexidade:** Decompõem a tomada de decisão em estruturas gerenciáveis, facilitando leitura, depuração e manutenção do código.

### Tipos de condicionais disponíveis

Scheme oferece várias construções condicionais, cada uma adequada a necessidades lógicas diferentes:
- **`if`:** Para decisões binárias simples — um bloco se a condição for verdadeira, outro se for falsa.
- **`cond`:** Uma construção poderosa de ramificação múltipla para tratar várias condições de forma clara e estruturada.
- **`and` / `or`:** Operadores lógicos que avaliam combinações de condições para decisões mais complexas.
- **`else`:** Um caso padrão que define o comportamento quando nenhuma condição especificada é atendida.

### Como funcionam os condicionais

Condicionais normalmente envolvem:
1. **Avaliar uma condição:** Uma expressão de teste determina se uma condição é verdadeira ou falsa.
2. **Execução ramificada:** Com base na avaliação, o script escolhe qual bloco de código executar.
3. **Retornar um valor (opcional):** Em alguns casos, os condicionais também produzem um valor utilizável em outras partes do script.