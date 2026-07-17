---
title: "Estruturas de dados"
type: docs
weight: 3
translation_provenance: ai-reviewed
translation_source_sha256: 352594bbda9977488d773240c50663f63fd432a17483772a9cbf8d59dab378be
translation_lock: true
url: "hub/scripting/fundamentals/Data Structures/_index"
---
No Scheme, **estruturas de dados** são ferramentas essenciais para organizar, armazenar e manipular dados. Permitem que os desenvolvedores criem scripts eficientes, legíveis e reutilizáveis. Ao escolher a estrutura de dados certa para um problema específico, pode otimizar o desempenho e a clareza do código.

## Principais estruturas de dados no Scheme

O Scheme fornece diversas estruturas de dados poderosas e versáteis, cada uma adequada para tarefas específicas. As estruturas de dados primárias incluem:

### Listas

Listas são coleções ordenadas de elementos que podem aumentar ou diminuir dinamicamente. Eles são ideais para dados sequenciais ou hierárquicos e são amplamente utilizados em programação funcional.

Principais recursos:
- Dimensionado dinamicamente.
- Os elementos podem ser de tipos mistos.
- Comumente usado para algoritmos recursivos e representação de estruturas semelhantes a árvores.

Exemplos de uso:
- Gerir coleções de itens.
- Representando sequências ou hierarquias.

---

### Vetores

Vetores são coleções de elementos de tamanho fixo, indexados para acesso rápido. Eles são mais adequados para cenários onde o desempenho e o acesso posicional são críticos.

Principais recursos:
- Tamanho fixo na criação.
- Os elementos são acessados pelo o índice.
- Mais rápido que listas para determinadas operações, como acesso aleatório.

Exemplos de uso:
- Armazenamento de configurações ou dados de tamanho fixo.
- Pesquisas e atualizações rápidas com base na posição.

---

### Escolhendo a estrutura de dados correta

A decisão de usar uma **lista** ou um **vetor** depende das necessidades específicas do script. Aqui estão algumas diretrizes:

| Recurso | Listas | Vetores |
|-------------------------|----------------------------------------|--------------------------------|
| **Flexibilidade de tamanho** | Dinâmico | Fixo |
| **Velocidade de acesso** | Mais lento (acesso sequencial) | Mais rápido (acesso indexado) |
| **Facilidade de modificação**| Mais fácil | Mais difícil (requer realocação)|
| **Casos de uso** | Dados dinâmicos, recursão | Dados estáticos, pesquisas rápidas |

---