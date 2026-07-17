---
title: "Variáveis e Escopo"
type: docs
weight: 1
translation_provenance: ai-reviewed
translation_source_sha256: 82a033dab5a3f8e3bacc73cde3d2f965fda6cd1b8957877e29da8cfcb547abdd
translation_lock: true
url: "hub/scripting/fundamentals/Variables and Scope/_index"
---
No Scheme, o gestão de variáveis e respetivo âmbito é um conceito central para escrever scripts eficientes e de fácil manutenção. As variáveis armazenam valores de dados que o script pode manipular, enquanto o escopo define onde essas variáveis são acessíveis. Compreender como definir e usar variáveis de forma eficaz permite criar código estruturado, reutilizável e livre de erros.

### Digitação Dinâmica

O Scheme é digitado dinamicamente: não declara os tipos antecipadamente e uma variável pode conter valores de diferentes tipos ao longo do tempo.

```scheme
(define x 42) ; x é um número
(set! x "hello") ; agora x é uma string
```

### O papel das definições de variáveis e do escopo no Scheme

Definir variáveis e gerir o respetivo âmbito serve a vários propósitos:
- **Organização de dados:** As variáveis armazenam informações, tornando os scripts mais legíveis e geríveis.
- **Mais capacidade de reutilização:** Ao usar variáveis com escopo definido, pode reutilizar seções de código sem conflitos.
- **Encapsulamento:** O escopo localizado evita interações não intencionais entre variáveis em diferentes partes do script.
- **Simplificar a lógica:** Variáveis temporárias em um escopo limitado reduzem a complexidade em cálculos ou fluxos de trabalho maiores.

### Tipos de definições e escopo de variáveis

O Scheme fornece várias construções para definir e definir o escopo de variáveis:
- **`let`:** Cria ligações locais para variáveis dentro de um bloco específico de código.
- **`let*`:** Uma versão sequencial de `let` onde cada ligação pode depender das anteriores.
- **Nomeado `let`:** Uma construção poderosa para definir procedimentos ou loops locais recursivos.
- **`define`:** Cria variáveis ou funções globais que são acessíveis em todo o script.

### Como funcionam as definições de variáveis e o escopo

As definições e o escopo das variáveis normalmente envolvem:
1. **Declaração de Variáveis:** Atribuir um valor a uma variável em um contexto específico.
2. **Limitando o escopo:** Controlar onde a variável é acessível (por exemplo, dentro de um bloco `let` ou globalmente).
3. **Uso de variáveis:** Acessar e modificar valores de variáveis para realizar cálculos, lógicas ou operações procedimentais.

### Exemplo: usando `let` para variáveis locais

A construção `let` permite definir variáveis temporárias que estão disponíveis apenas dentro de um bloco específico:

```scheme
(let ((x 10)
 (y 20))
 (+ x y))
```

- Este exemplo declara `x` e `y` com valores locais e calcula a soma.

### Exemplo: usando `define` para variáveis globais

A construção `define` cria variáveis ou funções com escopo global:

```scheme
(define pi 3.14159)
(define (circle-area radius)
 (* pi radius radius))
```

- Este script define uma constante global `pi` e uma função `circle-area` que a utiliza.

### Comparação de escopo: local x global

| Recurso | Escopo local (`let`, `let*`) | Escopo Global (@LUMI_TOKEN_19@@) |
|------------------|------------------------------------------|----------------------------------------------------------|
| **Acessibilidade** | Limitado ao bloco em que está definido | Acessível em todo o roteiro |
| **Encapsulamento** | Evita interações não intencionais | Pode entrar em conflito com outras variáveis definidas globalmente |
| **Caso de uso** | Variáveis temporárias para tarefas específicas | Variáveis ou funções compartilhadas usadas em todo |

### Resumo

- **Definições e escopo de variáveis** são fundamentais para organizar e gerir dados nos scripts de Scheme.

- Use **escopo local** (`let`, `let*`, denominado `let`) para encapsular variáveis temporárias e evitar conflitos.
- Use **escopo global** (`define`) para funções reutilizáveis ou constantes compartilhadas no script.
- Uma compreensão clara dessas construções melhorará a legibilidade, a capacidade de manutenção e a confiabilidade do código.