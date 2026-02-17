---
title: "Refatoração"
type: docs
weight: 2
---
Assim que tivermos uma função funcionando, podemos dar um passo atrás e pensar na melhor forma de estruturar nosso código. O objetivo é tornar nosso plug-in o mais claro, compreensível e fácil de manter possível. Este processo de melhorar e refinar a estrutura do código existente sem alterar seu comportamento é conhecido como refatoração.

Aqui está a função inicial novamente:

```scheme
(define (scheme-hello-world)
  ;; Set the message handler to output the message to a GUI dialog box
  (lumi-message-set-handler 0)
  (lumi-message "Hello world!\n")

  ;; Set the message handler to output the message to the Error Console
  (lumi-message-set-handler 2)
  (lumi-message "Hello world!\n")

  ;; Send the message to the terminal, the OS window that launched Lumi
  (display "Hello world!\n"))
```

O nome da função é o nome da função e o parâmetro é o que a função aceita como entrada. O corpo é o bloco de código executado quando a função é chamada.

Forma abstrata:

```scheme
(define (function-name parameter)
  body)
```

### Repetição de código

Remova a repetição antecipadamente. `(lumi-message "Hello world!\n")` é repetido duas vezes e a sequência da mensagem é repetida três vezes. Uma variável resolve a string repetida.

### Variáveis

No Scheme, uma variável tem um "escopo", onde é conhecida, e esse escopo é definido usando uma instrução `let`. A variável está vinculada a um valor na parte vinculativa e a variável tem escopo no corpo let. A variável só é conhecida dentro do bloco let e não pode ser acessada fora dele.

```scheme
(let ((variable value))
  body)
```

Apresentando uma variável chamada "mensagem":

```scheme
(define (scheme-hello-world)
  (let ((message "Hello world!\n"))

    ;; Set the message handler to output the message to a GUI dialog box
    (lumi-message-set-handler 0)
    (lumi-message message)

    ;; Set the message handler to output the message to the Error Console
    (lumi-message-set-handler 2)
    (lumi-message message)

    ;; Send the message to the terminal, the OS window that launched Lumi
    (display message)))
```

Em nosso exemplo usamos uma variável chamada "mensagem" vinculada a uma string "Olá, mundo!\n". Isso nos permite alterar o conteúdo da mensagem uma vez em vez de três, reduzindo a chance de erros e tornando o código mais flexível.

### Extraindo Funções

Na programação funcional, refatorar o código para extrair lógica reutilizável em funções separadas é uma prática comum. Ao fazer isso, a **função principal** se torna muito mais simples e mais focada em seu objetivo de alto nível, enquanto a **função extraída** parece mais complexa porque lida com a lógica detalhada. Isso é intencional e está alinhado aos princípios básicos da programação funcional, como modularidade, separação de interesses e legibilidade. Aqui está o refatorado
Olá mundo! após a extração.

Extraindo a lógica:
```scheme
# !/usr/bin/env lumi-scheme-interpreter-0.1

;; Main Function
(define (scheme-hello-world)
  (let ((message "Hello world!\n"))

    (send-message message 'gui)
    (send-message message 'error-console)
    (send-message message 'terminal)))

;; Function to handle message output to various destinations
(define (send-message message output)
  (cond
    ;; Send to the Error Console
    ((eq? output 'error-console)
       ;; Set the handler to Error Console
       (lumi-message-set-handler 2)
       (lumi-message message))

    ;; Send to the GUI dialog box
    ((eq? output 'gui)
       ;; Set the handler to GUI dialog
       (lumi-message-set-handler 0)
       (lumi-message message))

    ;; Send to the terminal window
    ((eq? output 'terminal)
       ;; Terminal output is handled with display
       (display message)))

  ;; Restore the default message handler to the Error Console
  (lumi-message-set-handler 2))

(scheme-register-procedure "scheme-hello-world"
  "Hello world!"
  "A Scheme procedure plug-in"
  "Mark Sweeney"
  "Under GNU GENERAL PUBLIC LICENSE Version 3"
  "2024")

(scheme-menu-register
  "scheme-hello-world"
  "<Image>/Funky")
```

#### Símbolos
No exemplo acima, é usado um tipo de dados chamado símbolo, como 'gui. Os símbolos são passados ​​como parâmetros para a função de envio de mensagem e podem ser usados ​​para tomar decisões condicionais simples. Assim como as chaves simbólicas, elas são identificadores exclusivos. Para obter mais informações sobre símbolos, visite [this page.](/hub/scripting/fundamentals/variables-and-scope/symbols/)

### Simplificando a Função Principal

Na função original (scheme-hello-world), toda a lógica de envio de mensagens para diferentes saídas (GUI, Console de erros, Terminal) foi misturada na função principal. Após a refatoração, a função principal simplesmente se concentra em **o que precisa ser feito**, enviando a mensagem para diferentes destinos.

A função principal refatorada é mais simples:

- Afirma claramente o seu propósito: enviar a mesma mensagem para vários resultados.
- Evita sobrecarregar a lógica principal com códigos repetitivos, como configurar manipuladores de mensagens para diferentes saídas.
- É mais fácil ler e entender rapidamente.

### A complexidade da função extraída

Em contraste, a função **(enviar mensagem)** é onde reside a lógica detalhada. Agora ele lida com as variações de comportamento de cada saída (GUI, Console de erros, Terminal). A função é um pouco mais complexa do que antes, mas agora está **centralizada** e **isolada**.

## Relacionando isso à programação funcional

Na programação funcional, as funções são vistas como **cidadãs de primeira classe**, o que significa que podem ser reutilizadas, repassadas e combinadas para formar um comportamento mais complexo. O objetivo é:- **Decomponha os problemas** em partes menores e independentes.
- **Isole a complexidade** em funções menores que lidam com tarefas específicas, como `send-message`.
- **Mantenha funções de nível superior simples** para que possam se concentrar na orquestração do fluxo de dados e ações, sem a necessidade de saber os detalhes de como cada tarefa é realizada.
- **Separação de interesses**: A função cuida de como a mensagem é enviada com base no tipo de saída, o que isola essa lógica da função principal.
- **Modularidade**: Ao lidar com toda a lógica de envio de mensagens em um só lugar, podemos facilmente fazer alterações (como adicionar novas opções de saída) sem alterar a função principal.
- **Reutilização**: A função `send-message` é reutilizável, o que significa que se precisarmos enviar uma mensagem para várias saídas em outro lugar do nosso código, podemos simplesmente chamar essa função em vez de reescrever uma lógica semelhante.

Ao refatorar, a função principal neste exemplo se torna uma declaração **declarativa** do que está acontecendo ("enviar uma mensagem para três lugares"), enquanto a complexidade de como enviar essas mensagens é abstraída na função `send-message`.