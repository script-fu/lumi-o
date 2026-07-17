---
title: "Valores de retorno"
type: docs
weight: 8
translation_provenance: ai-reviewed
translation_source_sha256: 586ad49d823eb3fa85ff606b73c3f95e3fd3efb8bd9a0c9482e2c3e21f953de9
translation_lock: true
url: "hub/scripting/tutorials/First Step/return-values"
---
Os valores de retorno são importantes porque permitem controlar o fluxo sem estado extra. Em Scheme, a última expressão avaliada torna-se o valor de retorno.

Esta página usa os auxiliares de validação do exemplo de mensagens para mostrar como os valores de retorno explícitos facilitam a composição do código.

### O que é um valor de retorno?

Em Scheme, o valor de retorno de uma função é determinado pela última expressão avaliada pela função. Isso significa que qualquer valor avaliado pela última linha de código da função será retornado como resultado da função. Se nenhum valor for retornado explicitamente, a função retornará `#f` (falso) ou `undefined`.

Vamos revisitar a função de validação, (is-valid-string?)

```scheme
;; Propósito: Valida que a mensagem é uma string não vazia
(define (is-valid-string? message)
 ;; Verificar se a mensagem é uma string não vazia
 (if (or (not (string? message)) (string=? message ""))
 (error "Message must be a non-empty string")))
```

Nesta função, se a mensagem for inválida, um erro será gerado. No entanto, se a mensagem for válida, nenhum valor de retorno explícito será fornecido e a função retornará `#f` por padrão.

### Tornando os valores de retorno explícitos

Podemos melhorar isso tornando o valor de retorno mais explícito. Por exemplo, poderíamos retornar `#t` (true) se a mensagem for válida:

```scheme
;; Propósito: Valida que a mensagem é enviada para uma saída válida
(define (is-valid-output-display? output)
 ;; Verificar se a saída é um dos destinos de exibição esperados
 (if (not (member output '(dialog-box status-bar error-console terminal)))
 (error "Invalid output destination: " output)
 #t))
```

Nesta versão, a função retornará `#t` quando a mensagem for válida, fornecendo um resultado claro. Isto permite que a função seja usada de forma mais flexível em outros contextos onde um resultado booleano é necessário.

### Usando valores de retorno de forma eficaz

Ao decidir o que as funções retornam, podemos torná-las mais previsíveis e úteis. Retornar valores como `#t`, `#f` ou um resultado específico nos dá mais controle sobre como a função interage com o restante do código. Por exemplo, pode usar o valor de retorno para tomar decisões adicionais na função de chamada ou passá-lo como argumento para outra função.

Aqui está um exemplo simples de uso de um valor de retorno para controlar o fluxo da lógica:

```scheme
;; Propósito: Envia uma mensagem para o destino de saída apropriado
(define (send-message message output)
 (if (is-valid-output-display? output)
 (cond
 ((eq? output 'error-console) (send-to-error-console message))
 ((eq? output 'dialog-box) (send-to-dialog-box message))
 ((eq? output 'status-bar) (send-to-status-bar message))
 ((eq? output 'terminal) (send-to-terminal message)))))
```

Neste caso, (send-message) depende do valor de retorno de (is-valid-output-display?) para decidir se deve continuar.
A instrução condicional `cond` será ignorada se o primeiro teste falhar. Além disso, observe como ele é lido de forma bastante natural, se a exibição da saída for válida?

## Lógica da instrução `if` em Scheme

Antes do exemplo da biblioteca refatorada, aqui está uma rápida revisão da lógica condicional. Scheme usa `if` para escolher entre dois caminhos.

Aqui está uma forma simples de uma instrução `if`:

```scheme
(if (conditional test)
 do if true
 do if false)
```

Essa estrutura verifica a condição e, se a condição for verdadeira, executa a primeira ação. Se a condição for falsa, ele executa a segunda ação.

Nos casos em que precisa realizar múltiplas ações quando a condição é verdadeira ou falsa, pode usar `begin` para agrupá-las:

```scheme
(if (conditional test)
 (begin
 do if true)
 (begin
 do if false))
```

Isso permite lidar com situações mais complexas, onde múltiplas expressões ou instruções precisam ser executadas dependendo do resultado do teste condicional.

Ok, aqui está o código da biblioteca com valores de retorno incorporados e usados para controlar o processo de execução.

### Refatorado com valores de retorno

```scheme
;; Propósito: Envia uma mensagem para a barra de status, retorna #t se bem-sucedido
(define (send-to-status-bar message)
 (if (is-valid-string? message)
 (begin
 (lumi-message-set-handler MESSAGE-BOX)
 (lumi-message message)
 (lumi-message-set-handler ERROR-CONSOLE)
 #t)
 #f))

;; Propósito: Envia uma mensagem para a caixa de diálogo, retorna #t se bem-sucedido
(define (send-to-dialog-box message)
 (if (is-valid-string? message)
 (begin
 (lumi-message-set-handler MESSAGE-BOX)
 (lumi-message (string-append message "\n"))
 (lumi-message-set-handler ERROR-CONSOLE)
 #t)
 #f))

;; Propósito: Envia uma mensagem para a consola de erros, retorna #t se bem-sucedido
(define (send-to-error-console message)
 (if (is-valid-string? message)
 (begin
 (lumi-message-set-handler ERROR-CONSOLE)
 (lumi-message message)
 #t)
 #f))

;; Propósito: Envia uma mensagem para o terminal, retorna #t se bem-sucedido
(define (send-to-terminal message)
 (if (is-valid-string? message)
 (begin
 (display message)
 (lumi-message-set-handler ERROR-CONSOLE)
 #t)
 #f))

;; Propósito: Envia uma mensagem para a saída apropriada, retorna #t se bem-sucedido
(define (send-message message output)
 (if (is-valid-string-output? output)
 (cond
 ((eq? output 'error-console) (send-to-error-console message))
 ((eq? output 'dialog-box) (send-to-dialog-box message))
 ((eq? output 'status-bar) (send-to-status-bar message))
 ((eq? output 'terminal) (send-to-terminal message)))
 #f))

;; Propósito: Valida que a mensagem é uma string não vazia, retorna #t se válida
(define (is-valid-string? message)
 (if (or (not (string? message)) (string=? message ""))
 (begin
 (error "Message must be a non-empty string")
 #f)
 #t))

;; Propósito: Valida que a saída é um destino válido, retorna #t se válida
(define (is-valid-string-output? output)
 (if (not (member output '(dialog-box status-bar error-console terminal)))
 (begin
 (error "Invalid output destination: " output)
 #f)
 #t))
```

## Conclusão

Os valores de retorno são uma parte fundamental para tornar as funções flexíveis e reutilizáveis. Ao decidir cuidadosamente o que cada função deve retornar, podemos garantir que as funções interajam bem entre si e forneçam informações úteis para o restante do código. Seja retornando `#t` ou `#f`, ou algo mais específico, os valores de retorno nos dão uma forma de controlar o fluxo de os programas e lidar com vários resultados.