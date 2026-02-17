---
title: "Refatorar novamente"
type: docs
weight: 5
---
À medida que a biblioteca auxiliar cresce, fica mais difícil acompanhar rapidamente. Refatore novamente para manter cada função pequena e de propósito único.

### Quebrando a Complexidade

Para tornar a função mais fácil de seguir e manter, divida-a em funções menores e focadas. Comece separando a validação do roteamento de mensagens.

### Crie uma função de validação

Podemos pegar a parte da função que valida os argumentos `message` e `output` e movê-la para uma função separada. Dessa forma, a função principal `send-message` não precisa se preocupar com validação, facilitando o acompanhamento.

```scheme
(define (validate-message message output)
  ;; Check if the message is a non-empty string
  (if (or (not (string? message)) (string=? message ""))
      (error "Message must be a non-empty string"))

  ;; Check if the output is one of the expected destinations
  (if (not (member output '(gui error-console terminal)))
      (error "Invalid output destination: " output)))
```

### Simplifique o envio de mensagens

Agora que a validação foi movida para uma função separada, a função `send-message` pode se concentrar apenas no envio da mensagem. Será muito mais simples, pois trata apenas da tarefa específica de direcionar a mensagem ao destino correto.

```scheme
(define (send-message message output)
  ;; Call the validation function before proceeding
  (validate-message message output)

  (cond
    ;; Send to the Error Console
    ((eq? output 'error-console)
       (lumi-message-set-handler 2)
       (lumi-message message))

    ;; Send to the GUI dialog box
    ((eq? output 'gui)
       (lumi-message-set-handler 0)
       (lumi-message message))

    ;; Send to the terminal window
    ((eq? output 'terminal)
       (display message)))

  ;; Restore the default message handler to the Error Console
  (lumi-message-set-handler 2))
```

### Dividindo ainda mais: separe cada manipulador de saída

Cada tipo de saída de mensagem (GUI, Console de Erros, Terminal) pode ser movido para sua própria função. Isso permite testes, modificações e extensões potenciais mais fáceis no futuro.

```scheme
(define (send-to-gui message)
  (lumi-message-set-handler 0)
  (lumi-message message))

(define (send-to-error-console message)
  (lumi-message-set-handler 2)
  (lumi-message message))

(define (send-to-terminal message)
  (display message))

(define (send-message message output)
  ;; Send to the appropriate output
  (cond
    ((eq? output 'error-console) (send-to-error-console message))
    ((eq? output 'gui) (send-to-gui message))
    ((eq? output 'terminal) (send-to-terminal message)))

  ;; Restore the default message handler to the Error Console
  (lumi-message-set-handler 2))
```

### Reutilizando validação em cada função de envio

Como a validação é uma parte importante para garantir que a mensagem e a saída estejam corretas, faz sentido que cada função `send-*` execute sua própria validação. Isso garante que não importa qual saída seja chamada, sempre verificaremos as entradas primeiro.

```scheme
(define (send-to-gui message)
  ;; Validate the message before proceeding
  (validate-message message 'gui)
  (lumi-message-set-handler 0)
  (lumi-message message))

(define (send-to-error-console message)
  ;; Validate the message before proceeding
  (validate-message message 'error-console)
  (lumi-message-set-handler 2)
  (lumi-message message))

(define (send-to-terminal message)
  ;; Validate the message before proceeding
  (validate-message message 'terminal)
  (display message))
```

Veja que removemos a validação da função de envio de mensagem e transferimos a responsabilidade para cada função de saída individual. Essa mudança garante que cada destino (GUI, Console de Erros, Terminal) lide com sua própria validação, agilizando a função de envio de mensagem e mantendo a lógica de validação mais próxima de onde é necessária.

Essa abordagem pode simplificar a função de envio de mensagem, tornando-a um _dispatcher_, ao mesmo tempo em que garante que cada função enviar para * valide a mensagem corretamente antes do processamento.

Ao mover a validação para cada função send-to-*, nós as tornamos reutilizáveis ​​como funções independentes. Isso significa que podemos chamar qualquer uma das funções send-to-gui, send-to-error-console ou send-to-terminal diretamente, sem depender da função send-message dispatcher. Cada uma dessas funções agora lida totalmente com sua própria lógica e pode ser usada de forma independente em outras partes do código ou em outros plug-ins, tornando seu código mais modular e flexível.

## Benefícios da Refatoração

- **Separação clara de preocupações**: cada função agora lida com apenas uma responsabilidade, tornando o código mais fácil de entender.
- **Extensibilidade**: Adicionar novos tipos de saída é simples. Você simplesmente define uma nova função como `send-to-file` ou `send-to-logger` e, em seguida, adiciona um caso na instrução `cond`.
- **Reutilização**: cada uma dessas funções de manipulação de saída pode ser reutilizada em outro lugar do seu projeto ou compartilhada entre vários plug-ins.
- **Consistência**: Ao reutilizar a função de validação em cada função `send-to-*`, você garante que todas as saídas sejam devidamente validadas, tornando o código mais robusto.

Uma versão refatorada da biblioteca:

```scheme
;; Purpose: Sends a message to the GUI dialog box
(define (send-to-gui message)
  ;; Validate the message before proceeding
  (validate-message message 'gui)
  (lumi-message-set-handler 0)
  (lumi-message message))

;; Purpose: Sends a message to the Error Console
(define (send-to-error-console message)
  ;; Validate the message before proceeding
  (validate-message message 'error-console)
  (lumi-message-set-handler 2)
  (lumi-message message))

;; Purpose: Sends a message to the terminal window
(define (send-to-terminal message)
  ;; Validate the message before proceeding
  (validate-message message 'terminal)
  (display message))

;; Purpose: Dispatches a message to the appropriate output destination
(define (send-message message output)
  (cond
    ((eq? output 'error-console) (send-to-error-console message))
    ((eq? output 'gui) (send-to-gui message))
    ((eq? output 'terminal) (send-to-terminal message)))

  ;; Restore the default message handler to the Error Console
  (lumi-message-set-handler 2))

;; Purpose: Validates that the message is a non-empty string and the output is valid
(define (validate-message message output)
  ;; Check if the message is a non-empty string
  (if (or (not (string? message)) (string=? message ""))
      (error "Message must be a non-empty string"))

  ;; Check if the output is one of the expected destinations
  (if (not (member output '(gui error-console terminal)))
      (error "Invalid output destination: " output)))
```

Isso é tudo que podemos fazer? Não! há mais a ser feito, por favor continue lendo.