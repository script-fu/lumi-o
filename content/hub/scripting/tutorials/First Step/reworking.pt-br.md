---
title: "Retrabalho"
type: docs
weight: 7
---
Esta etapa corrige um comportamento sutil no exemplo de mensagens.

Estávamos passando a string "Hello world!\n" como mensagem. O "\n" é um tipo especial de caractere, um caractere de "escape". Diz à impressão de saída para iniciar uma nova linha. No Scheme, também forçará uma mensagem enviada à barra de status a aparecer como uma caixa GUI.

O auxiliar `send-to-gui` envia mensagens para uma caixa de diálogo Lumi.

Atualize o conteúdo e os destinos da mensagem para que o exemplo se comporte de forma consistente.

Removendo o caractere de escape e estendendo as funções:
```scheme
# !/usr/bin/env lumi-scheme-interpreter-0.1

(load "/path/to/your/messaging.scm")

(define (scheme-hello-world)
  (let ((message "Hello world!"))
    (send-message message 'dialog-box)
    (send-message message 'status-bar)
    (send-message message 'error-console)
    (send-message message 'terminal)))

(scheme-register-procedure "scheme-hello-world"
  "Hello world!"
  "A Scheme procedure plug-in refactored"
  "Mark Sweeney"
  "Under GNU GENERAL PUBLIC LICENSE Version 3"
  "2024")

(scheme-menu-register
  "scheme-hello-world"
  "<Image>/Funky")
```

Substitua os números mágicos pelas constantes fornecidas pelo Lumi (por exemplo, `MESSAGE-BOX` e `ERROR-CONSOLE`).

Em seguida, divida a validação em duas funções para que possa ser reutilizada em vários sites de chamada.

- (is-valid-string?) Para verificar se uma string é uma string e não uma string vazia, dentro de uma função send-to*.
- (is-valid-output-display?) Para verificar se um determinado destino de saída é válido, na função de envio de mensagem.

Retrabalhe a biblioteca:

```scheme
(define (send-to-status-bar message)
  (is-valid-string? message)
  (lumi-message-set-handler MESSAGE-BOX)
  (lumi-message message)
  (lumi-message-set-handler ERROR-CONSOLE))

(define (send-to-dialog-box message)
  (is-valid-string? message)
  (lumi-message-set-handler MESSAGE-BOX)

  ;; Append a newline to force a box the message
  (lumi-message (string-append message "\n"))
  (lumi-message-set-handler ERROR-CONSOLE))

(define (send-to-error-console message)
  (is-valid-string? message)
  (lumi-message-set-handler ERROR-CONSOLE)
  (lumi-message message))

(define (send-to-terminal message)
  (is-valid-string? message)
  (display message)
  (lumi-message-set-handler ERROR-CONSOLE))

;; Purpose: Dispatches a message to the appropriate output destination
(define (send-message message output)
  (is-valid-output-display? output)
  (cond
    ((eq? output 'error-console) (send-to-error-console message))
    ((eq? output 'dialog-box) (send-to-dialog-box message))
    ((eq? output 'status-bar) (send-to-status-bar message))
    ((eq? output 'terminal) (send-to-terminal message))))

;; Purpose: Validates that the message is a non-empty string
(define (is-valid-string? message)
  ;; Check if the message is a non-empty string
  (if (or (not (string? message)) (string=? message ""))
      (error "Message must be a non-empty string")))

;; Purpose: Validates that the message is sent to a valid output
(define (is-valid-output-display? output)
  ;; Check if the output is one of the expected display destinations
  (if (not (member output '(dialog-box status-bar error-console terminal)))
      (error "Invalid output destination: " output)))
```

## Conclusão

Ao reformular a nossa biblioteca de mensagens, tornámo-la mais robusta e fiável. Corrigimos o problema oculto com o caractere de nova linha, introduzimos constantes para melhor clareza e expandimos a funcionalidade adicionando suporte para a barra de status e saídas de caixa de diálogo. Além disso, separar a lógica de validação em funções menores e focadas garante que nosso código seja mais fácil de manter e estender no futuro.

Este retrabalho demonstra como pequenas mudanças podem melhorar a estrutura geral e a funcionalidade da nossa biblioteca, abrindo caminho para mais flexibilidade e capacidade de reutilização à medida que o nosso projeto cresce.