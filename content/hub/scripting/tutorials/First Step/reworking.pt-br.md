---
title: "Retrabalho"
type: docs
weight: 7
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: af1b2f3233ef50936b24aa195d3a7da50529a4fff3109b087be2f861e15496d1
url: "hub/scripting/tutorials/First Step/reworking"
---
Esta etapa corrige um comportamento sutil no exemplo de mensagens.

Estávamos passando a string "Hello world!\n" como mensagem. O "\n" é um tipo especial de caractere, um caractere de "escape". Diz à impressão de saída para iniciar uma nova linha. No Scheme, também forçará uma mensagem enviada à barra de status a aparecer como uma caixa GUI.

O auxiliar `send-to-gui` envia mensagens para uma caixa de diálogo Lumi.

Atualize o conteúdo e os destinos da mensagem para que o exemplo se comporte de forma consistente.

Removendo o caractere de escape e estendendo as funções:
```scheme
#!/usr/bin/env lumi-scheme-interpreter-0.1

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

  ;; Anexar uma quebra de linha para forçar uma caixa de mensagem
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

;; Propósito: Envia uma mensagem para o destino de saída apropriado
(define (send-message message output)
  (is-valid-output-display? output)
  (cond
    ((eq? output 'error-console) (send-to-error-console message))
    ((eq? output 'dialog-box) (send-to-dialog-box message))
    ((eq? output 'status-bar) (send-to-status-bar message))
    ((eq? output 'terminal) (send-to-terminal message))))

;; Propósito: Valida que a mensagem é uma string não vazia
(define (is-valid-string? message)
  ;; Verificar se a mensagem é uma string não vazia
  (if (or (not (string? message)) (string=? message ""))
      (error "Message must be a non-empty string")))

;; Propósito: Valida que a mensagem é enviada para uma saída válida
(define (is-valid-output-display? output)
  ;; Verificar se a saída é um dos destinos de exibição esperados
  (if (not (member output '(dialog-box status-bar error-console terminal)))
      (error "Invalid output destination: " output)))
```

## Conclusão

Ao reformular a nossa biblioteca de mensagens, tornámo-la mais robusta e fiável. Corrigimos o problema oculto com o caractere de nova linha, introduzimos constantes para melhor clareza e expandimos a funcionalidade adicionando suporte para a barra de status e saídas de caixa de diálogo. Além disso, separar a lógica de validação em funções menores e focadas garante que nosso código seja mais fácil de manter e estender no futuro.

Este retrabalho demonstra como pequenas mudanças podem melhorar a estrutura geral e a funcionalidade da nossa biblioteca, abrindo caminho para mais flexibilidade e capacidade de reutilização à medida que o nosso projeto cresce.