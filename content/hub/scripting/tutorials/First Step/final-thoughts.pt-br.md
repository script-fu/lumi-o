---
title: "Considerações Finais"
type: docs
weight: 10
---
Agora você tem um plug-in de procedimento funcional e uma pequena biblioteca auxiliar. Esta série apresentou os padrões básicos que você usará na maioria dos scripts Lumi:

- Funções: Os blocos de construção dos nossos plug-ins.
- Refatoração: Melhorando a estrutura do código mantendo a funcionalidade.
- Bibliotecas de Código: Centralizando funções reutilizáveis ​​para manter nosso código limpo e modular.
- Técnicas de validação: Garantir que as entradas sejam válidas antes de executar nossa lógica central.

Você também viu os fundamentos do uso do Git para rastrear alterações e manter uma estrutura de projeto limpa. Esse fluxo de trabalho facilita a iteração sem perder versões de trabalho.

Aqui está a versão final do nosso código de plug-in principal:

```scheme
# !/usr/bin/env lumi-scheme-interpreter-0.1

(load "/home/your-name/code/path/to/repo/funky-library/messages.scm")
(load "/path/to/your/library/messages.scm")

(define (scheme-hello-world)
  (let ((message "Hello world!"))
    (send-message message 'status-bar)
    (send-message message 'dialog-box)
    (send-message message 'error-console)
    (send-message message 'terminal)))

(scheme-register-procedure "scheme-hello-world"
  "Hello world!"
  "A Scheme procedure plug-in example"
  "Mark Sweeney"
  "Under GNU GENERAL PUBLIC LICENSE Version 3"
  "2024")

(scheme-menu-register
  "scheme-hello-world"
  "<Image>/Scheme")
```

Código da biblioteca:

```scheme
;; Purpose: Sends a message to the status bar, returns #t if successful
(define (send-to-status-bar message)
  (if (is-valid-string? message)
    (begin
      (lumi-message-set-handler MESSAGE-BOX)
      (lumi-message message)
      (lumi-message-set-handler ERROR-CONSOLE)
      #t)
    #f))

;; Purpose: Sends a message to the dialog box, returns #t if successful
(define (send-to-dialog-box message)
  (if (is-valid-string? message)
    (begin
      (lumi-message-set-handler MESSAGE-BOX)
      (lumi-message (string-append message "\n"))
      (lumi-message-set-handler ERROR-CONSOLE)
      #t)
    #f))

;; Purpose: Sends a message to the error console, returns #t if successful
(define (send-to-error-console message)
  (if (is-valid-string? message)
    (begin
      (lumi-message-set-handler ERROR-CONSOLE)
      (lumi-message message)
      #t)
    #f))

;; Purpose: Sends a message to the terminal, returns #t if successful
(define (send-to-terminal message)
  (if (is-valid-string? message)
    (begin
      (display message)
      (lumi-message-set-handler ERROR-CONSOLE)
      #t)
    #f))

;; Purpose: Dispatches a message to the appropriate output, returns #t if successful
(define (send-message message output)
  (if (is-valid-string-output? output)
    (cond
      ((eq? output 'error-console) (send-to-error-console message))
      ((eq? output 'dialog-box) (send-to-dialog-box message))
      ((eq? output 'status-bar) (send-to-status-bar message))
      ((eq? output 'terminal) (send-to-terminal message)))
    #f))

;; Purpose: Validates that the message is a non-empty string, returns #t if valid
(define (is-valid-string? message)
  (if (or (not (string? message)) (string=? message ""))
    (begin
      (error "Message must be a non-empty string")
      #f)
    #t))

;; Purpose: Validates that the output is a valid destination, returns #t if valid
(define (is-valid-string-output? output)
  (if (not (member output '(dialog-box status-bar error-console terminal)))
    (begin
      (error "Invalid output destination: " output)
      #f)
    #t))
```

## Conclusão

Ao refatorar os auxiliares de mensagens em uma pequena biblioteca, o plug-in permanece focado na intenção e a biblioteca contém os detalhes da implementação. A validação e o roteamento consistente de mensagens mantêm as falhas previsíveis.

```scheme
(message "Hello world!")
(send-message message 'status-bar)
(send-message message 'dialog-box)
(send-message message 'error-console)
(send-message message 'terminal)
```

Próximas etapas:

- Mova auxiliares reutilizáveis para um arquivo de biblioteca dedicado.
- Mantenha os plug-ins pequenos e nomeie os procedimentos de acordo com o que eles fazem.
- Adicione validação nos limites (entradas, caminhos de arquivos, opções de menu).

Mantenha o resultado final como dois arquivos no seu repositório de plug-ins:

- `hello-world/hello-world.scm`
-`funky-library/messages.scm`