---
title: "Refatorar novamente"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 4563817b27aa107aa948c9bb7fb53f358c663dfbc6f070c4a4b725b0d1d600f0
---
À medida que a biblioteca auxiliar cresce, fica mais difícil acompanhar rapidamente. Refatore novamente para manter cada função pequena e de propósito único.

### Quebrando a Complexidade

Para tornar a função mais fácil de seguir e manter, divida-a em funções menores e focadas. Comece separando a validação do roteamento de mensagens.

### Crie uma função de validação

Podemos pegar a parte da função que valida os argumentos `message` e `output` e movê-la para uma função separada. Dessa forma, a função principal `send-message` não precisa se preocupar com validação, facilitando o acompanhamento.

```scheme
(define (validate-message message output)
  ;; Verificar se a mensagem é uma string não vazia
  (if (or (not (string? message)) (string=? message ""))
      (error "Message must be a non-empty string"))

  ;; Verificar se a saída é um dos destinos esperados
  (if (not (member output '(gui error-console terminal)))
      (error "Invalid output destination: " output)))
```

### Simplifique o envio de mensagens

Agora que a validação foi movida para uma função separada, a função `send-message` pode se concentrar apenas no envio da mensagem. Será muito mais simples, pois trata apenas da tarefa específica de direcionar a mensagem ao destino correto.

```scheme
(define (send-message message output)
  ;; Chamar a função de validação antes de prosseguir
  (validate-message message output)

  (cond
    ;; Enviar para a Message console
    ((eq? output 'error-console)
       (lumi-message-set-handler 2)
       (lumi-message message))

    ;; Enviar para a caixa de diálogo GUI
    ((eq? output 'gui)
       (lumi-message-set-handler 0)
       (lumi-message message))

    ;; Enviar para a janela do terminal
    ((eq? output 'terminal)
       (display message)))

  ;; Restaurar o manipulador de mensagens padrão para a Message console
  (lumi-message-set-handler 2))
```

### Dividindo ainda mais: separe cada manipulador de saída

Cada tipo de saída de mensagem (GUI, console de mensagens, Terminal) pode ser movido para sua própria função. Isso permite testes, modificações e extensões potenciais mais fáceis no futuro.

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
  ;; Enviar para a saída apropriada
  (cond
    ((eq? output 'error-console) (send-to-error-console message))
    ((eq? output 'gui) (send-to-gui message))
    ((eq? output 'terminal) (send-to-terminal message)))

  ;; Restaurar o manipulador de mensagens padrão para a Message console
  (lumi-message-set-handler 2))
```

### Reutilizando validação em cada função de envio

Como a validação é uma parte importante para garantir que a mensagem e a saída estejam corretas, faz sentido que cada função `send-*` execute sua própria validação. Isso garante que não importa qual saída seja chamada, sempre verificaremos as entradas primeiro.

```scheme
(define (send-to-gui message)
  ;; Validar a mensagem antes de prosseguir
  (validate-message message 'gui)
  (lumi-message-set-handler 0)
  (lumi-message message))

(define (send-to-error-console message)
  ;; Validar a mensagem antes de prosseguir
  (validate-message message 'error-console)
  (lumi-message-set-handler 2)
  (lumi-message message))

(define (send-to-terminal message)
  ;; Validar a mensagem antes de prosseguir
  (validate-message message 'terminal)
  (display message))
```

Veja que removemos a validação da função de envio de mensagem e transferimos a responsabilidade para cada função de saída individual. Essa mudança garante que cada destino (GUI, console de mensagens, Terminal) lide com sua própria validação, agilizando a função de envio de mensagem e mantendo a lógica de validação mais próxima de onde é necessária.

Essa abordagem pode simplificar a função de envio de mensagem, tornando-a um _dispatcher_, ao mesmo tempo em que garante que cada função enviar para * valide a mensagem corretamente antes do processamento.

Ao mover a validação para cada função send-to-*, nós as tornamos reutilizáveis ​​como funções independentes. Isso significa que podemos chamar qualquer uma das funções send-to-gui, send-to-error-console ou send-to-terminal diretamente, sem depender da função send-message dispatcher. Cada uma dessas funções agora lida totalmente com sua própria lógica e pode ser usada de forma independente em outras partes do código ou em outros plug-ins, tornando seu código mais modular e flexível.

## Benefícios da Refatoração

- **Separação clara de preocupações**: cada função agora lida com apenas uma responsabilidade, tornando o código mais fácil de entender.
- **Extensibilidade**: Adicionar novos tipos de saída é simples. Você simplesmente define uma nova função como `send-to-file` ou `send-to-logger` e, em seguida, adiciona um caso na instrução `cond`.
- **Reutilização**: cada uma dessas funções de manipulação de saída pode ser reutilizada em outro lugar do seu projeto ou compartilhada entre vários plug-ins.
- **Consistência**: Ao reutilizar a função de validação em cada função `send-to-*`, você garante que todas as saídas sejam devidamente validadas, tornando o código mais robusto.

Uma versão refatorada da biblioteca:

```scheme
;; Propósito: Envia uma mensagem para a caixa de diálogo GUI
(define (send-to-gui message)
  ;; Validar a mensagem antes de prosseguir
  (validate-message message 'gui)
  (lumi-message-set-handler 0)
  (lumi-message message))

;; Propósito: Envia uma mensagem para a Message console
(define (send-to-error-console message)
  ;; Validar a mensagem antes de prosseguir
  (validate-message message 'error-console)
  (lumi-message-set-handler 2)
  (lumi-message message))

;; Propósito: Envia uma mensagem para a janela do terminal
(define (send-to-terminal message)
  ;; Validar a mensagem antes de prosseguir
  (validate-message message 'terminal)
  (display message))

;; Propósito: Envia uma mensagem para o destino de saída apropriado
(define (send-message message output)
  (cond
    ((eq? output 'error-console) (send-to-error-console message))
    ((eq? output 'gui) (send-to-gui message))
    ((eq? output 'terminal) (send-to-terminal message)))

  ;; Restaurar o manipulador de mensagens padrão para a Message console
  (lumi-message-set-handler 2))

;; Propósito: Valida que a mensagem é uma string não vazia e que a saída é válida
(define (validate-message message output)
  ;; Verificar se a mensagem é uma string não vazia
  (if (or (not (string? message)) (string=? message ""))
      (error "Message must be a non-empty string"))

  ;; Verificar se a saída é um dos destinos esperados
  (if (not (member output '(gui error-console terminal)))
      (error "Invalid output destination: " output)))
```

Isso é tudo que podemos fazer? Não! há mais a ser feito, por favor continue lendo.