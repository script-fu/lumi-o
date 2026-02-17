---
title: "Validação"
type: docs
weight: 4
---
Ao construir plug-ins robustos, é importante garantir que nossas funções tratem erros normalmente e funcionem conforme o esperado, mesmo em casos de uso indevido ou entradas inesperadas. A validação ajuda a proteger a integridade da função e evitar falhas ou comportamento não intencional.

Vejamos como podemos melhorar a função `send-message` adicionando verificações de validação para garantir que ela lide com as entradas corretamente.

### Validar entradas

Antes de enviar uma mensagem, devemos garantir que o argumento `output` passado para a função `send-message` é válido. Podemos adicionar uma verificação para confirmar se o destino de saída é um dos valores esperados (gui, console de erro ou terminal).

Exemplo:

```scheme
(define (send-message message output)
  ;; Validate the output argument
  (if (not (member output '(gui error-console terminal)))
    (error "Invalid output destination: " output)
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
         (display message))))

  ;; Restore the default message handler to the Error Console
  (lumi-message-set-handler 2))
```

Neste exemplo, usamos `member` para verificar se o argumento `output` é válido. Caso contrário, a função gera um erro com uma mensagem clara, evitando que valores inválidos causem problemas.

### Lidar com mensagens vazias

Também é útil garantir que o argumento `message` seja válido. Por exemplo, se uma string vazia ou #f (falso) for passada como mensagem, a função deverá lidar com isso normalmente.

Exemplo de tratamento de uma mensagem vazia:

```scheme
(define (send-message message output)
  ;; Check if the message is empty
  (if (or (not message) (string=? message ""))
    (error "Message cannot be empty")
    (cond
      ((eq? output 'error-console)
         (lumi-message-set-handler 2)
         (lumi-message message))
      ((eq? output 'gui)
         (lumi-message-set-handler 0)
         (lumi-message message))
      ((eq? output 'terminal)
         (display message))))

  (lumi-message-set-handler 2))
```

Essa abordagem garante que a função sempre receba entradas válidas, melhorando sua confiabilidade e evitando comportamentos inesperados.

### Exemplo de validação combinada

```scheme
;; Function to handle message output to various destinations
(define (send-message message output)

  ;; Validate the message and output arguments
  (if (or (not (string? message)) (string=? message ""))
    (error "Message must be a non-empty string")
    (if (not (member output '(gui error-console terminal)))
      (error "Invalid output destination: " output)
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
           (display message)))))

  ;; Restore the default message handler to the Error Console
  (lumi-message-set-handler 2))
```

Nesta versão:
- A função verifica primeiro se `message` está vazio ou inválido. Se a mensagem for válida, ele passa a verificar se `output` é um dos valores aceitos (`gui`, `error-console` ou `terminal`).
- Se ambas as verificações forem aprovadas, a mensagem será enviada para a saída apropriada. Caso contrário, uma mensagem de erro será gerada com uma explicação clara.
- Uma verificação adicional é feita para garantir que a mensagem também seja uma string.

Esta função de validação combinada mantém o código mais limpo e garante que ambas as entradas sejam validadas antes de qualquer ação ser tomada, tornando a função mais robusta. Observe que também estamos construindo um sistema de mensagens de depuração. Quando o
o código falha, obtemos um motivo, um motivo pelo qual nós mesmos escrevemos.

```
Execution error for 'Hello loaded!':
Error: Message must be a non-empty string
```

```
Execution error for 'Hello loaded!':
Error: Invalid output destination:  gu
```