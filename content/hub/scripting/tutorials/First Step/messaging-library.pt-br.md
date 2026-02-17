---
title: "Biblioteca de mensagens"
type: docs
weight: 6
---
Com o tempo, o que começou como uma função única para enviar mensagens evoluiu para uma coleção de funções relacionadas. Essas funções agora formam a base de uma **Biblioteca de Mensagens**, projetada para lidar com saídas para diferentes destinos, como GUI, Console de Erros e Terminal.

### Por que uma biblioteca de mensagens?

À medida que as nossas necessidades crescem, o tratamento de mensagens através de múltiplas saídas requer uma abordagem mais modular e extensível. Em vez de uma única função fazer tudo, dividimos o processo em componentes reutilizáveis, permitindo maior flexibilidade. Esta biblioteca agora pode ser usada como uma ferramenta de mensagens de uso geral que pode ser emprestada por outros plug-ins ou funções.

### O que a biblioteca de mensagens faz?

A Biblioteca de Mensagens inclui atualmente as seguintes funções:

- **send-to-gui**: Envia mensagens para a caixa de diálogo Lumi GUI.
- **send-to-error-console**: Envia mensagens para o Lumi Error Console.
- **send-to-terminal**: Envia mensagens para a janela do terminal.
- **send-message**: Uma função de despachante que direciona mensagens para a saída apropriada.
- **validate-message**: Garante que a mensagem e a saída sejam válidas antes do envio.

### Expandindo a Biblioteca

A **Biblioteca de Mensagens** pode ser facilmente estendida para suportar saídas adicionais. Por exemplo:

- **enviar para arquivo**: Salva mensagens em um arquivo de log.
- **send-to-logger**: Integração com um sistema de registro externo.
- **enviar para notificação**: exibe mensagens como notificações do sistema.

Seguindo o mesmo padrão de design modular e funções reutilizáveis, esta biblioteca pode se transformar em uma ferramenta abrangente para lidar com todos os tipos de tarefas de mensagens.

## Benefícios de uma biblioteca de mensagens

- **Reutilização**: as funções podem ser reutilizadas em diferentes plug-ins ou projetos.
- **Modularidade**: cada função lida com uma tarefa específica, tornando o código mais fácil de manter e estender.
- **Consistência**: usar as mesmas funções de validação e tratamento de mensagens garante um comportamento consistente em todo o aplicativo.

A **Biblioteca de Mensagens** é o início de uma estrutura mais ampla que pode simplificar a forma como as mensagens são gerenciadas no seu projeto. À medida que a biblioteca cresce, novos plug-ins podem acessá-la facilmente para enviar mensagens para onde quer que precisem.

Podemos ajustar a estrutura do arquivo:

```plaintext
/home/your-username/code/
  ├── script-fu/
      ├── library/
      │     └── send-message.scm -> messaging.scm
      └── plug-ins/
            └── hello-world/
                  └── hello-world.scm
```

E lembre-se de ajustar o `load` no plug-in principal:

```scheme
# !/usr/bin/env lumi-scheme-interpreter-0.1

(load "/home/mark/code/github/script-plugins/funky-library/messaging.scm")

(define (scheme-hello-world)
  (let ((message "Hello world!\n"))
    (send-message message 'gui)
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