---
title: "Carregando"
type: docs
weight: 3
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 3dd031042d2683ece82da9ee4444cc1818609d9acf5f609bb1a42115c39275d8
---
Assim que uma função auxiliar crescer, mova-a para um pequeno arquivo de biblioteca. Isso mantém o plug-in focado e torna o auxiliar reutilizável em vários plug-ins.

### Make a Library Function

Podemos pegar a função enviar mensagem e criar um novo arquivo com ela como conteúdo. Salve o arquivo em sua pasta repo, não na parte de plugins, talvez próximo ao nível superior;

```plaintext
/home/your-username/code/
  ├── scheme/
      ├── library/
      │     └── send-message.scm
      └── plug-ins/
            └── hello-world/
                  └── hello-world.scm
```

- **scheme/**: Este é o diretório principal para armazenar o código do esquema.
  - **library/**: É aqui que funcionam funções compartilhadas como `send-message.scm`.
  - **plug-ins/**: É aqui que seus plug-ins individuais são armazenados.
    - **hello-world/**: A folder for the specific "Hello World!" plug-in.
      - **hello-world.scm**: The script file for the plug-in.

Exemplo de função de biblioteca send-message.scm

```scheme
;; Função para tratar a saída de mensagens para vários destinos
(define (send-message message output)
  (cond
    ;; Enviar para a Message console
    ((eq? output 'error-console)
       ;; Definir o manipulador para Message console
       (lumi-message-set-handler 2)
       (lumi-message message))

    ;; Enviar para a caixa de diálogo GUI
    ((eq? output 'gui)
       ;; Definir o manipulador para o diálogo GUI
       (lumi-message-set-handler 0)
       (lumi-message message))

    ;; Enviar para a janela do terminal
    ((eq? output 'terminal)
       ;; A saída do terminal é tratada com display
       (display message)))

  ;; Restaurar o manipulador de mensagens padrão para a Message console
  (lumi-message-set-handler 2))
```

### Carregar a função da biblioteca

Podemos carregar essa função de biblioteca com o comando Scheme `load`;

Carregando um arquivo de biblioteca:

```scheme
#!/usr/bin/env lumi-scheme-interpreter-0.1

(load "/home/mark/code/github/script-plugins/funky-library/send-message.scm")

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

Ei! We've now got something simpler and shorter to read, that kind of describes itself without comments. Esta é a conclusão satisfatória da refatoração.