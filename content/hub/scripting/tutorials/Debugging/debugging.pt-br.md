---
title: "Depuração"
type: docs
weight: 5
---
Em scripts, nenhuma função é infalível. Mesmo os comandos mais confiáveis ​​podem falhar quando confrontados com entradas ou condições inesperadas. Para nos proteger contra isso, podemos implementar um sistema de depuração personalizado e adotar técnicas de programação defensivas. Ao agrupar funções padrão com mecanismos de tratamento de erros e fornecer feedback informativo, podemos tornar nossos scripts mais robustos e fáceis de solucionar.

Uma parte importante desta estratégia é usar um sinalizador de depuração global para controlar a saída detalhada, permitindo-nos ativar informações detalhadas de depuração quando necessário, mantendo a saída limpa durante a execução normal.

## Sinalizador de depuração global

Um sinalizador de depuração global é uma maneira simples, mas eficaz, de controlar o nível de saída de informações durante a execução do script. Quando ativado, ele fornece mensagens de depuração detalhadas que podem ser inestimáveis ​​para rastrear problemas. Quando desabilitado, mantém a saída concisa para uso em produção.

```scheme
;; Purpose: Global flag to control debug output.
(define debug #f)
```

Por padrão, a depuração está desativada. Para ativar a saída detalhada durante o desenvolvimento, basta definir o sinalizador como `#t`:

```scheme
;; Purpose: Global flag to control debug output.
(define debug #t)
```

Também podemos ativar ou desativar temporariamente a depuração para seções específicas do código usando funções auxiliares.

### Controle de depuração local

Para um controle mais preciso, podemos ativar ou desativar a depuração em partes específicas do script usando funções auxiliares.

```scheme
;; Purpose: Turn off debug mode for a section of code.
(define (debug-off)
  (set! debug #f))

;; Purpose: Turn on debug mode for a section of code.
(define (debug-on)
  (set! debug #t))
```

Isso nos permite controlar a depuração dinamicamente:

```scheme
(debug-on)  ;; Enable verbose output

;; Some script logic here

(debug-off) ;; Disable verbose output
```

## Depurar sistema de mensagens

Para lidar com eficiência com a saída de depuração no Scheme, usamos uma abordagem estruturada envolvendo várias funções auxiliares. Essas funções garantem que as mensagens de depuração e aviso sejam claras, legíveis e fáceis de manter.

### Visão geral do sistema de mensagens de depuração

Nosso sistema de mensagens de depuração consiste nos seguintes componentes:

1. `debug-message` – Exibe mensagens de depuração quando a depuração está habilitada.
2. `serialize-item` – Converte vários tipos de dados do esquema em uma representação de string.
3. `concat` – Concatena vários itens em uma única string.
4. `list->string` – Formata uma lista em uma string legível.
5. `message` – Exibe a saída no console de mensagens do Lumi.
6. `warning-message` – Exibe mensagens de aviso quando os avisos estão habilitados.

Cada função desempenha uma função na formatação e exibição de mensagens estruturadas.

---

### Função de mensagem de depuração

A função `debug-message` é o método principal para exibir a saída de depuração. Ele garante que as mensagens sejam mostradas apenas quando a depuração estiver habilitada.

```scheme
;; Purpose: Display a debug message.
(define (debug-message . items)
  (when debug (message "> " (apply concat items))))
```

- A condição `when debug` garante que as mensagens apareçam apenas quando a depuração estiver habilitada.
- As mensagens são prefixadas com `"> "` para maior clareza.
- A função usa `concat` para formatar o conteúdo da mensagem.
- Por fim, chama `message` para enviar a saída para o console de mensagens do Lumi.

Exemplo de uso:

```scheme
;; Purpose: Returns the item's tree position or #f if the item is invalid
(define (get-item-tree-position image item)
  (if (item-is-valid? item)
    (let ((position (list->item (lumi-image-get-item-position image item))))
      (debug-message "item : " (item-get-name item) " has tree position : " position)
      position)
    #f))
```

Com a depuração habilitada, a saída pode ser:

```scheme
> item: background-layer has tree position : 3
```

### Serializando dados para mensagens de depuração

As mensagens podem conter diferentes tipos de dados, como listas, vetores e números. Para garantir que estejam formatados corretamente, usamos `serialize-item`.

```scheme
;; Purpose: Converts various Scheme data types (lists, vectors, pairs, etc.)
;;          into a string representation.
(define (serialize-item item)
  (cond
    ((and (list? item) (null? item)) "\"\"")          ; Empty list
    ((and (string? item) (string=? item "")) "\"\"")  ; Empty string
    ((list? item) (list->string item))                ; Nested list
    ((vector? item)                                   ; Handle vectors
     (string-append "#("
                    (string-join (map serialize-item (vector->list item)) " ")
                    ")"))
    ((pair? item)                                     ; Handle pairs
     (string-append "("
                    (serialize-item (car item))
                    " . "
                    (serialize-item (cdr item))
                    ")"))
    ((number? item) (number->string item))            ; Numbers
    ((symbol? item) (symbol->string item))            ; Symbols
    ((boolean? item) (if item "#t" "#f"))             ; Booleans
    ((string? item) item)                             ; Strings
    (else (warning-message "serialize-item: Unsupported item type!" item))))
```

Exemplo de uso:

```scheme
(serialize-item '(1 2 3))
```

Saída:

```scheme
list:
1
2
3
```

### Concatenação para Mensagens

Para mesclar vários componentes de mensagem em uma única string, usamos `concat`.

```scheme
;; Purpose: Concatenate multiple items into a single string.
(define (concat . items)
  (apply string-append (map serialize-item items)))
```

Exemplo de uso:

```scheme
(concat "Image size: " 1920 "x" 1080)
```

### Formatando listas como strings

A função `list->string` converte uma lista em uma string formatada.

```scheme
;; Purpose: Convert a list of items into a readable string.
(define (list->string list)
  (if (list? list)
      (string-append "list: \n" (string-join (map serialize-item list) "\n"))
      (warning-message "list->string: Input is not a list!")))
```

### Mensagens de avisoA função `warning-message` funciona de forma semelhante a `debug-message`, mas exibe avisos mesmo quando a depuração está desabilitada.

```scheme
;; Purpose: Display a warning message.
(define (warning-message . items)
  (if warning
    (message "Warning: " (apply concat items)))
    #f)
```

- Garante que as mensagens sejam mostradas apenas quando os avisos estão habilitados (o sinalizador `warning` é definido em `common.scm` como `#t`).
- Chama `concat` para formatar o conteúdo da mensagem.
- Usa `message` para enviar saída para Lumi.

## Aprimorando funções padrão

Uma vez implementado um sistema de depuração, podemos aprimorar nossa biblioteca de funções incorporando mensagens detalhadas. Isso fornece informações sobre estados de itens, valores de variáveis ​​e chamadas de função.

Um exemplo comum é `item-is-valid?`, que envolve `lumi-item-id-is-valid` para retornar `#t` ou `#f`. Se `#f` for retornado, podemos acionar um `warning-message` no código de chamada, se a entrada não for um número podemos dar um aviso na função.

```scheme
;; Purpose: Check if an item is valid, returning #t or #f.
;;          Issues a warning if the item is not a number.
(define (item-is-valid? item)
  (if (number? item)
      (= (list->item (lumi-item-id-is-valid item)) 1)
      (begin
        (warning-message "item-is-valid?: Expected a number, but received: " item)
        #f)))
```

## Uso prático

Ao desenvolver plug-ins do Scheme, agrupar funções dessa maneira reduz significativamente o tempo de depuração e garante um código robusto e de fácil manutenção. Com nosso sistema de depuração instalado, podemos gerar um fluxo de depuração estruturado no console de erros com o toque de um botão.

Neste fluxo de depuração, as chamadas de função são marcadas com um asterisco (*), facilitando o rastreamento da execução de scripts e a identificação de falhas, especialmente em plug-ins complexos. Essa visibilidade nos ajuda a entender o fluxo das operações e a diagnosticar comportamentos inesperados com eficiência.

Um wrapper para nossa função de mensagem usar um `*`

```scheme
(define (call . items)
  (when debug (message "* (" (apply concat items) ")")))
```

Exemplo de `call` sendo usado na prática:

```scheme
;; Purpose: Apply the texturing process to the given list of group masks
(define (process-masks groups pattern) (call 'process-masks)
  (for-each
    (lambda (group)
      (let ((mask (add-mask-to-layer group ADD-MASK-WHITE)))
        (message "Process mask : " (item-get-name group))
        (fill-and-adjust-group-mask group mask pattern)
        (lumi-layer-set-opacity group (get 'color-opacity))
        (lumi-item-set-expanded (item-get-parent group) 0)
        (lumi-selection-none (get-image))))
    (ensure-list groups)))
```

Exemplo de um fluxo de depuração conforme um plug-in é executado:

```scheme
> Recording the plug-in settings
* (convert-gui-settings)
> all-masks : 1
> strokes : 1
> color : 1
> plate-layer : 1
> drawables : #(37)
* (filter-list-for-matching-groups)
> all-masks : #t
> sub-groups of group : root
blue
blue_strokes
blue_colour
yellow
yellow_strokes
yellow_colour
gray
gray_strokes
gray_colour
> groups with identifier in name: _colour
blue_colour
yellow_colour
gray_colour
* (filter-list-for-matching-groups)
> all-masks : #t
> sub-groups of group : root
blue
blue_strokes
blue_colour
yellow
yellow_strokes
yellow_colour
gray
gray_strokes
gray_colour
> groups with identifier in name: _strokes
blue_strokes
yellow_strokes
gray_strokes
* (begin-apply-texture)

Start Apply Texture

> color : #t

Texturing color group masks
> color-pattern : 2655
* (process-masks)
Process mask : blue_colour
* (fill-and-adjust-group-mask)
> Fill-and-adjust : blue_colour mask
> using pattern for fill : 2655
* (apply-color-effect)
> color-contrast : 64
> color-levels-gamma : 10
> levels on drawable: blue_colour mask
>   gamma: 8.2
>   low-in: 0.7278  high-in: 0.9222
>   low-out: 0  high-out: 1
> light-opacity : 6
> light-opacity : 6
* (apply-light-effect)
> apply-light-effect opacity : 6
> from layer : light_blue
> edit-copy light_blue
> edit-paste blue_colour mask
> shade-opacity : 60
> shade-opacity : 60
* (apply-light-effect)
> apply-light-effect opacity : 60
> from layer : shad_blue_opa*5
> edit-copy shad_blue_opa*5
> edit-paste blue_colour mask
* (apply-opaque-effect)
> children in : blue_colour
blue_colour
hue_blue
light_blue
shad_blue_opa*5
base_blue
...
...
...
Finished Apply Texture!
```

Esse log estruturado fornece um cronograma claro de chamadas de função e alterações de dados, facilitando significativamente a depuração e a análise de desempenho.

## Conclusão

Ao implementar um sistema de depuração estruturado, criamos scripts mais seguros e de fácil manutenção que oferecem insights em tempo real sobre sua execução.

### Principais conclusões

- **Controlar verbosidade** – Use um sinalizador de depuração global para gerenciar os níveis de saída.
- **Forneça feedback claro** – Envolva funções padrão com mensagens informativas de depuração.
- **Aumente a robustez** – Lide com entradas inesperadas com elegância para evitar erros.
- **Simplifique a solução de problemas** – Mensagens de depuração estruturadas facilitam o diagnóstico e a correção de problemas.

Com essa abordagem, nossos scripts “explicam-se” efetivamente à medida que processam dados, reduzindo a frustração e melhorando a eficiência do fluxo de trabalho. A depuração se torna uma ferramenta proativa em vez de uma tarefa reativa, tornando nosso processo de script mais suave e gratificante.