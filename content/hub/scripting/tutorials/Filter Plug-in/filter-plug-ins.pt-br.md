---
title: "O plug-in de filtro"
type: docs
weight: 2
---
Usamos um plug-in _procedure_ para o tutorial [First Step](../../first-step/). Esses tipos de plug-ins funcionam sem a necessidade de uma imagem ou drawable como entrada. Normalmente, usamos um plug-in para alterar uma imagem e seus drawables. Plug-ins como esses são chamados plug-ins de _filtro_.

### O que é um drawable?

Um **drawable** no Lumi refere-se a um elemento de imagem que pode ser desenhado, como uma camada ou canal. Os plug-ins de filtro normalmente operam nesses elementos.

### Um exemplo simples de plug-in de filtro

```scheme
# !/usr/bin/env lumi-scheme-interpreter-0.1

(define (scheme-simple-filter-plug-in image drawables)
  ;; Use a let statement to define a message variable and core code
  (let ((message "hello, world"))
    ;; Display the message in Lumi's error console
    (lumi-message message)
    ;; Invert the colors of the first selected drawable
    (lumi-drawable-invert (vector-ref drawables 0) 1)))

;; Register the plug-in
(scheme-register-filter
  "scheme-simple-filter-plug-in"           ;; Main procedure name
  "Simple Filter Plug-in Demo"             ;; The name as it appears in the Lumi menu
  "Tests a basic Scheme filter plug-in"    ;; Tool-tip description
  "Author Name"                            ;; Give yourself some credit
  "License"                                ;; License
  "Date written"                           ;; Date written
  "*"                                      ;; Indicates this plug-in requires an image
  SF-ONE-OR-MORE-DRAWABLE)                 ;; Requires one or more selected drawables

;; Specify the menu location for the plug-in
(scheme-menu-register
  "scheme-simple-filter-plug-in"
  "<Image>/Plug-in")
```

Copie o texto e salve-o como `simple-filter-plug-in.scm` em uma pasta chamada `simple-filter-plug-in` dentro de uma das pastas de plug-ins do Lumi. Uma pasta de plug-ins do Lumi é _qualquer_ pasta listada em:
 **Lumi > Editar > Preferências > Pastas > Plug-ins**

No Linux, clique com o botão direito no arquivo `simple-filter-plug-in.scm`, vá para **Propriedades > Permissões** e marque **Permitir execução de arquivo como programa**. Uma vez que o arquivo esteja no lugar certo, executável e livre de erros de sintaxe, quando o Lumi for reiniciado, ele aparecerá na barra de cabeçalho do menu superior, dentro de um menu chamado **Plug-in**.

### Executando o plug-in

1. Abra uma imagem (este plug-in de filtro requer uma imagem para funcionar).
2. Abra **Windows > Caixas de diálogo encaixáveis ​​> Console de erros** para ver uma mensagem.
3. Selecione **Demonstração de plug-in de filtro simples** no menu **Plug-in**.
4. Uma das camadas selecionadas terá suas cores invertidas e uma mensagem será impressa no console de erros.

### Editando o plug-in

Você pode personalizar o plug-in editando seu arquivo `.scm`. Por exemplo, para alterar a mensagem exibida:

1. Abra o arquivo e localize a linha que define `message`.
2. Substitua `"hello, world"` pelo seu texto personalizado.
3. Salve o arquivo.

No Lumi versão 3, os plug-ins não precisam ser atualizados para que as alterações salvas tenham efeito. Basta executar novamente o plug-in para ver a mensagem atualizada.

### Exame de plug-in

#### Linha Shebang

A primeira linha garante que o script funcione como um plug-in no Lumi 3:

```scheme
# !/usr/bin/env lumi-scheme-interpreter-0.1
```

#### Definição de procedimento

O procedimento aceita dois argumentos: a imagem ativa e os drawables selecionados.

```scheme
(define (scheme-simple-filter-plug-in image drawables)
```

#### Lógica Central

Uma instrução `let` define uma variável e executa operações no drawable.

```scheme
(let ((message "hello, world"))
  (lumi-message message) ;; Displays a message in Lumi's error console
  (lumi-drawable-invert (vector-ref drawables 0) 1)) ;; Inverts the colors of the first selected drawable
```

### Registro de plug-in

O plug-in está registrado no Lumi como um plug-in de filtro:

```scheme
(scheme-register-filter
  "scheme-simple-filter-plug-in"           ;; Register the main procedure
  "Simple Filter Plug-in Demo"             ;; The name as it appears in the Lumi menu
  "Tests a basic Scheme filter plug-in"    ;; Tool-tip description
  "Author Name"                            ;; Author's name
  "License"                                ;; License type
  "Date written"                           ;; Date written
  "*"                                      ;; Indicates the plug-in requires an image
  SF-ONE-OR-MORE-DRAWABLE)                 ;; Requires one or more selected drawables
```

#### Registro do Menu
Esta linha especifica a localização do menu do plug-in:

```scheme
(scheme-menu-register
  "scheme-simple-filter-plug-in"
  "<Image>/Plug-in")
```

### Solução de problemas

Se um plug-in não aparecer, verifique sua localização, nome e propriedade executável.

O local deve estar em um caminho de procura de plug-in.
O nome do arquivo deve corresponder ao nome da pasta que o contém.
O arquivo deve ser definido como executável.


O **Error Console** é uma ferramenta valiosa para solucionar problemas de plug-ins personalizados. Se o seu plug-in não se comportar conforme o esperado, verifique aqui se há mensagens de erro ou logs. A janela **Terminal** também pode fornecer informações de depuração e relatar problemas de carregamento.