---
title: "O plug-in de filtro"
type: docs
weight: 2
translation_provenance: ai-reviewed
translation_source_sha256: db9cfb794dad80ce918a3eca7b47d02b23dbbba960a26765c04d95d459d8ec6b
translation_lock: true
url: "hub/scripting/tutorials/Filter Plug-in/filter-plug-ins"
---
Usamos um plug-in _procedure_ para o tutorial [First Step](../../first-step/). Esses tipos de plug-ins funcionam sem a necessidade de uma imagem ou drawable como entrada. Normalmente, usamos um plug-in para alterar uma imagem e seus drawables. Plug-ins como esses são chamados plug-ins de _filtro_.

### O que é um drawable?

Um **drawable** no Lumi refere-se a um elemento de imagem que pode ser desenhado, como uma camada ou canal. Os plug-ins de filtro normalmente operam nesses elementos.

### Um exemplo simples de plug-in de filtro

```scheme
#!/usr/bin/env lumi-scheme-interpreter-0.1

(define (scheme-simple-filter-plug-in image drawables)
 ;; Usar uma instrução let para definir uma variável de mensagem e o código principal
 (let ((message "hello, world"))
 ;; Exibir a mensagem na consola de erros do Lumi
 (lumi-message message)
 ;; Inverter as cores do primeiro drawable selecionado
 (lumi-drawable-invert (vector-ref drawables 0) 1)))

;; Registra o plug-in
(scheme-register-filter
 "scheme-simple-filter-plug-in" ;; Nome do procedimento principal
 "Simple Filter Plug-in Demo" ;; O nome como aparece no menu do Lumi
 "Tests a basic Scheme filter plug-in" ;; Descrição da dica de ferramenta
 "Author Name" ;; Dê crédito a si mesmo
 "License" ;; Licença
 "Date written" ;; Data de escrita
 "*" ;; Indica que este plug-in requer uma imagem
 SF-ONE-OR-MORE-DRAWABLE) ;; Requer um ou mais drawables selecionados

;; Especificar a localização do menu para o plug-in
(scheme-menu-register
 "scheme-simple-filter-plug-in"
 "<Image>/Plug-in")
```

Copie o texto e salve-o como `simple-filter-plug-in.scm` em uma pasta chamada `simple-filter-plug-in` dentro de uma das pastas de plug-ins do Lumi. Uma pasta de plug-ins do Lumi é _qualquer_ pasta listada em:
 **Lumi > Editar > Preferências > Pastas > Plug-ins**

No Linux, clique com o botão direito no ficheiro `simple-filter-plug-in.scm`, vá para **Propriedades > Permissões** e marque **Permitir execução de ficheiro como programa**. Uma vez que o ficheiro esteja no lugar certo, executável e livre de erros de sintaxe, quando o Lumi for reiniciado, ele aparecerá na barra de cabeçalho do menu superior, dentro de um menu chamado **Plug-in**.

### Executando o plug-in

1. Abra uma imagem (este plug-in de filtro requer uma imagem para funcionar).
2. Abra **Ferramentas > Depuração > Console de mensagens** para ver uma mensagem.
3. Selecione **Demonstração de plug-in de filtro simples** no menu **Plug-in**.
4. Uma das camadas selecionadas terá suas cores invertidas e uma mensagem será impressa no console de erros.

### Editando o plug-in

Pode personalizar o plug-in editando o ficheiro `.scm`. Por exemplo, para alterar a mensagem exibida:

1. Abra o ficheiro e localize a linha que define `message`.
2. Substitua `"hello, world"` pelo o texto personalizado.
3. Salve o ficheiro.

No Lumi versão 3, os plug-ins não precisam ser atualizados para que as alterações salvas tenham efeito. Basta executar novamente o plug-in para ver a mensagem atualizada.

### Exame de plug-in

#### Linha Shebang

A primeira linha garante que o script funcione como um plug-in no Lumi 3:

```scheme
#!/usr/bin/env lumi-scheme-interpreter-0.1

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
 (lumi-message message) ;; Exibe uma mensagem na consola de erros do Lumi
 (lumi-drawable-invert (vector-ref drawables 0) 1)) ;; Inverte as cores do primeiro drawable selecionado
```

### Registo de plug-ins

O plug-in está registado no Lumi como um plug-in de filtro:

```scheme
(scheme-register-filter
 "scheme-simple-filter-plug-in" ;; Registra o procedimento principal
 "Simple Filter Plug-in Demo" ;; O nome como aparece no menu do Lumi
 "Tests a basic Scheme filter plug-in" ;; Descrição da dica de ferramenta
 "Author Name" ;; Nome do autor
 "License" ;; Tipo de licença
 "Date written" ;; Data de escrita
 "*" ;; Indica que o plug-in requer uma imagem
 SF-ONE-OR-MORE-DRAWABLE) ;; Requer um ou mais drawables selecionados
```

#### Registo do Menu

Esta linha especifica a localização do menu do plug-in:

```scheme
(scheme-menu-register
 "scheme-simple-filter-plug-in"
 "<Image>/Plug-in")
```

### Solução de problemas

Se um plug-in não aparecer, verifique a localização, nome e propriedade executável.

O local deve estar em um caminho de procura de plug-in.
O nome do ficheiro deve corresponder ao nome da pasta que o contém.
O ficheiro deve ser definido como executável.


O **Console de mensagens** é uma ferramenta valiosa para solucionar problemas de plug-ins personalizados. Se o plug-in não se comportar conforme o esperado, verifique aqui se há mensagens de erro ou logs. A janela **Terminal** também pode fornecer informações de depuração e relatar problemas de carregamento.