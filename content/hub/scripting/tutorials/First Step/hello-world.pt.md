---
title: "Olá mundo!"
type: docs
weight: 1
translation_provenance: ai-reviewed
translation_source_sha256: c250d07dff926c7b51434efc644786f35b5189e03449dcdf4ec5916c1c151886
translation_lock: true
url: "hub/scripting/tutorials/First Step/hello-world"
---
Este tutorial percorre a estrutura mínima de um plug-in do Scheme. Algumas linhas são “padrão”: são necessárias para que o Lumi carregue o ficheiro, mesmo que ainda não as entenda completamente.

```bash
#!/usr/bin/env lumi-scheme-interpreter-0.1

```

Em um nível alto, irá:

1. Defina uma função
2. Cadastre-o para que apareça no banco de dados de procedimentos
3. (Opcional) Adicione uma entrada de menu
4. Instale o ficheiro em uma pasta de plug-ins

### Definir uma função

Uma função, também conhecida como _procedimento_, é um pedaço de código com nome e finalidade, recebe uma entrada e produz uma saída.

**Entrada** > **_Função_** > **Saída**

### Registre a função

Cadastrar é o ato de colocar o nome da função em uma lista para que Lumi saiba disso.

```scheme
(scheme-register-procedure "scheme-hello-world"...
```

### Link para o menu

Isso informa ao Lumi onde encontrar a função no sistema de menu.

```scheme
(scheme-menu-register "scheme-hello-world" "<Image>/Funky")
```

Isso exibe o menu "Funky" na barra de menu principal. Altere o caminho para colocar o plug-in em outro lugar. O caminho `<Image>/Funky` significa que o plug-in aparecerá na categoria de menu **Imagem**. Pode alterar `<Image>` para `<Tools>`, `<Filters>`, etc., dependendo de onde deseja que o plug-in apareça.

### Comentários

No Scheme, os comentários geralmente são feitos precedendo uma linha de texto útil com `;;`. O uso de comentários dependerá da fluência como programador – se programar ocasionalmente, mais comentários ajudarão. Se programar o tempo todo, o código será tão fácil de ler quanto o comentário. Além disso, ao programar funcionalmente, o código tende a se tornar descritivo o suficiente para ser lido como um script.

### Sintaxe

O código tende a ter pequenas regras sobre como colocar itens em uma linha, para que possamos ler a linha facilmente. Por exemplo, uma frase pode ter um espaço após uma vírgula ou ponto final. Ajuda na legibilidade.

O código pode organizar as coisas de forma semelhante, o que pode parecer estranho à primeira vista:

```scheme
(define (function-name input-a
 input-b
 input-c))
```

## Código de exemplo

Aqui está o exemplo completo. A maioria dos procedimentos Lumi são prefixados com `lumi- `. Por exemplo, `lumi-message` imprime uma string no manipulador de mensagens configurado.

```scheme
#!/usr/bin/env lumi-scheme-interpreter-0.1

(define (scheme-hello-world)

 ;; Definir o manipulador de mensagens para enviar a mensagem para uma caixa de diálogo GUI
 (lumi-message-set-handler 0)
 (lumi-message "Hello world!\n")

 ;; Definir o manipulador de mensagens para enviar a mensagem para a consola de erros
 (lumi-message-set-handler 2)
 (lumi-message "Hello world!\n")

 ;; Enviar a mensagem para o terminal, a janela do SO que iniciou o Lumi
 (display "Hello world!\n"))


(scheme-register-procedure "scheme-hello-world"
 "Hello world!"
 "A Scheme procedure plug-in"
 "Mark Sweeney"
 "Under GNU GENERAL PUBLIC LICENSE Version 3"
 "2024")

(scheme-menu-register
 "scheme-hello-world"
 "<Image>/Funky")
```

### Instale o plug-in

1. Vá para **Lumi -> Editar -> Preferências -> Pastas -> Plug-ins**.
2. Adicione a pasta de plug-ins [repo](/hub/scripting/tools/git) à lista.
3. Crie uma pasta para o plug-in e salve o código de exemplo acima como `hello-world.scm`:
 - `your-plug-ins-repo/hello-world/hello-world.scm`
4. Clique com o botão direito no ficheiro `hello-world.scm`.
5. Vá para **Propriedades -> Permissões -> Permitir execução de ficheiro como programa**.
6. Reinicie o Lumi.

### Experimente o plug-in

O plug-in agora deve aparecer no menu “Funky” na janela principal do Lumi. Clique nele e deverá exibir a mensagem "Olá, mundo!" mensagem. Tente modificar o código, como alterar o texto da mensagem, e salve o ficheiro. Ao executar o plug-in novamente, as alterações serão refletidas sem reiniciar o Lumi.

Experimente alterar o caminho do menu. Por exemplo, `"<Image>/File"` irá colocá-lo dentro do menu Arquivo e `"<Image>/File/Funky"` irá criar uma nova secção no menu Arquivo. Essa é uma ótima forma de personalizar onde o plug-in aparece e de organizar suas ferramentas.