---
title: "Encapsulamento"
type: docs
weight: 4
translation_provenance: ai-reviewed
translation_source_sha256: 7b176d9b546b2566812e825fb2e10da5dd4e86f0e79be2c362a4775546110ac6
translation_lock: true
url: "hub/scripting/tutorials/Wrapping/wrapping"
---
Os comandos Scheme operam a baixo nível: mesmo tarefas simples podem exigir vários passos. Esta granularidade dá flexibilidade para agrupar comandos em funções pequenas e reutilizáveis. O encapsulamento não é tudo ou nada; pode ir desde simples alias para comandos frequentes até funções complexas que gerem fluxos de trabalho inteiros. Por vezes, um wrapper é apenas uma função conveniente para melhorar a legibilidade; noutros casos evolui para um utilitário completo que encapsula várias operações.

### Por que agrupar funções?

Existem vários benefícios importantes nas funções de empacotamento:

- **Simplifica tarefas repetitivas** – Em vez de repetir comandos de baixo nível, envolva-os em uma função auxiliar e reutilize-a.
- **Melhora a legibilidade** – Dar às funções agrupadas nomes claros e descritivos torna o código mais fácil de entender rapidamente.
- **Encapsula a complexidade** – Em vez de lidar com listas longas e enigmáticas de comandos, loops profundamente aninhados ou instruções de mensagens complexas, podemos dividi-los em funções auxiliares menores e bem estruturadas.
- **Melhora a capacidade de manutenção** – Se a funcionalidade principal de um comando for alterada, só precisaremos atualizar a função encapsulada uma vez, isolando os plug-ins dos detalhes dessas alterações.
- **Incentiva a reutilização de código** – Cada auxiliar se torna parte da biblioteca, tornando mais rápido escrever e depurar scripts futuros.

À medida que os plug-ins crescem, os wrappers ajudam a manter a lógica central legível e a isolar detalhes repetitivos.

Outra vantagem de agrupar funções é integrá-las a um marcador de sintaxe como o Visual Studio Code. Isso melhora a legibilidade e a navegação, tornando os scripts mais claros. Em um plug-in que usa funções personalizadas, qualquer função destacada em verde confirma que foi referenciada corretamente na biblioteca.

Se mantiver uma biblioteca auxiliar, considere adicionar os nomes das funções do projeto ao destaque de sintaxe do editor. Torna a navegação e a refatoração mais rápidas.

Exemplos:

### Semente Aleatória

```scheme
;; Propósito: Retorna um inteiro aleatório para inicializar um filtro
(define (random-seed)
 (msrg-rand))
```

Embora pudéssemos usar ***msrg-rand*** diretamente no código, envolvê-lo em uma função chamada ***random-seed*** melhora a legibilidade. Ao dar à função um nome claro e descritivo, fica mais fácil entender rapidamente a finalidade.

Além disso, definir ***random-seed*** como uma função independente nos permite usá-la em qualquer lugar nos plug-ins enquanto centralizamos a implementação em um único local. Se algum dia precisarmos alterar a forma como a semente é gerada, só precisaremos atualizar esta função, deixando o resto do código intacto.

Por exemplo, se decidirmos mudar para ***random***:

```scheme
;; Propósito: Retorna um inteiro aleatório para inicializar um filtro
(define (random-seed)
 (random 1000))
```

O nome da função permanece o mesmo, garantindo que os scripts continuem funcionando sem modificações. Essa abordagem mantém o código flexível, sustentável e fácil de ler.

### Exportação JPEG

A função de exportação de JPEG no Scheme vem com muitos parâmetros, oferecendo um controle preciso sobre como as imagens são salvas. No entanto, na maioria dos casos, nos preocupamos apenas com algumas configurações importantes, como nome e qualidade do ficheiro. Para simplificar o processo, podemos agrupar a função.

```scheme
;; Propósito: Salva uma imagem como JPEG com uma qualidade especificada
(define (file-jpg-save image file quality)
 (let ((export-file (if (has-substring? file ".jpg")
 file
 (string-append file ".jpg")))) ;; Evitar jpg.jpg
 (debug-message "Exporting: " export-file)
 (file-jpeg-export #:run-mode RUN-NONINTERACTIVE
 #:image image
 #:file export-file
 #:options -1
 #:quality (* 0.01 quality)
 #:smoothing 0.0
 #:optimize 1
 #:progressive 1
 #:cmyk 0
 #:sub-sampling "sub-sampling-1x1"
 #:baseline 1
 #:restart 0
 #:dct "integer")))
```

Nesta função wrapper, a maioria das opções de exportação são codificadas, expondo apenas os parâmetros que provavelmente ajustaremos: nome e qualidade do ficheiro. Essa abordagem melhora a legibilidade e simplifica o guardar imagens.

Além disso, se o exportador do Lumi mudar no futuro, precisaremos apenas atualizar esta função em vez de modificar cada script que exporta um JPEG.

### Usando o wrapper

Para exportar um JPEG nos plug-ins, simplesmente incluímos a biblioteca e chamamos a função personalizada:

```scheme
(file-jpg-save image "/home/mark/pictures/my-picture" 85)
```

Isso mantém o código limpo, legível e adaptável, ao mesmo tempo que nos permite exportar JPEGs de forma eficiente e com mínimo esforço.

### Substituir `car`

A função ***car*** pode ser enigmática e propensa a erros de script. É fácil aplicar erroneamente ***car*** a um vetor ou item que não esteja na lista, levando a um comportamento inesperado. Para tornar o código mais robusto e legível, podemos agrupar essa funcionalidade em uma função mais segura.

```scheme
;; Propósito: Retorna o primeiro item de uma lista ou vetor.
;; Avisa se a entrada for inválida ou vazia.
(define (first-item collection)
 (cond
 ;; Trata listas não vazias
 ((and (list? collection) (not (null? collection)))
 (list-ref collection 0))
 ;; Trata vetores não vazios
 ((and (vector? collection) (> (vector-length collection) 0))
 (vector-ref collection 0))
 ;; Entrada inválida ou vazia
 (else
 (begin
 (warning-message "first-item: Expected a non-empty list or vector, but received: " collection)
 #f))))
```

Esta função recupera com segurança o primeiro item de uma lista ou vetor enquanto fornece avisos úteis quando entradas inválidas ou vazias são encontradas. Ao usar ***first-item*** em vez de ***car***, reduzimos o risco de erros acidentais e melhoramos a clareza de os scripts.

#### Por que usar este wrapper?

- **Evita falhas de script** – Evita erros causados pela aplicação de ***car*** a não listas.
- **Suporta listas e vetores** – Expande a usabilidade além de apenas listas.
- **Fornece avisos significativos** – Ajuda a depurar problemas de entrada inesperados.
- **Melhora a legibilidade** – O nome da função transmite claramente a finalidade.

Ao encapsular essa lógica no primeiro item, tornamos os plug-ins mais robustos e fáceis de manter. Claro, isso se resume à preferência pessoal, pode sentir-se completamente confortável usando car, caar, cadr e funções de Scheme semelhantes diretamente.

### Quebrando uma função empacotada

O empacotamento de uma função que já está empacotada pode melhorar ainda mais a legibilidade e a manutenção. Por exemplo, ao trabalhar com pares de coordenadas como ***coordenadas de pixel (lista 100 200)***, poderíamos usar:

```scheme
(first-item pixel-coords)
```

para recuperar a coordenada ***x***. Porém, embora funcional, isso não é muito expressivo. Em vez disso, podemos agrupar o ***primeiro item*** em uma definição mais apropriada para tornar a intenção mais clara.

```scheme
;; Propósito: Retornar a coordenada x, para legibilidade
(define (x-coord pixel-coords)
 (first-item pixel-coords))

;; Propósito: Retornar a coordenada y, para legibilidade
(define (y-coord pixel-coords)
 (second-item pixel-coords))
```

### Por que usar esta abordagem?

- **Aumenta a clareza do código** – Em vez de usar funções genéricas de acesso à lista, definimos explicitamente funções que descrevem a finalidade.
- **Melhora a capacidade de manutenção** – Se a representação de coordenadas mudar (por exemplo, usando vetores em vez de listas), só precisaremos atualizar essas pequenas funções.
- **Incentiva a consistência** – Usar ***x-coord*** e ***y-coord*** torna o script mais fácil de ler e entender rapidamente.

Agora, em vez de escrever em Scheme fallback:

```scheme
(car pixel-coords) ;; Obtém a coordenada x
(cadr pixel-coords) ;; Obtém a coordenada y
```

Podemos escrever em Scheme:

```scheme
(x-coord pixel-coords)
(y-coord pixel-coords)
```

Ao agrupar funções de baixo nível em nomes significativos, criamos uma forma mais intuitiva de trabalhar com dados, reduzindo confusão e possíveis erros.

### Invólucros incluídos: utilitário Stdlib

O Lumi envia um conjunto de invólucros prontos carregados automaticamente na inicialização, para que estejam disponíveis em qualquer plug-in ou na consola Scheme sem qualquer chamada `(load ...)`. Estas bibliotecas (`common.scm`, `files.scm`, `gegl.scm`, `images.scm`, `layers.scm`, `parasites.scm` e `paths.scm`) são construídas exactamente no mesmo princípio dos exemplos acima: fornecem nomes claros para operações de baixo nível, ocultam clichês repetitivos e concentram num único local a actualização se o comando subjacente mudar. Por exemplo, `images.scm` fornece `image-get-open-list` como um invólucro legível em torno da chamada PDB bruta, e `files.scm` expõe auxiliares de construção de caminho que, de outra forma, exigiriam cadeias `string-append` repetidas.

Pode navegar por cada nome exportado, ler a documentação e ver de qual biblioteca ele vem em **[Navegador de Utilitários]({{< ref "/hub/scripting/reference/utility-browser" >}})** (Ajuda → Programação → Navegador de Utilitários). É uma demonstração prática de empacotamento em escala e uma fonte útil de padrões para emprestar ao construir a própria biblioteca auxiliar.

### Conclusão

As funções de agrupamento são uma forma poderosa de simplificar o desenvolvimento do Scheme, tornando os scripts mais legíveis, fáceis de manter e robustos. Ao encapsular a complexidade e expor apenas os detalhes necessários, criamos uma abordagem mais estruturada para escrever plug-ins.

Principais conclusões desta abordagem:

- **Simplifica tarefas repetitivas** – Em vez de repetir manualmente comandos de baixo nível, criamos funções reutilizáveis.
- **Melhora a legibilidade do código** – Invólucros bem nomeados tornam os scripts mais fáceis de entender.
- **Encapsula a complexidade** – Detalhes de baixo nível são tratados dentro do wrapper, mantendo o script principal limpo.
- **Melhora a capacidade de manutenção** – Se a funcionalidade principal for alterada, só precisaremos atualizar o wrapper, não todos os scripts que dependem dele.
- **Incentiva a reutilização e a consistência** – A biblioteca pessoal de funções cresce com o tempo, tornando o desenvolvimento mais rápido e eficiente.

Ao usar consistentemente o empacotamento de funções, podemos transformar a forma como escrevemos plug-ins do Scheme, criando um ambiente de script mais modular e expressivo. Com estes princípios em mente, podemos continuar a refinar a abordagem, desenvolvendo uma versão mais eficiente e personalizada de Scheme adaptada às necessidades específicas do projecto.

Próximas etapas: identifique blocos repetidos nos scripts e extraia pequenos auxiliares com nomes claros.