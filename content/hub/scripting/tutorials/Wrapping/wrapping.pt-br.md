---
title: "Embrulho"
type: docs
weight: 4
---
Os comandos do esquema operam em um nível baixo, o que significa que mesmo tarefas simples podem exigir várias etapas. No entanto, esta granularidade oferece flexibilidade, podemos agrupar comandos em funções pequenas e reutilizáveis ​​que fazem exatamente o que precisamos. Embrulhar não é um conceito preto e branco; ele pode variar de simples aliases para comandos usados ​​com frequência até funções mais complexas que gerenciam fluxos de trabalho inteiros. Às vezes, um wrapper é apenas uma função conveniente para melhorar a legibilidade, enquanto em outros casos evolui para um utilitário completo que encapsula várias operações.

### Por que agrupar funções?

Existem vários benefícios importantes nas funções de empacotamento:

- **Simplifica tarefas repetitivas** – Em vez de repetir comandos de baixo nível, envolva-os em uma função auxiliar e reutilize-a.
- **Melhora a legibilidade** – Dar às nossas funções agrupadas nomes claros e descritivos torna nosso código mais fácil de entender rapidamente.
- **Encapsula a complexidade** – Em vez de lidar com listas longas e enigmáticas de comandos, loops profundamente aninhados ou instruções de mensagens complexas, podemos dividi-los em funções auxiliares menores e bem estruturadas.
- **Melhora a capacidade de manutenção** – Se a funcionalidade principal de um comando for alterada, só precisaremos atualizar nossa função encapsulada uma vez, isolando nossos plug-ins dos detalhes dessas alterações.
- **Incentiva a reutilização de código** – Cada auxiliar se torna parte de sua biblioteca, tornando mais rápido escrever e depurar scripts futuros.

À medida que seus plug-ins crescem, os wrappers ajudam a manter a lógica central legível e a isolar detalhes repetitivos.

Outra vantagem de agrupar funções é integrá-las a um marcador de sintaxe como o Visual Studio Code. Isso melhora a legibilidade e a navegação, tornando os scripts mais claros. Em um plug-in que usa funções personalizadas, qualquer função destacada em verde confirma que foi referenciada corretamente em nossa biblioteca.

Se você mantém sua própria biblioteca auxiliar, considere adicionar os nomes das funções do seu projeto ao destaque de sintaxe do seu editor. Torna a navegação e a refatoração mais rápidas.

Exemplos:

### Semente Aleatória

```scheme
;; Purpose: Returns a random int for seeding a filter
(define (random-seed)
  (msrg-rand))
```

Embora pudéssemos usar ***msrg-rand*** diretamente em nosso código, envolvê-lo em uma função chamada ***random-seed*** melhora a legibilidade. Ao dar à função um nome claro e descritivo, fica mais fácil entender rapidamente sua finalidade.

Além disso, definir ***random-seed*** como uma função independente nos permite usá-la em qualquer lugar em nossos plug-ins enquanto centralizamos a implementação em um único local. Se algum dia precisarmos alterar a forma como a semente é gerada, só precisaremos atualizar esta função, deixando o resto do nosso código intacto.

Por exemplo, se decidirmos mudar para ***random***:

```scheme
;; Purpose: Returns a random int for seeding a filter
(define (random-seed)
  (random 1000))
```

O nome da função permanece o mesmo, garantindo que nossos scripts continuem funcionando sem modificações. Essa abordagem mantém nosso código flexível, sustentável e fácil de ler.

### Exportação JPEG

A função de exportação de JPEG no Scheme vem com muitos parâmetros, oferecendo um controle preciso sobre como as imagens são salvas. No entanto, na maioria dos casos, nos preocupamos apenas com algumas configurações importantes, como nome e qualidade do arquivo. Para simplificar o processo, podemos agrupar a função.

```scheme
;; Purpose: Saves an image as a JPEG with a specified quality
(define (file-jpg-save image file quality)
  (let ((export-file (if (has-substring? file ".jpg")
                         file
                         (string-append file ".jpg")))) ;; Avoid jpg.jpg
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

Nesta função wrapper, a maioria das opções de exportação são codificadas, expondo apenas os parâmetros que provavelmente ajustaremos: nome e qualidade do arquivo. Essa abordagem melhora a legibilidade e simplifica o salvamento de imagens.Além disso, se o exportador do Lumi mudar no futuro, precisaremos apenas atualizar esta função em vez de modificar cada script que exporta um JPEG.

### Usando o wrapper

Para exportar um JPEG em nossos plug-ins, simplesmente incluímos a biblioteca e chamamos nossa função personalizada:

```scheme
(file-jpg-save image "/home/mark/pictures/my-picture" 85)
```

Isso mantém nosso código limpo, legível e adaptável, ao mesmo tempo que nos permite exportar JPEGs de forma eficiente e com mínimo esforço.

### Substituição de carro

A função ***car*** pode ser enigmática e propensa a erros de script. É fácil aplicar erroneamente ***car*** a um vetor ou item que não esteja na lista, levando a um comportamento inesperado. Para tornar nosso código mais robusto e legível, podemos agrupar essa funcionalidade em uma função mais segura.

```scheme
;; Purpose: Returns the first item of a list or vector.
;;          Warns if the input is invalid or empty.
(define (first-item collection)
  (cond
    ;; Handle non-empty lists
    ((and (list? collection) (not (null? collection)))
     (list-ref collection 0))
    ;; Handle non-empty vectors
    ((and (vector? collection) (> (vector-length collection) 0))
     (vector-ref collection 0))
    ;; Invalid or empty input
    (else
     (begin
       (warning-message "first-item: Expected a non-empty list or vector, but received: " collection)
       #f))))
```

Esta função recupera com segurança o primeiro item de uma lista ou vetor enquanto fornece avisos úteis quando entradas inválidas ou vazias são encontradas. Ao usar ***first-item*** em vez de ***car***, reduzimos o risco de erros acidentais e melhoramos a clareza de nossos scripts.

#### Por que usar este wrapper?

- **Evita falhas de script** – Evita erros causados ​​pela aplicação de ***car*** a não listas.
- **Suporta listas e vetores** – Expande a usabilidade além de apenas listas.
- **Fornece avisos significativos** – Ajuda a depurar problemas de entrada inesperados.
- **Melhora a legibilidade** – O nome da função transmite claramente sua finalidade.

Ao encapsular essa lógica no primeiro item, tornamos nossos plug-ins mais robustos e fáceis de manter. Claro, isso se resume à preferência pessoal, você pode se sentir completamente confortável usando car, caar, cadr e funções de esquema semelhantes diretamente.

### Quebrando uma função empacotada

O empacotamento de uma função que já está empacotada pode melhorar ainda mais a legibilidade e a manutenção. Por exemplo, ao trabalhar com pares de coordenadas como ***coordenadas de pixel (lista 100 200)***, poderíamos usar:

```scheme
(first-item pixel-coords)
```

para recuperar a coordenada ***x***. Porém, embora funcional, isso não é muito expressivo. Em vez disso, podemos agrupar o ***primeiro item*** em uma definição mais apropriada para tornar nossa intenção mais clara.

```scheme
;; Purpose: Return the x-coordinate, for readability
(define (x-coord pixel-coords)
  (first-item pixel-coords))

;; Purpose: Return the y-coordinate, for readability
(define (y-coord pixel-coords)
  (second-item pixel-coords))
```

### Por que usar esta abordagem?

- **Aumenta a clareza do código** – Em vez de usar funções genéricas de acesso à lista, definimos explicitamente funções que descrevem sua finalidade.
- **Melhora a capacidade de manutenção** – Se nossa representação de coordenadas mudar (por exemplo, usando vetores em vez de listas), só precisaremos atualizar essas pequenas funções.
- **Incentiva a consistência** – Usar ***x-coord*** e ***y-coord*** torna o script mais fácil de ler e entender rapidamente.

Agora, em vez de escrever em esquema genérico:

```scheme
(car pixel-coords) ;; Gets the x-coordinate
(cadr pixel-coords) ;; Gets the y-coordinate
```

Podemos escrever em _nosso_ esquema:

```scheme
(x-coord pixel-coords)
(y-coord pixel-coords)
```

Ao agrupar funções de baixo nível em nomes significativos, criamos uma maneira mais intuitiva de trabalhar com dados, reduzindo confusão e possíveis erros.

### Conclusão

As funções de agrupamento são uma maneira poderosa de simplificar o desenvolvimento do esquema, tornando os scripts mais legíveis, fáceis de manter e robustos. Ao encapsular a complexidade e expor apenas os detalhes necessários, criamos uma abordagem mais estruturada para escrever plug-ins.

Principais conclusões desta abordagem:- **Simplifica tarefas repetitivas** – Em vez de repetir manualmente comandos de baixo nível, criamos funções reutilizáveis.
- **Melhora a legibilidade do código** – Wrappers bem nomeados tornam os scripts mais fáceis de entender.
- **Encapsula a complexidade** – Detalhes de baixo nível são tratados dentro do wrapper, mantendo o script principal limpo.
- **Melhora a capacidade de manutenção** – Se a funcionalidade principal for alterada, só precisaremos atualizar o wrapper, não todos os scripts que dependem dele.
- **Incentiva a reutilização e a consistência** – Nossa biblioteca pessoal de funções cresce com o tempo, tornando o desenvolvimento mais rápido e eficiente.

Ao usar consistentemente o empacotamento de funções, podemos transformar a forma como escrevemos plug-ins do Scheme, criando um ambiente de script mais modular e expressivo. Com estes princípios em mente, podemos continuar a refinar a nossa abordagem, desenvolvendo uma versão mais eficiente e personalizada do Scheme que atenda às nossas necessidades específicas.

Próximas etapas: identifique blocos repetidos em seus scripts e extraia pequenos auxiliares com nomes claros.