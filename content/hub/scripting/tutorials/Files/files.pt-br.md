---
title: "Arquivos"
type: docs
weight: 7
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: a68dc9328daa1e5b96aee6bf0949a8454b7826df85bdae254502ad9a24864992
---
Trabalhar com arquivos e diretórios é essencial para o desenvolvimento do Scheme. Esteja você salvando resultados, carregando recursos ou organizando a estrutura do seu projeto, compreender as operações dos arquivos tornará seus scripts mais robustos e fáceis de usar.

Esta página aborda tarefas comuns de arquivos e diretórios: leitura de caminhos, criação de diretórios e coleta de entradas de pastas por meio de parâmetros GUI.

## Diretório inicial do usuário

Lumi é somente Linux, então o diretório inicial do usuário vem da variável de ambiente `HOME`.

Para obter o diretório inicial do usuário como uma string:

```scheme
(getenv "HOME")
```

Exemplo de saída:

```scheme
"/home/username"
```

## SEPARADOR DE DIR

Há também a variável global `DIR-SEPARATOR`, que é o separador de caminho específico da plataforma. No Lumi (Linux), é sempre `/`.

```scheme
> DIR-SEPARATOR
"/"
```

## Obtendo um local de diretório

Podemos pedir ao usuário um local de diretório na caixa de diálogo Esquema para um plug-in.

```scheme
(scheme-register
  "scheme-batch-process"
  "Batch Process"
  "Iteratively open the source files, then process, export and close"
  "Mark Sweeney"
  "Under GNU GENERAL PUBLIC LICENSE Version 3"
  "2025"
  ""
  SF-DIRNAME "Loca_tion of Source"       ""
  SF-DIRNAME "Location of _Destination"  ""
  SF-TOGGLE  "S_how Loaded Images"       0
  SF-TOGGLE  "Only Process Open I_mages" 0)
```

O `SF-DIRNAME` fornece um navegador para um diretório.

```scheme
(define (batch-process-file-system src-dir src-dir-fallback extension dst-dir dst-dir-fallback show-images process-fn export-fn)
  (let* ((validated-src-dir (validate-path-and-dir src-dir src-dir-fallback "Source"))
         (validated-dst-dir (validate-path-and-dir dst-dir dst-dir-fallback "Destination"))
         (files (discover-files validated-src-dir extension)))
    ;; …
    ))
```

Aqui validamos as duas entradas de diretório (origem e destino) e voltamos aos padrões se os caminhos da GUI estiverem vazios/inválidos.

[/hub/scripting/plug-ins/processo em lote/](/hub/scripting/plug-ins/batch-process/)

Se você estiver interessado nos detalhes da implementação, pesquise `validate-path-and-dir` na fonte do plug-in.

## Criando um diretório

O esquema fornece o comando ```dir-make``` para criar um diretório. Este comando usa um caminho separado por "/" e cria um único diretório com um parâmetro opcional para os privilégios. Não fornecemos caminhos específicos de plataforma.

Normalmente precisamos criar vários diretórios para um caminho prático. Podemos usar um wrapper para ```dir-make``` para nos ajudar aqui.

```scheme
;; Propósito: Um wrapper para (dir-make) que cria um caminho dado a partir de uma plataforma
;;          caminho fornecido. Sempre emite separadores no estilo Linux para dir-make.
(define (make-dir-path path)
  (let* ((path-parts (strbreakup path DIR-SEPARATOR))
         (current-path (car path-parts))) ; Diretório raiz
    ;; Criar o restante dos diretórios passo a passo
    (for-each
     (lambda (part)
       (set! current-path (string-append current-path "/" part)) ; Constrói o caminho
       (if (file-exists? current-path)
         (debug-message "Directory exists: " current-path)
         (if (dir-make current-path)
           (debug-message "Made directory: " current-path)
           (warning-message "Failed to make directory: " current-path))))
     (cdr path-parts))))
```

Nota: Esta função também usa ```file-exists?``` integrado para pular chamadas desnecessárias. Retorna #t se o arquivo ou diretório indicado existir e #f se não existir ou se não estiver acessível ao usuário solicitante.

## Construindo um Caminho

Também precisamos quebrar e reconstruir caminhos no Scheme.

Para dividir um caminho em partes, use ```strbreakup```:

### Exemplos de caminhos do Linux

```scheme
> (strbreakup (getenv "HOME") DIR-SEPARATOR)
("" "home" "username")

> (strbreakup "/this/path/" DIR-SEPARATOR)
("" "this" "path" "")
```

> Nota: As barras iniciais e finais tornam-se elementos de string vazios na lista resultante.

Para reconstruir um caminho, use ```string-append```:

### Construção de caminho Linux

```scheme
> (string-append (getenv "HOME") DIR-SEPARATOR "myfolder" DIR-SEPARATOR "myfile.xcf")
"/home/username/myfolder/myfile.xcf"
```
```