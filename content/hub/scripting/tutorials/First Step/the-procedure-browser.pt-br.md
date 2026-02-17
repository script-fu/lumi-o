---
title: "O navegador de procedimentos"
type: docs
weight: 1
---
O **Lumi Procedure Browser** permite pesquisar os procedimentos disponíveis (integrados e plug-in fornecidos) e inspecionar seus parâmetros e valores de retorno.

### Onde encontrar o navegador de procedimento Lumi

Você pode acessar o Navegador de Procedimentos no Lumi através do menu **Ajuda**:

- **Ajuda** -> **Navegador de procedimentos**

### O que o navegador de procedimento faz

O Navegador de Procedimentos lista todos os procedimentos internos do Lumi, juntamente com aqueles adicionados por plug-ins, incluindo aquele que você acabou de instalar. Cada entrada de procedimento fornece informações úteis, incluindo:

- O nome do procedimento.
- Uma descrição do que faz.
- Os parâmetros que aceita (valores de entrada).
- Os valores de retorno (saída).

Pesquise por palavra-chave ou nome de procedimento quando precisar verificar uma assinatura de chamada ou confirmar o nome exato do procedimento.

#### (mensagem lumi) no navegador de procedimento

Pesquise `lumi-message` para ver seus parâmetros e valores de retorno.

### Encontrando seu plug-in

Depois de instalar o "Hello World!" plug-in, você pode encontrá-lo listado no Navegador de procedimentos. Basta procurar pelo nome da função que você cadastrou no Lumi, neste caso, “scheme-hello-world”. A entrada exibirá os parâmetros e quaisquer valores de retorno associados ao plug-in, juntamente com uma breve descrição. Você também verá onde algumas das linhas de texto inseridas como parâmetros de entrada durante o processo de registro são exibidas na seção **Informações adicionais**.

```scheme
(scheme-register-procedure "scheme-hello-world"   ;; Procedure name
  "Hello world!"                                        ;; Menu item name
  "A Scheme procedure plug-in"                       ;; Tool tip and description
  "Your Name"                                           ;; Author
  "Under GNU GENERAL PUBLIC LICENSE Version 3"          ;; License
  "2024")                                               ;; Copyright Date
```

Isso facilita a verificação de que seu plug-in está devidamente registrado e oferece uma maneira rápida de revisar como ele interage com outros procedimentos no Lumi. O Procedure Browser é uma ferramenta poderosa para depurar e expandir seus plug-ins, explorando todos os procedimentos disponíveis no Lumi.