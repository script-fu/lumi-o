---
title: "Camadas e edição não destrutiva"
type: docs
---
O sistema de camadas do Lumi permite fluxos de trabalho complexos e não destrutivos com controle total sobre mesclagem, mascaramento e composição.

## Visão geral

As camadas são a base da ilustração estruturada. Cada camada é independente, com seu próprio modo de mesclagem, opacidade e máscara de camada opcional. Os grupos podem aninhar camadas hierarquicamente com suas próprias propriedades de mesclagem e recorte.

## Acesso

**Painéis** → **Camadas** ou o painel padrão **Camadas** à direita.

## Tipos de camada

### Pintar Camadas
Camadas raster padrão para conteúdo pintado. Armazene dados de pixels como buffers GEGL com transparência alfa opcional.

### Camadas de Grupo
Contêineres hierárquicos para organizar camadas relacionadas. Os grupos podem ter seu próprio modo de mesclagem, opacidade e máscaras de corte. As projeções do grupo são compostas sob demanda.

### Máscaras de camada
Máscaras em tons de cinza anexadas a qualquer camada, controlando a opacidade por pixel. Pintar uma máscara com branco torna os pixels opacos; o preto os torna transparentes; cinza fornece opacidade parcial.

## Modos de mesclagem

Cada camada possui um modo de mesclagem que determina como ela se combina com as camadas abaixo:

- **Normal**: Mesclagem direta de opacidade.
- **Multiplicar**: Escureça multiplicando os valores das cores.
- **Tela**: Ilumine invertendo, multiplicando e invertendo novamente.
- **Sobreposição**: Combinação de Multiplicação e Tela.
- **Adicionar**: Mistura de aditivos (soma os valores das cores).
- **Subtrair**: Mistura subtrativa.
- **Cor, Matiz, Saturação, Luminosidade**: Mistura de componentes HSL.

## Recorte e mascaramento

- **Modo Composto — Cortar no Pano de Fundo**: Definir o modo composto de uma camada como **Cortar no Pano de Fundo** restringe a composição a áreas onde as camadas **União** acumuladas abaixo estabeleceram opacidade. A camada pinta apenas onde essas camadas têm conteúdo — ela não pode expandir a área alfa. Isso é definido por camada na caixa de diálogo Atributos da camada (menu suspenso **Modo composto**). Quando o modo de composição efetivo de uma camada for diferente de União, o ícone de olho no painel Camadas será substituído por um ícone de composição para indicar o comportamento de composição não padrão.

  **Exemplo — forma alfa compartilhada:** Em um grupo, a camada inferior contém um círculo preenchido em um fundo transparente, definido como o modo de composição padrão **União**. Cada camada acima dela no mesmo grupo está definida como **Cortar para Pano de Fundo**. Essas camadas só podem pintar onde o círculo fornece opacidade – uma forma, muitas camadas. Este é um padrão comum para colorir, sombrear e detalhar dentro de uma silhueta definida sem se preocupar com respingos.
- **Máscaras de camada**: aplique uma máscara de escala de cinza para controlar a visibilidade da camada pixel por pixel. A pintura branca na máscara revela; ocultações pretas; cinza fornece opacidade parcial.
- **Máscaras Pure-Child**: as máscaras são armazenadas como filhas na pilha drawable, evitando a perda de dados durante as transformações.

## Seleção de camada (tecla Alt)

Tocar em **Alt** (Alt esquerdo) enquanto passa o mouse sobre a tela seleciona a camada com pixels visíveis abaixo do cursor – sem trocar de ferramentas ou clicar.

### Como funciona

- **Pressione Alt**: O cursor muda para uma cruz, indicando que o modo de seleção está ativo.
- **Soltar Alt**: Lumi escolhe a camada não transparente superior na posição do cursor (opacidade > 25%) e a seleciona. A camada é destacada no painel Camadas e a barra de status mostra **"Camada escolhida: 'nome da camada'"**.
- Uma alça é desenhada no ponto central da camada selecionada na tela. A alça diminui e desaparece à medida que o cursor se afasta.

### Percorrendo camadasCada toque Alt subsequente no mesmo local seleciona a **próxima camada** na pilha naquele ponto. Lumi se lembra da última camada escolhida e passa para a que está abaixo. Assim que a parte inferior da pilha for alcançada, o próximo toque volta para a camada superior naquela posição. Isso facilita o acesso a camadas aninhadas em cenas complexas tocando em Alt repetidamente.

### Regras de cancelamento

A seleção é cancelada (não é acionada ao liberar Alt) se uma das seguintes situações ocorrer enquanto Alt estiver pressionado:

- Um botão do mouse é pressionado (clique esquerdo ou direito).
- Qualquer outra tecla é pressionada.

Isso garante que os gestos de arrastar Alt (como o ajuste do tamanho do pincel) e os atalhos modificados por Alt funcionem sem alterar acidentalmente a camada ativa.

### Limitações

- A seleção de camadas não é ativada durante as operações da ferramenta **Transformar** — Alt tem um significado diferente aqui.
- A seleção não ocorre se uma seleção flutuante estiver presente.
- Somente seleção de gatilhos Alt esquerdos; Alt direito é tratado como um modificador padrão.

## Operações

No painel Camadas:

- **Criar camada**: clique com o botão direito → **Nova camada** ou use o menu **Camada**.
- **Duplicar**: clique com o botão direito → **Duplicar** ou **Camada** → **Duplicar**.
- **Excluir**: clique com o botão direito → **Excluir** ou selecione e pressione **Excluir**.
- **Reordenar**: arraste as camadas para cima ou para baixo para alterar a ordem de empilhamento.
- **Renomear**: Clique duas vezes no nome da camada.
- **Mesclar**: Clique com o botão direito → **Mesclar** para combinar com a camada abaixo.
- **Achatar imagem**: **Imagem** → **Achatar imagem** para mesclar todas as camadas visíveis.

## Propriedades da Camada

- **Opacidade**: 0–100%, controla a transparência geral da camada.
- **Modo de mesclagem**: menu suspenso para selecionar como a camada se combina com as camadas abaixo.
- **Visível/Oculto**: o ícone do olho alterna a visibilidade da camada.

## Bloqueios de camada

Os ícones de bloqueio são mostrados na linha do cabeçalho do painel Camadas. Cada bloqueio pode ser alternado de forma independente. Clicar com o botão direito em um ícone de cadeado o define como exclusivo (bloqueia apenas aquele tipo, desbloqueando todos os outros na mesma camada).

- **Lock Alpha**: Evita pintura em áreas transparentes. As pinceladas afetam apenas os pixels que já possuem opacidade; pixels totalmente transparentes não são modificados. Útil para pintar dentro de formas existentes sem derramar fora delas.

- **Bloquear Máscara**: Impede a edição da máscara da camada. A máscara permanece visível e ativa, mas não pode ser pintada ou modificada enquanto este bloqueio estiver ativado.

- **Bloquear cor**: bloqueia a pintura em uma cor específica — a cor de primeiro plano atual no momento em que o bloqueio é aplicado. Os traços subsequentes nesta camada usam a cor armazenada, independentemente da cor de primeiro plano ativa. O desbloqueio descarta a cor armazenada.

- **Bloquear Conteúdo** (Bloquear Pixels): Impede todas as edições de pixels na camada. A camada não pode ser pintada, preenchida, transformada ou modificada de outra forma. Útil para proteger camadas acabadas.

- **Posição de bloqueio**: Evita que a camada seja movida ou transformada. A camada ainda pode ser editada; apenas alterações de posição (ferramenta Mover, ferramenta Transformar) são bloqueadas.

- **Bloquear visibilidade**: evita que o ícone do olho alterne a visibilidade da camada. Protege camadas que devem permanecer sempre visíveis (ou ocultas) durante a edição.

Todos os bloqueios são salvos com o projeto e persistem nas sessões.

## Efeitos de camada (fx)

Os filtros GEGL não destrutivos aplicados por meio do menu **Filtros** são armazenados como efeitos confirmados na camada, em vez de modificar pixels imediatamente. Quando uma camada tem pelo menos um efeito confirmado, um ícone **fx** aparece no painel Camadas próximo a essa camada.### Acessando o pop-up de efeitos

Clique no ícone **fx** em uma linha de camada no painel Camadas para abrir o popover **Efeitos de camada** dessa camada.

O popover exibe a pilha de filtros da camada – cada efeito confirmado listado por nome com um botão de visibilidade ao lado dele.

### Controles

- **Alteração do olho de visibilidade** (parte superior do pop-up): ativa ou desativa todos os efeitos simultaneamente.
- **Alternância de visibilidade por filtro**: cada linha de filtro tem seu próprio ícone de olho para ativar ou desativar esse efeito de forma independente.
- **Editar**: Abre a caixa de diálogo de configurações do filtro selecionado, permitindo que seus parâmetros sejam ajustados de forma não destrutiva.
- **Aumentar / Diminuir**: Move o filtro selecionado para cima ou para baixo na pilha, alterando a ordem em que os efeitos são aplicados.
- **Mesclar**: Confirma todos os efeitos atualmente visíveis nos pixels da camada, tornando as alterações permanentes. O ícone fx é removido se todos os efeitos forem mesclados. A mesclagem não está disponível em camadas de grupo.
- **Remover**: Exclui totalmente o filtro selecionado. O popover fecha automaticamente se nenhum efeito permanecer.

Clicar duas vezes em um filtro na lista também abre sua caixa de diálogo de edição.

**Editar** e **Remover** serão bloqueados se Bloquear Pixels estiver ativo na camada. Os filtros não podem ser reordenados enquanto um deles estiver sendo editado ativamente.

### Adicionando efeitos

Aplique um filtro de **Filtros** → (qualquer categoria). Se a camada ativa for direcionada e a operação for executada de forma não destrutiva, o resultado será armazenado como um efeito de camada, em vez de incorporado aos dados de pixel. O ícone fx aparece na camada quando pelo menos um efeito está presente.

## Caixa de diálogo de atributos de camada

Clique duas vezes em uma camada no painel Camadas para abrir a caixa de diálogo Atributos da camada.

### Identidade

- **Etiqueta de cor**: etiqueta colorida para organização visual no painel Camadas.

### Espaço e modo composto

- **Espaço composto**: O espaço de cores usado ao compor esta camada com as camadas abaixo. Opções: Automático, Linear (RGB), Perceptual (RGB).
- **Modo composto**: controla como a camada alfa interage com o pano de fundo. As opções incluem União (afeta todas as áreas — o padrão para o modo Normal), Cortar para Pano de Fundo (afeta apenas áreas com conteúdo existente — o padrão para a maioria dos outros modos de mesclagem) e Interseção.

### Tamanho e deslocamentos

Para uma camada existente, **Tamanhos** mostra as dimensões da camada e da máscara (se uma máscara estiver anexada) como rótulos somente leitura.

**Deslocamentos de camada** — controles giratórios X e Y que controlam a posição da camada na tela. As alterações são aplicadas imediatamente e não no fechamento da caixa de diálogo.

Se a camada tiver uma máscara, **Deslocamentos de máscara** — controles giratórios X e Y para a posição independente da máscara — são mostrados abaixo.

Ao criar uma nova camada, os campos Largura e Altura e um menu suspenso **Preencher com** (Primeiro plano, Plano de fundo, Branco, Transparente) substituem a exibição do tamanho somente leitura.

### Atributos de Camada (Parasitas Persistentes)

A seção inferior da caixa de diálogo contém uma tabela rolável de Nome/Valor para parasitas persistentes — metadados de valores-chave arbitrários anexados à camada. Esses valores são armazenados com o projeto e podem ser acessados ​​na interface de script do esquema.

- Clique em qualquer célula na coluna Nome ou Valor para editá-la in-line.
- **Adicionar**: acrescenta uma nova linha vazia.
- **Excluir**: Remove a linha selecionada e seu parasita da camada.

Se a camada não tiver parasitas persistentes, serão mostradas três linhas iniciais vazias.

### Estado do conteúdoUma linha de informações somente leitura na parte inferior mostra o estado atual do conteúdo da camada (e da máscara, se presente): **Limpar**, **Uniforme** ou **Misto**. Um prefixo `*` indica que a camada tem alterações não salvas desde o último salvamento.

## Desempenho

- **Modo Rápido**: Ao pintar em uma única camada aninhada dentro de um grupo, o Lumi alterna temporariamente os grupos ancestrais para a renderização de passagem durante o traço, ignorando a recomposição completa da projeção do grupo. Isso elimina o atraso de atualização da projeção aninhada durante a tintagem e a pintura. A composição completa é retomada quando o traço termina, a camada ativa muda ou antes de salvar.

  O modo rápido é desativado quando qualquer uma das seguintes condições se aplica a um grupo ancestral:
  - O grupo possui filtros não destrutivos visíveis (os filtros precisam do buffer de projeção).
  - O modo de mesclagem do grupo é diferente de **Normal** ou **Passagem**.
  - O grupo tem um filho direto usando o modo composto **Clip to Backdrop** ou **Intersection** (eles exigem dados de pano de fundo do buffer de projeção).

  O modo rápido também não é ativado para camadas de nível superior, seleções flutuantes ou quando várias camadas são direcionadas simultaneamente.

  Estruturar arquivos para evitar essas condições em grupos de pintura, usando modos de mesclagem Normal em camadas, garante que o modo rápido permaneça ativo durante uma sessão de tinta ou pintura.
- **Carregamento lento**: projetos grandes carregam rapidamente; os dados da camada são carregados apenas quando necessário (por exemplo, quando ficam visíveis ou pintados).

## Formato de arquivo

Todas as camadas, máscaras e propriedades são armazenadas no formato aberto `.lum` do Lumi. O arquivo é um diretório que contém buffers de camada individuais e metadados, garantindo compatibilidade e acessibilidade a longo prazo.