---
title: "Ferramenta Pincel"
type: docs
url: "hub/features/paintbrush"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 467281fdb592ef90257d2dc951c59516e623a5ae34360c4be446a8d487529330
---

A ferramenta Pincel é o principal instrumento de pintura do Lumi: uma forma responsiva e expressiva de desenhar, pintar, sombrear, texturizar e construir marcas diretamente na tela. Ele foi projetado para parecer imediato e, ao mesmo tempo, dar aos artistas espaço para moldar o comportamento de um traço.

Em vez de ser um único pincel fixo, atua como um sistema de pintura. A forma, a textura, o grão de superfície, o movimento, a pressão, o tempo e a cor podem contribuir para a marca final, o que o torna adequado para linhas limpas, pintura suave, mídia seca, traços caligráficos, texturas dispersas, tufos de cerdas e formações de várias cabeças.

## Marcas de pincel expressivas

Os pincéis podem ser baseados em carimbos de bitmap, formas procedurais ou fontes animadas baseadas em quadros. Isso permite que um traço varie de uma simples marca redonda e suave até uma cabeça de pincel ricamente texturizada ou em evolução. O mesmo mecanismo de pintura pode suportar desenhos precisos, acúmulo de pintura, marcas decorativas e separação de estilo de mídia natural.

Quando um pincel se torna visualmente complexo, a visualização pode permanecer simplificada para que a pintura permaneça responsiva e fácil de ler.

## Dinâmica e resposta de entrada

A ferramenta Pincel responde a entradas ao vivo, como pressão da caneta, velocidade, direção, inclinação e outros valores do controlador. Esses sinais podem influenciar o traço visível de várias maneiras: espessura, opacidade, ângulo, resposta da textura, comportamento da cor, espaçamento e outras qualidades podem mudar à medida que a mão se move.

Isso faz com que o pincel pareça menos um padrão estampado e mais um instrumento de desenho físico. Um toque leve pode produzir marcas delicadas, um movimento mais rápido pode revelar textura ou forma e um comportamento sensível à direção pode ajudar os traços a seguir o gesto da mão.

## Comportamento do traço

Os traços podem ser diretos e imediatos ou podem ser auxiliados por suavização e estabilização. Esses recursos ajudam a reduzir oscilações indesejadas, suavizar mudanças abruptas e fazer com que movimentos mais longos pareçam mais controlados, sem remover o caráter da entrada do artista.

O Pincel também oferece suporte a diferentes abordagens para o acúmulo de tinta. Ele pode se comportar como um traço contínuo, acumular pinceladas repetidas ou emitir marcas ao longo do tempo enquanto o ponteiro é mantido no lugar. Essa flexibilidade o torna útil tanto para trabalhos de linha deliberados quanto para construções tonais mais lentas.

Para marcas caligráficas ou semelhantes a tinta, o pincel pode gerar um traço de formato mais contínuo, em vez de depender apenas de carimbos repetidos. Isso produz formas fluidas em forma de fita que respondem naturalmente ao gesto e à velocidade.

## Captura de traços e renderização simulada

O pincel pode capturar uma pequena amostra de como uma predefinição normalmente é desenhada à mão e, em seguida, usar esse perfil ao renderizar traços definidos pela geometria em vez de movimento ao vivo. Linhas retas de clique com Shift, caminhos traçados e seleções traçadas podem usar o padrão de pressão e velocidade capturado da predefinição de ferramenta ativa em vez de se comportar como uma linha mecânica plana.

Isso mantém os traços construídos mais próximos do caráter do pincel. Uma linha desenhada a partir de um caminho pode começar suavemente, criar pressão, diminuir gradualmente ou variar a resposta de velocidade da mesma maneira ampla que o traço de mão amostrado, enquanto ainda segue a forma exata do caminho, borda de seleção ou gesto de linha reta.

## Pós-processamento

O pincel pode registrar um traço conforme você o desenha e, em seguida, reproduzir o gesto capturado quando você levanta a caneta, refinando o caminho antes que a marca final seja estabelecida. Você pode esboçar livremente e ainda assim chegar a uma direção mais limpa, cantos mais nítidos ou uma estrutura mais deliberada sem precisar desenhar com precisão mecânica.

Isso abre marcas de construção hachuradas e pautadas que se encaixam em ângulos limpos, mantendo o comprimento e o caráter desenhados à mão, traços de fita com inclinação estável e reprodução com reconhecimento de canto que trata curvas e linhas retas de maneira diferente. Pincéis com múltiplas cabeças podem compartilhar um caminho corrigido enquanto cada cabeça mantém sua própria variação, e a dinâmica ainda pode moldar o traço ao longo de sua curva final durante a reprodução. O pós-processamento aplica-se a traços desenhados em vez de emissão contínua de aerógrafo.

## Cor e textura

As pinceladas podem usar a cor de tinta ativa, responder a gradientes ou variar a cor por meio da dinâmica. O manuseio de textura permite que o pincel alterne entre cobertura sólida e marcas quebradas de superfície, o que é útil para efeitos de pincel seco, granulação e sombreamento expressivo.

Como a cor e a textura podem fazer parte do mesmo sistema dinâmico que a forma e a opacidade, um único traço pode evoluir à medida que se move pela tela, em vez de permanecer visualmente uniforme.

A textura na ponta do pincel fragmenta o próprio carimbo. A textura de superfície, descrita a seguir, é o grão e o dente da superfície que o traço encontra.

## Textura de superfície

A textura de superfície dá a um traço o grão e o dente de uma superfície: papel, tela ou outro padrão. O Lumi inclui grãos adequados a isso. As áreas claras do padrão deixam a tinta passar. As áreas escuras a seguram, e a marca se quebra como a mídia seca se quebra sobre o dente.

O lugar em que esse grão fica pode mudar com o traço. Cada traço pode começar no próprio ponto do padrão, e uma virada pode rolar um deslocamento novo. O sombreamento de vai e vem é onde isso aparece: cada inversão toma uma fase nova do dente, e as passagens sobrepostas constroem um sombreamento quebrado e realista. Uma curva gradual pode manter a fase que já está sob o traço, para que o dente continue coerente ao longo do gesto.

Pincéis carimbados e traços caligráficos podem receber esse grão. Uma fileira de pinceladas e uma fita contínua de tinta podem se quebrar sobre o mesmo tipo de dente. Num traço caligráfico a linha continua sendo uma marca moldada, e o grão decide onde essa fita está aberta e onde fica retida.

O quanto o dente está aberto pode mudar ao longo do traço. A pressão e outras entradas podem ir de uma leitura mais seca a uma mais cheia. Na leitura mais seca os buracos continuam limpos. Conforme o traço se abre, as ilhas pintadas podem crescer para dentro desses buracos, de modo que o grão se preenche a partir das bordas. Na leitura plena o traço corresponde ao padrão mais aberto.

Pinceladas sobrepostas dentro de um mesmo traço podem acumular opacidade nas ilhas. Também podem depositar uma única folha de cobertura que uma volta posterior desse traço reescreve, para que o grão não se empilhe sobre si mesmo.

## Cabeças e formações de escova

A ferramenta Pincel pode pintar com mais de uma cabeça por vez. Várias cabeças podem ser dispostas ao redor do caminho do traço para criar marcas de pontas, traços em leque, comportamento semelhante a cerdas, padrões de pulverização, formações texturizadas ou hachuras estruturadas.

Essas cabeças podem seguir a direção do deslocamento, variar umas das outras e se espalhar de maneiras que fazem com que o golpe pareça orgânico, em vez de repetido mecanicamente. Isso é especialmente útil para pincéis de mídia natural, traços decorativos, folhagens, pêlos, hachuras e outras marcas que se beneficiam de irregularidades controladas.

Pincéis gerados também podem ser construídos como campos de cerdas. A forma e a animação ao longo do traço desses campos estão descritas abaixo.

## Forma das cerdas

Um pincel de cerdas é um campo de marcas separadas, preso dentro de uma forma gerada. O contorno decide onde as cerdas podem ficar: círculo, quadrado, losango, triângulo, forma com espinhos, cantos suavizados ou uma proporção alongada. Cada cerda guarda a própria marca dentro desse contorno.

O espaçamento e o tamanho definem a compactação e o tamanho dos fios. A variação de tamanho vai de uma cabeça uniforme a fios que descem do tamanho cheio até perto do vazio. A regularidade move o campo entre uma grade uniforme e uma dispersão mais solta. O agrupamento puxa as cerdas para menos centros, de modo que a cabeça se apresenta em tufos. O campo pode ficar mais ralo.

As pontas podem continuar redondas ou virar pequenos polígonos dentro de cada cerda, espaçados de modo uniforme ou deixados irregulares, na borda ou puxados para dentro. Onde as pontas se sobrepõem, a marca mais forte é a que aparece.

Cada cabeça de um pincel de várias cabeças pode levar o próprio arranjo, um leque de tufos aparentados.

O tamanho das cerdas pode seguir a pressão da caneta e outras entradas. Conforme o tamanho muda, a cabeça pode ser redesenhada, e um toque leve e uma pressão firme diferem nos próprios fios.

## Cerdas ao longo do traço

Um pincel de cerdas pode mudar enquanto o traço é desenhado. As cerdas crescem e encolhem. Algumas se apagam e outras crescem em outro ponto, e o caráter percorre a marca. A densidade continua estável.

A animação avança com as pinceladas. Pode começar depois de uma distância, ou quando a mão vira. Os estados intermediários percorrem o caminho. O mesmo gesto produz a mesma sequência.

Um aerógrafo deposita um arranjo novo de cerdas conforme emite, e um traço parado continua renovando o spray. Um traço caligráfico conserva a ponta contínua, que ainda pode receber textura de superfície. A animação das cerdas pertence ao pincel de cerdas carimbado.

## Carregamento de pincel e coleta de tinta

O Pincel também pode simular a quantidade de tinta ou material que está atualmente carregado no pincel. À medida que um traço continua, essa carga pode diminuir gradualmente, deixando as marcas ficarem mais claras, mais secas, mais finas, mais ásperas ou mais fragmentadas, dependendo de como a dinâmica do pincel é definida.

A carga pode ser reintroduzida entre as pinceladas, mantida em um nível escolhido ou usada como um sinal de controle ao vivo para outros comportamentos da escova. Isso torna possível construir pincéis que se parecem mais com mídia real: molhados no início de uma pincelada, esgotados progressivamente ao longo da distância e depois mergulhados novamente para a próxima passagem.

## Contato com a superfície da escova

O pincel também pode simular a perda intermitente de contato com a superfície da pintura – as marcas quebradas que aparecem quando um lápis, um bastão de carvão, um pincel seco ou um marcador parcialmente esgotado se envolvem apenas parcialmente no papel.

Quando a simulação de contato está habilitada, a escova está em contato ou levantada. Durante o contato, as marcas se depositam normalmente. Durante a elevação, nenhum material é depositado e o curso deixa uma lacuna cujo comprimento é escolhido aleatoriamente entre uma distância mínima e máxima. A transição é binária: o efeito não altera a opacidade, o tamanho, a dureza, o espaçamento ou o fluxo – apenas se a tinta estiver aplicada.

A facilidade com que o contato é perdido é determinada pelo limite de contato, pela pressão da ponta e, opcionalmente, pela carga da escova. Valores de limite mais altos tornam as pausas mais frequentes. A pressão atua como uma força estabilizadora: uma leve pressão aumenta a chance de perda de contato, enquanto uma pressão firme aumenta a probabilidade de o golpe permanecer para baixo. Quando a carga da escova está ativada, a carga baixa pode tornar a marca mais quebrada e a carga alta pode ajudar a manter o contato, semelhante a uma ferramenta que ainda carrega material suficiente para aderir à superfície.

A perda é avaliada a partir da distância percorrida pelo traço em vez da contagem de salpicos, de modo que pincéis com espaçamento denso ou esparso se comportam de forma consistente. O recurso funciona com renderização baseada em carimbo e caligráfica, produzindo lacunas coerentes ao longo do traço, em vez de salpicos ignorados isolados.

## Animação e variação

As fontes de pincéis animados podem mudar de quadro à medida que o traço avança, dando aos pincéis uma sensação de movimento e variedade. A randomização e a variação por traço podem evitar que marcas repetidas pareçam idênticas, enquanto a propagação estável pode preservar um caráter consistente quando a repetibilidade é necessária.

Esses comportamentos são úteis para pincéis que devem parecer vivos: carimbos texturizados que mudam sutilmente com o tempo, ou ferramentas de várias cabeças em que cada cabeça tem a própria personalidade. Os pincéis de cerdas têm a própria animação ao longo do traço, descrita acima.

## Traços OpenGL

Uma predefinição de ferramenta pode pintar os traços com OpenGL. A diferença aparece sobretudo nos aerógrafos grandes e nos carimbos de textura grandes, que acompanham a mão conforme a marca cresce. A janela da imagem também pode desenhar a pilha de camadas com OpenGL, o que mantém a navegação na tela fluida. Os dois estão descritos em [OpenGL](/hub/features/opengl/).

## Fluxo de trabalho focado no artista

A ferramenta Pincel é organizada para que as decisões comuns de pintura fiquem à mão, enquanto as opções de configuração menos frequentes permanecem fora do caminho. A intenção é manter a ferramenta acessível durante a pintura e, ao mesmo tempo, oferecer suporte à personalização profunda do design do pincel.

No geral, o pincel foi desenvolvido para cobrir tanto a pintura cotidiana quanto a marcação especializada: esboços rápidos, ilustrações refinadas, renderização texturizada, trabalho de tinta expressivo e efeitos de pincel procedimentais complexos, todos compartilham a mesma base flexível.