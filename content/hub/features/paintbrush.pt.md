---
title: "Ferramenta Pincel"
type: docs
translation_provenance: ai-reviewed
translation_source_sha256: b11c0248addcd834613763c198cf5d00356b8d31e99d3dad18a1cea96e6b7e3f
url: "hub/features/paintbrush"
translation_lock: true
---
A ferramenta Pincel é o instrumento central de pintura do Lumi: uma forma responsiva e expressiva de desenhar, pintar, sombrear, texturizar e construir marcas directamente na tela. Foi concebida para parecer imediata, dando ao artista margem para moldar o comportamento de um traço.

Em vez de ser um único pincel fixo, actua como um sistema de pintura. A forma, a textura, o grão de superfície, o movimento, a pressão, o tempo, a cor e a plenitude com que o pincel encontra a superfície podem contribuir para a marca final, tornando-o adequado a linhas limpas, pintura suave, media seca, traços caligráficos, texturas dispersas, tufos de cerdas e formações multi-cabeça.

## Marcas expressivas

Os pincéis podem basear-se em carimbos bitmap, formas procedurais ou fontes animadas por frames. Isto permite que um traço vá de uma marca redonda suave simples a uma cabeça de pincel ricamente texturizada ou em evolução. O mesmo motor de pintura suporta desenho preciso, acumulação pictórica, marcas decorativas e quebra de estilo media natural.

Quando um pincel se torna visualmente complexo, a pré-visualização pode permanecer simplificada para que a pintura se mantenha responsiva e legível.

## Dinâmicas e resposta de entrada

A ferramenta Pincel responde a entrada em tempo real, como pressão do stylus, velocidade, direcção, inclinação e outros valores do controlador. Estes sinais podem influenciar o traço visível de muitas formas: espessura, opacidade, ângulo, resposta de textura, comportamento de cor, espaçamento e outras qualidades podem mudar à medida que a mão se move.

Isto faz o Pincel parecer menos um padrão carimbado e mais um instrumento de desenho físico. Um toque leve produz marcas delicadas, movimento mais rápido pode revelar textura ou forma, e comportamento sensível à direcção ajuda os traços a seguir o gesto da mão.

## Comportamento do traço

Os traços podem ser directos e imediatos, ou assistidos por suavização e estabilização. Estas funcionalidades ajudam a reduzir tremor indesejado, suavizar mudanças bruscas e tornar movimentos longos mais controlados sem remover o carácter da entrada do artista.

O Pincel também suporta diferentes abordagens à acumulação de tinta. Pode comportar-se como traço contínuo, acumular pinceladas repetidas ou emitir marcas ao longo do tempo enquanto o cursor permanece no lugar. Esta flexibilidade torna-o útil tanto para linha deliberada como para construção tonal mais lenta.

Para marcas caligráficas ou tipo tinta, o Pincel pode gerar um traço contínuo perfilado, em vez de depender apenas de carimbos repetidos. Isto produz formas fluidas em fita que respondem naturalmente ao gesto e à velocidade.

## Captura de traço e renderização simulada

O Pincel pode capturar uma pequena amostra de como uma predefinição é normalmente desenhada à mão e usar esse perfil ao renderizar traços definidos por geometria em vez de movimento ao vivo. Linhas rectas com Shift+clique, caminhos traçados e selecções traçadas podem usar o padrão de pressão e velocidade capturado da predefinição activa, em vez de se comportarem como linha mecânica plana.

Isto mantém traços construídos mais próximos do carácter do pincel. Uma linha desenhada a partir de um caminho pode começar suavemente, ganhar pressão, afilar ou variar a resposta de velocidade da mesma forma ampla que o traço manual amostrado, seguindo a forma exacta do caminho, borda de selecção ou gesto de linha recta.

## Pós-processamento

O pincel pode registar um traço enquanto se desenha e reproduzir o gesto capturado ao levantar, refinando o caminho antes de assentar a marca final. É possível esboçar livremente e ainda obter direcção mais limpa, cantos mais nítidos ou estrutura mais deliberada sem desenhar com precisão mecânica.

Isto abre hachuras e marcas de construção pautadas que encaixam em ângulos limpos mantendo comprimento e carácter desenhados à mão, traços em fita estáveis à inclinação e reprodução consciente de cantos que trata curvas e rectas de forma diferente. Pincéis multi-cabeça podem partilhar um caminho corrigido enquanto cada cabeça mantém a sua variação, e as dinâmicas ainda podem moldar o traço ao longo da curva final durante a reprodução. O pós-processamento aplica-se a traços desenhados, e não à emissão contínua de aerógrafo.

## Cor e textura

Pinceladas podem usar a cor activa, responder a gradientes ou variar a cor através de dinâmicas. O tratamento de textura permite alternar entre cobertura sólida e marcas quebradas à superfície, útil para pincel seco, grão e sombreado expressivo.

Como cor e textura podem fazer parte do mesmo sistema dinâmico que forma e opacidade, um único traço pode evoluir ao mover-se pela tela, em vez de permanecer visualmente uniforme.

A textura na ponta do pincel fragmenta o próprio carimbo. A textura de superfície, descrita de seguida, é o grão e o dente da superfície que o traço encontra.

## Textura de superfície

A textura de superfície confere a um traço o grão e o dente de uma superfície: papel, tela ou outro padrão. O Lumi inclui grãos próprios para isso. As zonas claras do padrão deixam passar a tinta. As zonas escuras retêm-na, pelo que a marca se quebra como a media seca se quebra sobre o dente.

O lugar em que esse grão assenta pode mudar com o traço. Cada traço pode começar no seu próprio ponto do padrão, e uma viragem pode rolar um novo deslocamento. O sombreado de vai-e-vem é onde isto se vê: cada inversão toma uma fase nova do dente, e as passagens sobrepostas constroem um sombreado quebrado e realista. Uma curva gradual pode manter a fase que já está sob o traço, para que o dente se mantenha coerente ao longo do gesto.

Pincéis carimbados e traços caligráficos podem ambos receber este grão. Uma fila de pinceladas e uma fita contínua de tinta podem quebrar-se sobre o mesmo tipo de dente. Num traço caligráfico a linha permanece uma marca perfilada, e o grão decide onde essa fita está aberta e onde é retida.

A abertura do dente pode mudar ao longo do traço. A pressão e outras entradas podem mover-se entre uma leitura mais seca e uma mais cheia. Na leitura mais seca os buracos mantêm-se limpos. À medida que o traço se abre, as ilhas pintadas podem crescer para dentro desses buracos, pelo que o grão se preenche a partir das bordas. Na leitura plena o traço corresponde ao padrão mais aberto.

Pinceladas sobrepostas dentro de um mesmo traço podem acumular opacidade nas ilhas. Também podem assentar uma única folha de cobertura que uma volta posterior desse traço reescreve, para que o grão não se empilhe sobre si próprio.

## Cabeças e formações de pincel

A ferramenta Pincel pode pintar com mais de uma cabeça de cada vez. Várias cabeças podem dispor-se em torno do caminho do traço para criar marcas de pena, traços em leque, comportamento tipo cerda, padrões de spray, formações texturizadas ou hachuras estruturadas.

Estas cabeças podem seguir a direcção do movimento, variar entre si e dispersar-se de formas que fazem o traço parecer orgânico em vez de repetido mecanicamente. Isto é especialmente útil para pincéis de media natural, traços decorativos, folhagem, pelo, hachuras e outras marcas que beneficiam de irregularidade controlada.

Os pincéis gerados também podem ser construídos como campos de cerdas. A forma desses campos, e a forma como se movem ao longo de um traço, estão descritas abaixo.

## Forma das cerdas

Um pincel de cerdas é um campo de marcas separadas, contido numa forma gerada. O contorno decide onde as cerdas podem assentar: círculo, quadrado, losango, triângulo, forma espigada, cantos suavizados ou uma proporção alongada. Cada cerda conserva a sua própria marca dentro desse contorno.

O espaçamento e o tamanho definem a compactação e o tamanho dos fios. A variação de tamanho vai de uma cabeça uniforme a fios que descem do tamanho pleno até perto do vazio. A regularidade move o campo entre uma grelha uniforme e uma dispersão mais solta. O agrupamento puxa as cerdas para menos centros, de modo que a cabeça se apresenta em tufos. O campo pode ser rarefeito.

As pontas podem manter-se redondas ou tornar-se pequenos polígonos dentro de cada cerda, espaçados de forma uniforme ou deixados irregulares, assentes na borda ou puxados para dentro. Onde as pontas se sobrepõem, vê-se a marca mais forte.

Cada cabeça de um pincel multi-cabeça pode ter o seu próprio arranjo, um leque de tufos aparentados.

O tamanho das cerdas pode seguir a pressão do stylus e outras entradas. À medida que o tamanho muda, a cabeça pode ser redesenhada, pelo que um toque leve e uma pressão firme diferem nos próprios fios.

## Cerdas ao longo do traço

Enquanto um traço é desenhado, os fios podem mover-se como um tufo vivo. Acompanham a mão com atraso, balançam numa viragem e assentam na nova direcção, empurram quando o traço inverte, torcem-se com a rotação do pincel, abrem sob pressão e voltam de mola para o arranjo em repouso. Os fios que ficam para trás podem passar o contorno em repouso.

A distância que percorrem, o aperto com que os tufos se mantêm e a rapidez com que assentam moldam-se livremente. Um tufo rígido pode ficar numa só pista. Um mais macio pode fender-se, atrasar uma torção e guardar um pouco da curva antes de regressar. O mesmo gesto produz o mesmo movimento.

Um aerógrafo assenta um arranjo novo de cerdas à medida que emite, pelo que um traço parado continua a renovar o spray. Um traço caligráfico conserva a sua pena contínua, que ainda pode receber textura de superfície. Este movimento vivo pertence ao pincel de cerdas carimbado.

## Gradientes de bordo

Um traço pode ter um lado nítido e outro esfumado, como um carvão ou um pastel. Para o lado suave a tinta pode desvanecer, a ponta secar e partir, e o dente do papel aparecer mais. As cerdas podem adelgaçar e cair aí, e uma formação de cabeças manter-se apertada e sólida no lado duro, enquanto o lado suave se solta e se espalha.

A queda desse gradiente, e a direcção para que olha, moldam-se livremente. Pode virar com o traço, ou manter-se na inclinação do stylus, de modo que um sombreado de vai-e-vem conserve o mesmo bordo duro.

## Carga de pincel e recolha de tinta

O Pincel também pode simular quanta tinta ou material está actualmente na escova. À medida que o traço continua, essa carga pode esgotar-se gradualmente, deixando marcas mais claras, mais secas, mais finas, mais ásperas ou mais quebradas, consoante a dinâmica do pincel.

A carga pode ser reintroduzida entre traços, mantida num nível escolhido ou usada como sinal de controlo em tempo real para outros comportamentos do pincel. Isto torna possível construir pincéis que parecem media real: húmidos no início do traço, progressivamente esgotados ao longo da distância e depois mergulhados de novo para a passagem seguinte.

## Contacto do pincel

O contacto do pincel é a forma como um traço encontra o papel e depois o perde. Ao longo do percurso a marca pode inchar e estreitar, levantar-se num trecho e voltar a assentar. Um lápis, um carvão, um pincel seco ou um marcador cansado podem engrossar, falhar e voltar a agarrar.

A facilidade com que isso acontece pode seguir a mão. Um toque leve interrompe mais cedo. Uma pressão firme fica mais tempo assente. Marcas mais grossas podem manter o contacto enquanto as mais finas abrem buracos, e um estreitamento pode ser o que faz um traço pesado começar a falhar. O levantamento pode ser súbito, ou a marca pode encolher para o intervalo e voltar a crescer ao sair dele.

Um pincel em plena carga pode encontrar o papel de outra maneira. O inchar e o estreitar correm mais fortes, e a marca mantém o contacto em trechos mais longos. À medida que a carga se esgota, essa forma de encontro desvanece, e o traço fica mais quieto e mais disposto a falhar.

Um tufo de cerdas pode fazê-lo cerda a cerda, de modo que o traço se desfia em fios separados e depois se reúne. As cabeças de uma formação podem quebrar juntas, ou cada uma por si. Um traço caligráfico levanta-se como uma só ponta, e a fita termina onde termina o contacto. Cada traço novo encontra o seu próprio ritmo.

## Animação e variação

Fontes de pincel animadas podem mudar de frame à medida que o traço avança, dando aos pincéis sensação de movimento e variedade. Aleatoriedade e variação por traço evitam que marcas repetidas pareçam idênticas, enquanto semente estável preserva carácter consistente quando a repetibilidade é necessária.

Estes comportamentos são úteis para pincéis que devem parecer vivos: carimbos texturizados que mudam subtilmente com o tempo, ou ferramentas multi-cabeça em que cada cabeça tem a sua personalidade. Os pincéis de cerdas têm a sua própria animação ao longo do traço, descrita acima.

## Traços OpenGL

Uma predefinição de ferramenta pode pintar os seus traços com OpenGL. A diferença nota-se sobretudo nos aerógrafos grandes e nos carimbos de textura grandes, que acompanham a mão à medida que a marca cresce. A janela da imagem também pode desenhar a pilha de camadas com OpenGL, o que mantém fluida a navegação na tela. Ambos estão descritos em [OpenGL](/hub/features/opengl/).

## Fluxo orientado para o artista

A ferramenta Pincel está organizada para manter decisões comuns de pintura à mão, enquanto opções de configuração menos frequentes permanecem afastadas. A intenção é manter a ferramenta acessível durante a pintura, suportando personalização profunda do design de pincéis.

No conjunto, o Pincel cobre pintura quotidiana e marcação especializada: esboço rápido, ilustração polida, renderização texturizada, trabalho de tinta expressivo e efeitos procedurais complexos partilham a mesma base flexível.
