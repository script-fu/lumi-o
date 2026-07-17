---
title: "Ferramenta Pincel"
type: docs
translation_provenance: ai-reviewed
translation_source_sha256: a37df7a3325c5a6028907f9584d45fd23746dd345b2d649f0a3ff5c1e03ed657
url: "hub/features/paintbrush"
translation_lock: true
---
A ferramenta Pincel é o instrumento central de pintura do Lumi: uma forma responsiva e expressiva de desenhar, pintar, sombrear, texturizar e construir marcas directamente na tela. Foi concebida para parecer imediata, dando ao artista margem para moldar o comportamento de um traço.

Em vez de ser um único pincel fixo, actua como um sistema de pintura. Forma, textura, movimento, pressão, tempo e cor do pincel podem contribuir para a marca final, tornando-a adequada a linha limpa, pintura suave, efeitos de media seca, traços caligráficos, texturas dispersas e formações multi-cabeça.

![brush-tool](/images/screens/brush-tool.jpg)

## Marcas expressivas

Os pincéis podem basear-se em carimbos bitmap, formas procedurais ou fontes animadas por frames. Isto permite que um traço vá de uma marca redonda suave simples a uma cabeça de pincel ricamente texturizada ou em evolução. O mesmo motor de pintura suporta desenho preciso, acumulação pictórica, marcas decorativas e quebra de estilo media natural.

Quando um pincel se torna visualmente complexo, a pré-visualização pode permanecer simplificada para que a pintura se mantenha responsiva e legível.

![tool-setup](/images/screens/tool-setup.jpg)


## Dinâmicas e resposta de entrada

A ferramenta Pincel responde a entrada em tempo real, como pressão do stylus, velocidade, direcção, inclinação e outros valores do controlador. Estes sinais podem influenciar o traço visível de muitas formas: espessura, opacidade, ângulo, resposta de textura, comportamento de cor, espaçamento e outras qualidades podem mudar à medida que a mão se move.

Isto faz o Pincel parecer menos um padrão carimbado e mais um instrumento de desenho físico. Um toque leve produz marcas delicadas, movimento mais rápido pode revelar textura ou forma, e comportamento sensível à direcção ajuda os traços a seguir o gesto da mão.

![dynamics](/images/screens/dynamics.jpg)

## Comportamento do traço

Os traços podem ser directos e imediatos, ou assistidos por suavização e estabilização. Estas funcionalidades ajudam a reduzir tremor indesejado, suavizar mudanças bruscas e tornar movimentos longos mais controlados sem remover o carácter da entrada do artista.

O Pincel também suporta diferentes abordagens à acumulação de tinta. Pode comportar-se como traço contínuo, acumular pinceladas repetidas ou emitir marcas ao longo do tempo enquanto o cursor permanece no lugar. Esta flexibilidade torna-o útil tanto para linha deliberada como para construção tonal mais lenta.

Para marcas caligráficas ou tipo tinta, o Pincel pode gerar um traço contínuo perfilado, em vez de depender apenas de carimbos repetidos. Isto produz formas fluidas em fita que respondem naturalmente ao gesto e à velocidade.

![stroke](/images/screens/stroke.jpg)

## Captura de traço e renderização simulada

O Pincel pode capturar uma pequena amostra de como uma predefinição é normalmente desenhada à mão e usar esse perfil ao renderizar traços definidos por geometria em vez de movimento ao vivo. Linhas rectas com Shift+clique, caminhos traçados e selecções traçadas podem usar o padrão de pressão e velocidade capturado da predefinição activa, em vez de se comportarem como linha mecânica plana.

Isto mantém traços construídos mais próximos do carácter do pincel. Uma linha desenhada a partir de um caminho pode começar suavemente, ganhar pressão, afilar ou variar a resposta de velocidade da mesma forma ampla que o traço manual amostrado, seguindo a forma exacta do caminho, borda de selecção ou gesto de linha recta.

## Pós-processamento

O pincel pode registar um traço enquanto se desenha e reproduzir o gesto capturado ao levantar, refinando o caminho antes de assentar a marca final. É possível esboçar livremente e ainda obter direcção mais limpa, cantos mais nítidos ou estrutura mais deliberada sem desenhar com precisão mecânica.

Isto abre hachuras e marcas de construção pautadas que encaixam em ângulos limpos mantendo comprimento e carácter desenhados à mão, traços em fita estáveis à inclinação e reprodução consciente de cantos que trata curvas e rectas de forma diferente. Pincéis multi-cabeça podem partilhar um caminho corrigido enquanto cada cabeça mantém a sua variação, e as dinâmicas ainda podem moldar o traço ao longo da curva final durante a reprodução. O pós-processamento aplica-se a traços desenhados, e não à emissão contínua de aerógrafo.

## Cor e textura

Pinceladas podem usar a cor activa, responder a gradientes ou variar a cor através de dinâmicas. O tratamento de textura permite alternar entre cobertura sólida e marcas quebradas à superfície, útil para pincel seco, grão e sombreado expressivo.

Como cor e textura podem fazer parte do mesmo sistema dinâmico que forma e opacidade, um único traço pode evoluir ao mover-se pela tela, em vez de permanecer visualmente uniforme.

## Cabeças e formações de pincel

A ferramenta Pincel pode pintar com mais de uma cabeça de cada vez. Várias cabeças podem dispor-se em torno do caminho do traço para criar marcas de pena, traços em leque, comportamento tipo cerda, padrões de spray, formações texturizadas ou hachuras estruturadas.

Estas cabeças podem seguir a direcção do movimento, variar entre si e dispersar-se de formas que fazem o traço parecer orgânico em vez de repetido mecanicamente. Isto é especialmente útil para pincéis de media natural, traços decorativos, folhagem, pelo, hachuras e outras marcas que beneficiam de irregularidade controlada.

![brush-heads](/images/screens/brush-heads.jpg)

## Carga de pincel e recolha de tinta

O Pincel também pode simular quanta tinta ou material está actualmente na escova. À medida que o traço continua, essa carga pode esgotar-se gradualmente, deixando marcas mais claras, mais secas, mais finas, mais ásperas ou mais quebradas, consoante a dinâmica do pincel.

A carga pode ser reintroduzida entre traços, mantida num nível escolhido ou usada como sinal de controlo em tempo real para outros comportamentos do pincel. Isto torna possível construir pincéis que parecem media real: húmidos no início do traço, progressivamente esgotados ao longo da distância e depois mergulhados de novo para a passagem seguinte.

![material-state](/images/screens/material-state.jpg)

## Contacto com a superfície

O Pincel também pode simular perda intermitente de contacto com a superfície de pintura — as marcas quebradas que aparecem quando um lápis, um carvão, um pincel seco ou um marcador parcialmente esgotado apenas se apoiam parcialmente no papel.

Quando a simulação de contacto está activa, o pincel está em contacto ou levantado. Em contacto, as marcas depositam-se normalmente. Levantado, nenhum material é depositado e o traço deixa um intervalo cujo comprimento é escolhido aleatoriamente entre distâncias mínima e máxima. A transição é binária: o efeito não altera opacidade, tamanho, dureza, espaçamento ou fluxo — apenas se a tinta é depositada.

A facilidade com que se perde contacto é moldada por um limiar de contacto, pressão do stylus e, opcionalmente, carga do pincel. Valores de limiar mais altos tornam as pausas mais frequentes. A pressão actua como força estabilizadora: pressão leve aumenta a probabilidade de perder contacto, enquanto pressão firme mantém o traço assente. Com carga activa, carga baixa pode tornar a marca mais quebrada e carga alta pode ajudar a manter contacto, como uma ferramenta que ainda transporta material suficiente para agarrar à superfície.

A perda é avaliada pela distância percorrida do traço e não pela contagem de pinceladas, pelo que pincéis com espaçamento denso ou esparso se comportam de forma consistente. A funcionalidade funciona com renderização por carimbo e caligráfica, produzindo intervalos coerentes ao longo do traço em vez de pinceladas isoladas ignoradas.

## Animação e variação

Fontes de pincel animadas podem mudar de frame à medida que o traço avança, dando aos pincéis sensação de movimento e variedade. Aleatoriedade e variação por traço evitam que marcas repetidas pareçam idênticas, enquanto semente estável preserva carácter consistente quando a repetibilidade é necessária.

Estes comportamentos são úteis para pincéis que devem parecer vivos: cerdas a mudar ao longo do traço, carimbos texturizados a variar subtilmente com o tempo ou ferramentas multi-cabeça em que cada cabeça tem personalidade própria.

## Fluxo orientado para o artista

A ferramenta Pincel está organizada para manter decisões comuns de pintura à mão, enquanto opções de configuração menos frequentes permanecem afastadas. A intenção é manter a ferramenta acessível durante a pintura, suportando personalização profunda do design de pincéis.

No conjunto, o Pincel cobre pintura quotidiana e marcação especializada: esboço rápido, ilustração polida, renderização texturizada, trabalho de tinta expressivo e efeitos procedurais complexos partilham a mesma base flexível.
