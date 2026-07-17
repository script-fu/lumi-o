---
title: "Filtros"
type: docs
translation_provenance: ai-reviewed
translation_source_sha256: 312088430d35761f6df789821c1629c829e6eb1d2f8b4be58c5843c893c3c7ed
url: "hub/features/filters"
translation_lock: true
---
O menu Filtros do Lumi reúne ajustes correctivos, efeitos de lente estilizados, geradores de textura procedimental, tratamentos inspirados na impressão e ferramentas de análise num só sítio. A ordem do menu é prática e não académica: ferramentas de desfoque e realce lado a lado, efeitos de distorção e iluminação agrupados por aspecto, e geradores de textura ou padrão reunidos quando o objectivo é criar material de origem em vez de modificar uma imagem existente.

As caixas de diálogo de filtro seguem o mesmo fluxo geral. Predefinições, pré-visualização, vista dividida e controlos de opacidade ou mistura permitem afinar rapidamente um efeito e, nas camadas, o resultado pode permanecer como filtro editável e não destrutivo em vez de ser fundido de imediato. O Lumi também mantém um historial recente de uso de filtros, pelo que repetir o último efeito ou reabrir a última caixa de diálogo faz parte do ritmo normal de pintura, e não de uma tarefa à parte.

## Desfoque

### Gaussian Blur

Gaussian Blur é o filtro de suavização standard do Lumi: um desfoque limpo e uniforme com controlos de tamanho horizontal e vertical separados, tratamento de bordas e opções de kernel. É a escolha generalista para foco suave, máscaras suavizadas, profundidade atmosférica e qualquer fluxo em que o desfoque deva permanecer neutro.

### Pixelize

Pixelize reduz o detalhe em estruturas de blocos deliberadas em vez de um desfoque suave. Como a caixa de diálogo expõe largura e altura de bloco, deslocamentos, forma de pixel e comportamento de preenchimento, serve tanto como efeito de censura grosseira como mosaico controlável ou tratamento gráfico de baixa resolução.

### Selective Gaussian Blur

Selective Gaussian Blur suaviza dentro de regiões tentando preservar bordas mais fortes. É útil quando uma imagem precisa de textura mais calma ou de ruído visual reduzido sem perder os contornos maiores que ainda precisam de se ler com clareza.

### Lens Blur

Lens Blur é um dos filtros de desfoque mais orientados para ilustração do Lumi. Os seus controlos assentam na forma de íris poligonal, curvatura das lâminas, estiramento anamórfico, realce de highlights e região de foco configurável, comportando-se menos como suavizador genérico e mais como ferramenta estilizada de profundidade de campo com bokeh modelado.

### Tilt-shift

Tilt-shift mantém uma faixa de foco nítida e controlável enquanto desfoca progressivamente a imagem acima e abaixo. O ângulo da faixa, o feather, o viés de perspectiva, a forma da íris e o reforço miniatura da caixa de diálogo tornam-no adequado a cenas tipo miniatura, vistas arquitectónicas e composições em que o foco deve ler-se como faixa desenhada e não como indício circular de profundidade.

### Circular Motion Blur

Circular Motion Blur espalha detalhe em torno de um ponto central, transformando bordas em trilhas rotacionais. É a escolha natural para sujeitos em rotação, energia tipo turbina ou ilustrações que precisam de movimento orbital.

### Linear Motion Blur

Linear Motion Blur estende detalhe numa direcção, simulando deslocamento, movimento de câmara ou gesto rápido pelo quadro. É especialmente útil quando o movimento deve parecer direccional e gráfico em vez de difuso.

### Zoom Motion Blur

Zoom Motion Blur irradia detalhe a partir de um centro, produzindo a sensação de aproximação ou afastamento do observador. Funciona bem para momentos de impacto, linhas de velocidade e composições que precisam de energia de zoom sem repintar toda a imagem.

## Realce

### High Pass

High Pass isola contraste local fino em vez de mudança tonal ampla. Com apenas escala e contraste a gerir, é uma ferramenta directa para extrair detalhe de bordas, construir sobreposições nítidas ou preparar passagens de nitidez que devem enfatizar estrutura mais do que cor.

### Noise Reduction

Noise Reduction faz o movimento oposto: suprime variação fina indesejada para que formas maiores se leiam com mais clareza. É útil quando material digitalizado, texturas comprimidas ou passagens sobrecarregadas precisam de simplificação antes de continuar a pintura ou filtragem.

### Sharpen

Sharpen usa um modelo de máscara de desfoque, com raio, quantidade e limiar a controlar a intensidade com que o contraste local é reforçado. Na prática, serve para restaurar clareza após desfoque, redimensionamento de exportação ou acabamentos subtis em que o detalhe precisa de avançar sem transformar cada pixel em ruído.

## Cor

### Tonal Grading

Tonal Grading remapeia cor por gama tonal em vez de remodelar contraste ou desenhar uma curva. A luminância de cada pixel escolhe uma mistura suave de três cores do utilizador para sombra, meio-tom e realce; a imagem mantém a estrutura claro-escuro enquanto a paleta muda. Intensidade por região, viés de equilíbrio ao estilo Lightroom (à esquerda favorece a gradação de sombra, à direita a de realce) e suavidade de transição controlam até onde cada cor chega e como as gradações se sobrepõem. Destina-se a ilustração, banda desenhada, concept art e fotografia quando o objectivo é uma gradação ou look coerente.

## Distorção

### Chromatic Aberration

Chromatic Aberration separa canais de cor para fora a partir de um centro escolhido, com controlos para direcção radial ou tangencial, viés entre pares de canais, falloff e preservação de luminância. O código e a caixa de diálogo tratam-no como ferramenta bidireccional: pode acrescentar fringing de lente estilizado para energia ou inverter o sinal para corrigir aberração ligeira no material de origem.

### Lens Distortion

Lens Distortion remodela a imagem através de curvatura tipo barril ou almofada, termos de borda, compensação de zoom, deslocamentos de centro e clarão de cantos. Serve tanto para corrigir uma imagem opticamente curvada como para empurrar deliberadamente uma imagem para carácter de lente grande angular ou retro.

## Iluminação

### Bloom

Bloom transforma áreas brilhantes em brilho controlado, com limiar, suavidade, raio e força a definir até onde a luz se espalha e com que intensidade eleva a imagem. O controlo extra de limitação de exposição mantém-no útil como efeito de realce, e não como lavagem automática.

### Sky

Sky é mais do que sobreposição de tonalidade ou gradiente: renderiza um céu analítico usando modelos Preetham, Hosek/Wilkie ou Nishita. Como a caixa de diálogo expõe projecção, ângulo solar, turbidez, densidade atmosférica, altitude, controlos do disco solar e exposição, pode construir desde um fundo claro simples até um pôr-do-sol ou crepúsculo mais fisicamente fundamentado.

### Vignette

Vignette escurece, colore ou até apaga em direcção às bordas da imagem, com controlos de forma, raio, suavidade, gamma, proporção, squeeze, rotação e posicionamento na tela. Funciona como tratamento clássico de borda fotográfica, mas é flexível o suficiente para actuar como máscara de enquadramento ou foco composicional irregular.

## Ruído

### HSV Noise

HSV Noise aleatoriza matiz, saturação e valor de forma independente. É útil quando uma imagem precisa de vivacidade cromática ou instabilidade analógica sem partir totalmente a estrutura local.

### Hurl

Hurl é a versão extrema do ruído: substitui pixels por cores completamente aleatórias. Pense nele como fonte de caos destrutivo para glitch, texturas desgastadas ou máscaras que precisam de rutura agressiva.

### Pick

Pick substitui cada pixel por um vizinho escolhido aleatoriamente, pelo que a imagem permanece ligada à origem em vez de se tornar estática pura. O resultado é variação granular embaralhada que pode parecer mais orgânica do que ruído totalmente aleatório.

### Spread

Spread dispersa pixels deslocando-os aleatoriamente dentro de um raio. É útil quando se quer perturbação sem movimento: superfície quebrada, borda manchada ou textura desgastada que ainda transporta as relações cromáticas da imagem de origem.

### Fractal

Fractal gera ruído Perlin fractal em mosaico, o que o torna especialmente valioso como fonte reutilizável para máscaras, nuvens, textura de papel, quebra tipo terreno e sobreposições procedimentais. Por ser em mosaico, alimenta fluxos maiores sem costuras óbvias.

### Blue Noise Grain

Blue Noise Grain é o gerador de grão monocromático estilo filme e impressão do Lumi. Predefinições de tamanho de grão, máscara de ruído azul, viés de meio-tom, viés de sombra e controlos de semente mostram que foi concebido para colocar grão de forma uniforme e controlável, e não apenas espalhar manchas monocromáticas aleatórias sobre a imagem.

### Risograph Grain

Risograph Grain assenta na mesma lógica de grão, mas transforma-a num efeito de impressão de duas chapas. Cores de tinta separadas, equilíbrio de chapas, desregisto deliberado e variação com semente tornam-no adequado a pósteres, estética de impressão independente e ilustrações que devem parecer sobre-impressas fisicamente e não digitalmente perfeitas.

### Halftone (FM)

Halftone (FM) cria meio-tom estocástico modulado em frequência usando ruído azul ou métodos de limiar relacionados. Com modos de cor monocromático, duotónico e CMYK, mais controlos de ganho de ponto e decorrelação de chapas, visa textura tipo impressão que permanece irregular e viva em vez de cair numa grelha rígida.

## Bordas

### Difference of Gaussians

Difference of Gaussians detecta bordas subtraindo duas versões desfocadas da imagem. É um operador compacto e útil para mapas de bordas, extracção de linhas estilizada e transições estruturais sem comprometer-se com contorno limiar completo.

## Morfologia

### Median

Median substitui cada pixel pelo valor mediano da vizinhança, o que tende a remover ruído isolado preservando melhor limites fortes do que um desfoque simples. É um filtro de limpeza prático para nivelar pequeno ruído visual sem suavizar imediatamente toda a imagem.

### Dilate

Dilate expande regiões mais claras para fora usando a mesma lógica de vizinhança consciente da forma. Em termos de criação de imagem, pode engrossar marcas claras, expandir formas luminosas ou fechar pequenas lacunas escuras.

### Erode

Erode faz o movimento complementar, expandindo regiões escuras e recuando as claras. É útil para afinar detalhes claros, ampliar massas escuras ou apertar máscaras e formas gráficas.

## Padrão

### Checkerboard

Checkerboard gera um padrão regular de ladrilhos alternados. É simples, mas essa simplicidade torna-o útil para testar transparência, construir máscaras, bloquear fundos gráficos ou criar material geométrico limpo.

### Grid

Grid desenha divisões horizontais e verticais repetidas, útil para guias de layout, fundos de design, ilustração técnica e máscaras procedimentais. Por ser gerado como filtro, espaçamento e aparência podem ser afinados sem construir o padrão manualmente.

### Voronoi

Voronoi gera textura celular em mosaico a partir de pontos com semente, com controlos de tipo de característica, métrica de distância, aleatoriedade, detalhe fractal e envolvimento contínuo. Na prática, pode ir de estruturas limpas de células partidas a padrões mais orgânicos de pedra, pele, mapa ou rede abstracta.

### Wave

Wave produz padrões em faixas ou anéis moldados por perfil de forma de onda, disposição geométrica, distorção, detalhe fractal e deslocamento de fase. É mais do que ferramenta de riscas: pode gerar ondulações controladas, faixas topográficas, gráficos tipo moiré ou campos concêntricos ruidosos.

### Halftone (AM)

Halftone (AM) aplica um ecrã clássico de pontos modulados em amplitude, com frequência, forma de ponto, nitidez, modo de cor e controlos de ângulo CMYK para estrutura de impressão em roseta. Comparado com meio-tom FM, é a opção mais ordenada e reconhecivelmente mecânica quando o aspecto desejado é jornal, litografia offset ou geometria de ecrã deliberadamente visível.
