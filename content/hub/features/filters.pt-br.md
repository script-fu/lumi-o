---
title: "Filtros"
type: docs
---
O menu Filtros do Lumi reúne ajustes corretivos, efeitos de lente estilizados, geradores de textura processual, tratamentos inspirados em impressão e ferramentas de análise em um só lugar. A ordem do menu é mais prática do que acadêmica: as ferramentas de desfoque e aprimoramento ficam lado a lado, os efeitos de distorção e iluminação são agrupados por aparência e os geradores de textura ou padrão são mantidos juntos quando o objetivo é construir o material de origem em vez de modificar uma imagem existente.

As caixas de diálogo de filtro seguem o mesmo fluxo de trabalho geral. Predefinições, visualização, visualização dividida e controles de opacidade ou mesclagem permitem que um efeito seja ajustado rapidamente e, nas camadas, o resultado pode permanecer como um filtro editável e não destrutivo, em vez de ser mesclado imediatamente. Lumi também mantém um histórico recente de uso de filtros, portanto, repetir o último efeito ou reabrir o último diálogo faz parte do ritmo normal da pintura, e não uma tarefa separada.

## Desfoque

### Desfoque gaussiano

Gaussian Blur é o filtro de suavização padrão da Lumi: um desfoque limpo e uniforme com controles de tamanho horizontais e verticais separados, manipulação de bordas e opções de kernel. É a escolha de uso geral para foco suave, máscaras suavizadas, profundidade atmosférica e qualquer fluxo de trabalho em que o desfoque em si deva permanecer neutro.

### Pixelizar

Pixelize reduz os detalhes em estruturas de blocos deliberadas em vez de um desfoque suave. Como a caixa de diálogo expõe a largura e a altura do bloco, os deslocamentos, a forma do pixel e o comportamento do preenchimento, ela funciona tanto como um efeito de censura grosseira quanto como um mosaico controlável ou tratamento gráfico de baixa resolução.

### Desfoque gaussiano seletivo

O Desfoque Gaussiano Seletivo suaviza dentro das regiões enquanto tenta preservar bordas mais fortes. É útil quando uma imagem precisa de uma textura mais calma ou vibração reduzida sem perder os limites maiores da forma que ainda precisam ser lidos com clareza.

### Desfoque de lente

Lens Blur é um dos filtros de desfoque mais focados em ilustrações do Lumi. Seus controles são construídos em torno do formato da íris poligonal, curvatura da lâmina, alongamento anamórfico, aumento de destaque e uma região de foco configurável, de modo que se comporta menos como um suavizador genérico e mais como uma ferramenta estilizada de profundidade de campo com bokeh modelado.

### Mudança de inclinação

A mudança de inclinação mantém nítida uma banda de foco controlável enquanto desfoca progressivamente a imagem acima e abaixo dela. O ângulo da faixa, a difusão, o viés de perspectiva, o formato da íris e o aumento de miniatura do diálogo o tornam adequado para cenas com aparência de miniatura, vistas arquitetônicas e qualquer composição em que o foco deva ser lido como uma faixa projetada em vez de uma sugestão circular de profundidade.

### Desfoque de movimento circular

O Circular Motion Blur espalha detalhes em torno de um ponto central, transformando as bordas em trilhas rotacionais. É a escolha natural para assuntos giratórios, energia semelhante a uma turbina ou ilustrações que precisam de uma sensação de movimento orbital.

### Desfoque de movimento linear

O Linear Motion Blur estende os detalhes em uma direção, simulando viagens, movimentos de câmera ou gestos rápidos no quadro. É especialmente útil quando o movimento precisa parecer direcional e gráfico, em vez de difuso.

### Zoom desfoque de movimento

O Zoom Motion Blur irradia detalhes para fora a partir de um centro, produzindo a sensação de aproximação ou afastamento do espectador. Funciona bem para momentos de impacto, linhas de velocidade e composições que precisam de energia de zoom da câmera sem repintar toda a imagem.

## Melhorar

### Passa altaHigh Pass isola contraste local fino em vez de ampla mudança tonal. Com apenas escala e contraste para gerenciar, é uma ferramenta simples para extrair detalhes de bordas, construir sobreposições nítidas ou preparar passagens de nitidez que devem enfatizar mais a estrutura do que a cor.

### Redução de ruído

A Redução de Ruído é o movimento oposto: ela suprime variações finas indesejadas para que formas maiores sejam lidas com mais clareza. É útil quando o material digitalizado, as texturas compactadas ou as passagens sobrecarregadas precisam ser simplificadas antes de continuar a pintura ou filtragem.

### Afiar

O Sharpen usa um modelo de máscara não nítida, com raio, quantidade e limite controlando a intensidade com que o contraste local é pressionado. Na prática, isso o torna adequado para restaurar a clareza após desfoque, redimensionamento de exportação ou passagens de acabamento sutis onde os detalhes precisam aparecer sem transformar cada pixel em ruído.

## Distorcer

### Aberração cromática

A Aberração Cromática separa os canais de cores para fora de um centro escolhido, com controles para direção radial ou tangencial, polarização entre pares de canais, queda e preservação de luminância. O código e o diálogo tratam-no como uma ferramenta bidirecional: pode adicionar bordas de lente estilizadas para obter energia ou inverter o sinal para corrigir aberrações leves no material de origem.

### Distorção da lente

A Distorção de Lente remodela a imagem por meio de curvatura estilo barril ou almofada de alfinetes, termos de borda, compensação de zoom, deslocamentos centrais e brilho de canto. Isso o torna útil tanto para corrigir uma imagem que parece opticamente curvada quanto para empurrá-la deliberadamente em direção a um caráter de lente grande angular ou retro.

## Iluminação

### Florescer

Bloom transforma áreas brilhantes em brilho controlado, com limite, suavidade, raio e força definindo até que ponto a luz se espalha e com que intensidade ela eleva a imagem. O controle extra de limitação de exposição o mantém utilizável como efeito de destaque, em vez de lavagem automática.

### Céu

O céu é mais do que uma sobreposição de tonalidade ou gradiente: ele renderiza um céu analítico usando os modelos Preetham, Hosek/Wilkie ou Nishita. Como a caixa de diálogo expõe a projeção, o ângulo do sol, a turbidez, a densidade atmosférica, a altitude, os controles do disco solar e a exposição, ela pode construir qualquer coisa, desde um simples cenário claro até um pôr do sol ou céu crepuscular mais fisicamente fundamentado.

### Vinheta

A vinheta escurece, colore ou até mesmo apaga as bordas da imagem, com controles de forma, raio, suavidade, gama, proporção, compressão, rotação e posicionamento na tela. Funciona como um tratamento de borda fotográfico clássico, mas é flexível o suficiente para atuar como uma máscara de enquadramento ou um foco de composição irregular.

## Ruído

### Ruído HSV

HSV Noise randomiza matiz, saturação e valor de forma independente. Isso o torna útil quando uma imagem precisa de vivacidade de cores ou instabilidade analógica sem quebrar totalmente a estrutura local.

### Arremesso

Hurl é a versão extrema do ruído: substitui pixels por cores completamente aleatórias. É melhor considerá-lo uma fonte de caos destrutivo para falhas, texturas desgastadas ou máscaras que precisam de uma ruptura agressiva.

### Escolha

Pick substitui cada pixel por um vizinho escolhido aleatoriamente, para que a imagem permaneça relacionada à sua fonte em vez de se tornar pura estática. O resultado é uma variação granular e embaralhada que pode parecer mais orgânica do que ruído totalmente aleatório.

### EspalharSpread espalha pixels deslocando-os aleatoriamente dentro de um raio. É útil quando você deseja uma interrupção imóvel: uma superfície quebrada, uma borda manchada ou uma textura desgastada que ainda mantém as relações de cores da imagem de origem.

### fractal

Fractal gera ruído Perlin fractal em blocos, o que o torna especialmente valioso como uma fonte reutilizável para máscaras, nuvens, textura de papel, ruptura semelhante a terreno e sobreposições procedimentais. Por ser lado a lado, ele pode alimentar fluxos de trabalho maiores sem criar costuras óbvias.

### Grão de ruído azul

Blue Noise Grain é o gerador de grãos monocromático estilo filme e impressão da Lumi. As predefinições de tamanho de grão da caixa de diálogo, máscara de ruído azul, tendência de meio-tom, tendência de sombra e controles de sementes mostram que ela foi projetada para colocar granulação de maneira uniforme e controlada, não apenas para espalhar manchas monocromáticas aleatórias sobre a imagem.

### Grão risográfico

Risograph Grain baseia-se na mesma lógica de granulação, mas a transforma em um efeito de impressão de duas placas. Cores de tinta separadas, equilíbrio de chapa, registro incorreto deliberado e variação semeada tornam-no uma boa opção para trabalhos de pôster, estética de impressão independente e ilustrações que deveriam parecer impressas fisicamente em vez de digitalmente perfeitas.

### Meio-tom (FM)

Meio-tom (FM) cria um meio-tom estocástico modulado em frequência usando ruído azul ou métodos de limiar relacionados. Com modos de cores para monocromático, duotônico e CMYK, além de controles de ganho de ponto e decorrelação de placa, ele visa textura semelhante a impressão que permanece irregular e viva em vez de cair em uma grade rígida.

## Bordas

### Diferença de Gaussianos

A diferença de gaussianas detecta bordas subtraindo duas versões desfocadas da imagem uma da outra. É um operador compacto e útil para mapas de arestas, extração de linhas estilizadas e localização de transições estruturais sem comprometer-se com um contorno de limiar completo.

## Morfologia

### Mediana

A mediana substitui cada pixel pelo valor mediano de sua vizinhança, o que tende a remover o ruído isolado, preservando melhor os limites mais fortes do que um simples desfoque. É um filtro de limpeza prático para nivelar pequenas vibrações visuais sem suavizar imediatamente a imagem inteira.

### Dilatar

Dilate expande regiões mais claras para fora usando a mesma lógica de vizinhança com reconhecimento de forma. Em termos de criação de imagens, pode engrossar marcas brilhantes, expandir formas claras ou fechar pequenas lacunas escuras.

### Erodir

Erode faz o movimento complementar, aumentando as regiões mais escuras e retirando as mais claras. É útil para diminuir detalhes claros, ampliar massas escuras ou estreitar máscaras e formas gráficas.

## Padrão

### Tabuleiro de damas

O tabuleiro de damas gera um padrão regular de ladrilhos alternados. É simples, mas essa simplicidade o torna útil para testar transparência, construir máscaras, bloquear fundos gráficos ou criar material de origem geométrica limpa.

### Grade

A grade desenha repetidas divisões horizontais e verticais, tornando-a útil para guias de layout, cenários de design, ilustrações técnicas e máscaras procedurais. Por ser gerado como um filtro, o espaçamento e a aparência podem ser ajustados sem a necessidade de construir o padrão manualmente.

### Voronói

Voronoi gera uma textura celular lado a lado a partir de pontos propagados, com controles para tipo de recurso, métrica de distância, aleatoriedade, detalhes fractais e envolvimento contínuo. Na prática, ele pode passar de estruturas limpas de células rachadas para padrões mais orgânicos de pedra, pele, mapa ou rede abstrata.

### AcenoWave produz padrões em faixas ou anéis moldados por perfil de forma de onda, arranjo geométrico, distorção, detalhe fractal e deslocamento de fase. Isso o torna mais do que uma simples ferramenta de distribuição: ela pode gerar ondulações controladas, bandas topográficas, gráficos tipo moiré ou campos de padrões concêntricos ruidosos.

### Meio-tom (AM)

Meio-tom (AM) aplica uma tela de pontos modulada em amplitude clássica, com frequência, formato de ponto, nitidez, modo de cor e controles de ângulo CMYK para estrutura de impressão em estilo roseta. Comparado com o meio-tom FM, é a opção mais ordenada e reconhecidamente mecânica quando a aparência desejada é papel de jornal, litografia offset ou geometria de tela deliberadamente visível.