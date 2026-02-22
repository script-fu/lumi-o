---
title: "Mistura espectral de cores"
type: docs
---
O sistema de paleta da Lumi usa um modelo de cores espectrais para simular como os pigmentos reais se misturam. O objetivo é fazer com que a experiência de construir e selecionar cores em uma paleta digital se comporte como uma mistura de tintas físicas. Depois que uma cor é aplicada à tela, ela é RGB padrão.

## O que significa mistura espectral

A mistura RGB tradicional é aditiva: a mistura de dois valores RGB os leva a um ponto médio. A mistura de pigmentos é subtrativa: cada pigmento absorve certos comprimentos de onda e seu efeito combinado é mais escuro e muitas vezes muda de tonalidade.

Lumi modela isso usando uma representação de refletância espectral de 10 bandas para cores da paleta, em vez de RGB.

Isto produz resultados semelhantes aos de tinta: misturar azul e amarelo produz verde, não cinza. A mistura de duas cores saturadas produz uma cor que muda para neutra, como fazem os pigmentos físicos.

A computação espectral é executada durante a construção da paleta, ao gerar entradas de paleta secundária e terciária e quando o Palette Mixer combina duas cores parentais. A cor resultante é convertida em RGB linear para exibição e pintura.

## Perfis de pigmentos

As entradas da paleta podem ser baseadas em dados reais de pigmentos usando **códigos de índice de cores (CI)**. Cada família de pigmentos CI possui uma tendência espectral característica que influencia a forma como ela se mistura.

| Papel do pigmento | Comportamento de mistura | Exemplo |
| :--- | :--- | :--- |
| **Primário** | Alto croma, secundários limpos | PY3 (Amarelo Limão), PR122 (Magenta) |
| **Corpo** | Tom de massa opaco e forte, mudando para oliva em misturas verdes | PY35 (Amarelo Cádmio), PR108 (Vermelho Cádmio) |
| **Neutralizador** | Desatura e silencia rapidamente | PBk11 (Marte Preto), PBr7 (Sienna) |
| **Âncora Croma** | Elevado poder tintorial, domina misturas | PB29 (azul ultramarino), PG7 (verde ftalo) |

Adicionar cores primárias com códigos CI a uma paleta fornece ao mecanismo de mixagem uma polarização espectral precisa para essas cores, de modo que as mixagens secundárias e terciárias geradas reflitam o comportamento de mixagem do mundo real.

## Pigmentos Lumi

A paleta Master vem com os seguintes pigmentos. As amostras mostram a aparência típica de masstone de cada pigmento (força total, não diluída).

### Laranjas e Amarelos

| Amostra | Nome | Código CI | Família |
| :---: | :--- | :--- | :--- |
| {{< swatch "245,135,20" >}} | Laranja Pirrol | PO73 | Vermelho (escarlate) |
| {{< swatch "243,114,64" >}} | Laranja Cádmio | PO20 | Amarelo (corpo) |
| {{< swatch "240,180,80" >}} | Amarelo Cádmio | PY35 | Amarelo (corpo) |
| {{< swatch "245,210,25" >}} | Amarelo Cádmio Pálido | PY35: Pálido | Amarelo (cádmio pálido) |
| {{< swatch "250,230,5" >}} | Amarelo Limão | PY3 | Amarelo (Limão) |
| {{< swatch "225,155,10" >}} | Níquel Amarelo Azo | PY150 | Amarelo (meio) |
| {{< swatch "180,175,45" >}} | Ouro Verde | PY129 | Amarelo-Verde (Ouro) |

### Cores da Terra

| Amostra | Nome | Código CI | Família |
| :---: | :--- | :--- | :--- |
| {{< swatch "200,100,70" >}} | Siena queimada | PBr7:Queimado | Terra (Marrom Vermelho) |
| {{< swatch "117,66,0" >}} | Umber Queimado | PBr7:Úmero | Terra (Neutro) |
| {{< swatch "205,68,35" >}} | Siena crua | PBr7:Cru | Terra (marrom amarelo) |
| {{< swatch "187,124,25" >}} | Ocre Amarelo | PY42 | Terra (Amarelo) |

### Verdes

| Amostra | Nome | Código CI | Família |
| :---: | :--- | :--- | :--- |
| {{< swatch "0,166,81" >}} | Verde Ftalo (YS) | PG36 | Verde (sombra amarela ftalo) |
| {{< swatch "64,130,109" >}} | Viridiano | PG18 | Verde (Viridiano) |
| {{< swatch "128,138,112" >}} | Terra Verde | PG23 | Verde (Terra Legal) |
| {{< swatch "0,110,100" >}} | Winsor Verde (BS) | PG7 | Verde (sombra azul ftalo) |

### Azuis e Cianos

| Amostra | Nome | Código CI | Família |
| :---: | :--- | :--- | :--- |
| {{< swatch "0,177,176" >}} | Luz Turquesa Cobalto | PG50 | Ciano (Mineral) |
| {{< swatch "0,148,214" >}} | Azul Cerúleo | PB35 | Ciano (Mineral) |
| {{< swatch "0,100,110" >}} | Ftalo Turquesa | PB16 | Azul (ftalo) |
| {{< swatch "0,123,194" >}} | Azul Cobalto | PB28 | Azul (Violeta-Lean) |
| {{< swatch "0,75,115" >}} | Winsor Azul | PB15 | Azul (ftalo) |
| {{< swatch "27,63,148" >}} | Ultramarino | PB29 | Azul (Violeta-Lean) |

### Violetas, Magentas e Vermelhos

| Amostra | Nome | Código CI | Família |
| :---: | :--- | :--- | :--- |
| {{< swatch "124,65,153" >}} | Violeta Brilhante | PV23 | Violeta (dioxazina) |
| {{< swatch "230,90,180" >}} | Rosa Permanente | PV19:Rosa | Magenta (Quinacridona) |
| {{< swatch "190,40,120" >}} | Quinacridona Magenta | PV19:Magenta | Magenta (Quinacridona) |
| {{< swatch "160,30,65" >}} | Alizarina Permanente Carmesim | PV19:Carmesim | Magenta (Quinacridona) |
| {{< swatch "120,35,65" >}} | Violeta Perileno | PV29 | Magenta (Quinacridona) |
| {{< swatch "135,10,45" >}} | Perileno Marrom | PR179 | Vermelho (Carmesim) |
| {{< swatch "215,30,60" >}} | Pirrol Vermelho | PR254 | Vermelho (escarlate) |
| {{< swatch "225,55,65" >}} | Pirrol Luz Vermelha | PR255 | Vermelho (Luz Pirrol) |

### Negros e Brancos

| Amostra | Nome | Código CI | Família |
| :---: | :--- | :--- | :--- |
| {{< swatch "22,15,10" >}} | Marte Preto (Quente) | PBk11 | Preto (Marte) |
| {{< swatch "18,28,12" >}} | Verde Perileno | PBk31 | Preto (Verde Perileno) |
| {{< swatch "10,18,19" >}} | Preto Marfim (Legal) | PBk9 | Preto (Marfim) |
| {{< swatch "18,18,18" >}} | Lâmpada Preta (Neutra) | PBk7 | Preto (Lâmpada) |
| {{< swatch "255,249,235" >}} | Branco Titânio (Quente) | PW6:Quente | Branco (titânio quente) |
| {{< swatch "255,255,255" >}} | Branco Titânio (Neutro) | PW6 | Branco (titânio neutro) |
| {{< swatch "245,250,255" >}} | Zinco Branco (Frio) | PW4 | Branco (Zinco Legal) |

### Controle de cinzas

Os cinzas de controle são neutralizadores padronizados usados para dessaturar misturas de maneira previsível.

| Amostra | Nome | Código CI |
| :---: | :--- | :--- |
| {{< swatch "135,128,120" >}} | Cinza Quente | N_QUER |
| {{< swatch "128,128,128" >}} | Cinza Neutro | N_NEUTRO |
| {{< swatch "120,128,135" >}} | Cinza Legal | N_FRESCO |

## O Mapa da Paleta

O Mapa de Paleta visualiza a paleta ativa como uma roda de matiz: 36 setores de matiz (passos de 10°) × 15 células de luminosidade. Quando as primárias são adicionadas, o sistema gera combinações secundárias e terciárias e as coloca nas posições apropriadas do mapa.

Clicar em uma célula seleciona uma cor como primeiro plano. Clique com a tecla Shift pressionada para atribuí-lo como um ponto final pai no Palette Mixer.

## O misturador de paleta

O Palette Mixer deriva novas cores de duas entradas pai usando um pipeline fixo de três estágios:

1. **Mistura**: WGM espectral entre o Pai A (CCW) e o Pai B (CW).
2. **Chroma**: Misture em direção ao espectro neutro da paleta, reduzindo a saturação.
3. **Tom**: Misture para misturar branco ou misturar preto, ajustando a luminosidade.

O tom é aplicado por último para que os ajustes de luminosidade não sejam diluídos pelas alterações de croma. Os controles Value Lock e Band Clamp restringem os resultados a um nível de luminosidade ou faixa de valor específico.

As cores misturadas podem ser salvas na paleta como entradas **Personalizadas**, armazenando a receita completa (UIDs principais, fator de mistura, tom, valores de croma) para recuperação posterior.

## Pixels da tela são RGB

O sistema espectral opera inteiramente na construção da paleta e na seleção de cores. Quando uma pincelada é aplicada, a cor do primeiro plano (já convertida em RGB linear) é o que é pintado. A tela armazena dados de pixels RGB padrão.A mistura espectral melhora a experiência de construção de uma paleta e escolha de cores de maneira consistente com o comportamento físico do pigmento, sem alterar a forma como os dados da imagem são armazenados ou compostos.