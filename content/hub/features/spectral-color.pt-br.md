---
title: "Mistura espectral de cores"
type: docs
---
O sistema de paleta da Lumi usa um modelo de cores espectrais para simular como os pigmentos reais se misturam. O objetivo é fazer com que a experiência de construir e selecionar cores em uma paleta digital se comporte como uma mistura de tintas físicas. Depois que uma cor é aplicada à tela, ela é RGB padrão.

## O que significa mistura espectral

A mistura RGB tradicional é aditiva: a mistura de dois valores RGB os leva a um ponto médio. A mistura de pigmentos é subtrativa: cada pigmento absorve certos comprimentos de onda e seu efeito combinado é mais escuro e muitas vezes muda de tonalidade.

Lumi modela isso usando uma representação de refletância espectral de 10 bandas para cores da paleta, em vez de RGB.

Isto produz resultados semelhantes aos de tinta: misturar azul e amarelo produz verde, não cinza. A mistura de duas cores saturadas produz uma cor que muda para neutra, como fazem os pigmentos físicos.

A computação espectral é executada durante a construção da paleta — ao gerar entradas de paleta secundária e terciária e quando o Palette Mixer combina duas cores principais. A cor resultante é convertida em RGB linear para exibição e pintura.

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

### Laranjas e Amarelos| Amostra | Nome | Código CI | Família |
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(245,135,20);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Laranja Pirrol | PO73 | Vermelho (escarlate) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(243,114,64);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Laranja Cádmio | PO20 | Amarelo (corpo) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(240,180,80);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Amarelo Cádmio | PY35 | Amarelo (corpo) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(245,210,25);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Amarelo Cádmio Pálido | PY35: Pálido | Amarelo (cádmio pálido) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(250,230,5);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Amarelo Limão | PY3 | Amarelo (Limão) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(225,155,10);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Níquel Amarelo Azo | PY150 | Amarelo (meio) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(180,175,45);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Ouro Verde | PY129 | Amarelo-Verde (Ouro) |

### Cores da Terra

| Amostra | Nome | Código CI | Família |
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(200,100,70);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Siena queimada | PBr7:Queimado | Terra (Marrom Vermelho) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(117,66,0);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Umber Queimado | PBr7:Úmero | Terra (Neutro) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(205,68,35);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Siena crua | PBr7:Cru | Terra (marrom amarelo) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(187,124,25);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Ocre Amarelo | PY42 | Terra (Amarelo) |

### Verdes

| Amostra | Nome | Código CI | Família |
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,166,81);vertical-align:middle;border:1px solid rgba(0,0,0,0,25)"></span> | Verde Ftalo (YS) | PG36 | Verde (sombra amarela ftalo) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(64.130.109);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Viridiano | PG18 | Verde (Viridiano) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(128.138.112);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Terra Verde | PG23 | Verde (Terra Legal) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,110,100);vertical-align:middle;border:1px solid rgba(0,0,0,0,25)"></span> | Winsor Verde (BS) | PG7 | Verde (sombra azul ftalo) |### Azuis e Cianos

| Amostra | Nome | Código CI | Família |
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0.177.176);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Luz Turquesa Cobalto | PG50 | Ciano (Mineral) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0.148.214);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Azul Cerúleo | PB35 | Ciano (Mineral) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,100,110);vertical-align:middle;border:1px solid rgba(0,0,0,0,25)"></span> | Ftalo Turquesa | PB16 | Azul (ftalo) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,123,194);vertical-align:middle;border:1px solid rgba(0,0,0,0,25)"></span> | Azul Cobalto | PB28 | Azul (Violeta-Lean) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,75,115);vertical-align:middle;border:1px solid rgba(0,0,0,0,25)"></span> | Winsor Azul | PB15 | Azul (ftalo) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(27,63,148);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Ultramarino | PB29 | Azul (Violeta-Lean) |

### Violetas, Magentas e Vermelhos

| Amostra | Nome | Código CI | Família |
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(124,65,153);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Violeta Brilhante | PV23 | Violeta (dioxazina) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(230,90,180);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Rosa Permanente | PV19:Rosa | Magenta (Quinacridona) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(190,40,120);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Quinacridona Magenta | PV19:Magenta | Magenta (Quinacridona) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(160,30,65);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Alizarina Permanente Carmesim | PV19:Carmesim | Magenta (Quinacridona) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(120,35,65);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Violeta Perileno | PV29 | Magenta (Quinacridona) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(135,10,45);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Perileno Marrom | PR179 | Vermelho (Carmesim) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(215,30,60);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Pirrol Vermelho | PR254 | Vermelho (escarlate) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(225,55,65);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Pirrol Luz Vermelha | PR255 | Vermelho (Luz Pirrol) |

### Negros e Brancos| Amostra | Nome | Código CI | Família |
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(22,15,10);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Marte Preto (Quente) | PBk11 | Preto (Marte) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(18,28,12);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Verde Perileno | PBk31 | Preto (Verde Perileno) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(10,18,19);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Preto Marfim (Legal) | PBk9 | Preto (Marfim) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(18,18,18);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Lâmpada Preta (Neutra) | PBk7 | Preto (Lâmpada) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(255.249.235);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Branco Titânio (Quente) | PW6:Quente | Branco (titânio quente) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(255.255.255);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Branco Titânio (Neutro) | PW6 | Branco (titânio neutro) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(245.250.255);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Zinco Branco (Frio) | PW4 | Branco (Zinco Legal) |

### Controle de cinzas

Os cinzas de controle são neutralizadores padronizados usados para dessaturar misturas de maneira previsível.

| Amostra | Nome | Código CI |
| :---: | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(135.128.120);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Cinza Quente | N_QUER |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(128.128.128);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Cinza Neutro | N_NEUTRO |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(120.128.135);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Cinza Legal | N_FRESCO |

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

## Pixels da tela são RGBO sistema espectral opera inteiramente na construção da paleta e na seleção de cores. Quando uma pincelada é aplicada, a cor do primeiro plano – já convertida em RGB linear – é o que é pintado. A tela armazena dados de pixels RGB padrão.

A mistura espectral melhora a experiência de construção de uma paleta e escolha de cores de maneira consistente com o comportamento físico do pigmento, sem alterar a forma como os dados da imagem são armazenados ou compostos.