---
title: "Misturador de paleta"
type: docs
---
O Palette Mixer deriva novas cores de pares de entradas de paleta usando um pipeline fixo de três estágios. Como a mistura ocorre no domínio espectral e não no RGB, os resultados se comportam como pigmentos físicos: azul e amarelo produzem cores verdes e saturadas mudam para neutras à medida que se misturam.

## O pipeline

Cada cor produzida pelo Mixer passa por três estágios em uma ordem fixa:

1. **Mistura**: WGM espectral entre o Pai A (CCW) e o Pai B (CW).
2. **Chroma**: Misture em direção ao espectro neutro da paleta, reduzindo a saturação.
3. **Tom**: Misture para misturar o branco (matiz) ou misturar o preto (sombra).

O tom é sempre aplicado por último. Isso torna a luminosidade dominante: um ajuste de tom atinge exatamente o nível de luminosidade pretendido sem ser diluído pelo ajuste de croma que o precede.

## Selecionando Pais

Pai A e Pai B são as duas entradas entre as quais o controle deslizante de mistura se mistura. Eles são carregados no Mapa da Paleta:

- Segure **Shift** no Mapa da Paleta e **clique com o botão esquerdo** para definir o Pai A (CCW).
- Segure **Shift** e **clique com o botão direito** para definir o Pai B (CW).

Somente inscrições de **Classe A** (misturas primárias e personalizadas com procedência intacta) são aceitas como pais. Excluem-se os terciários e entradas com ascendência perdida.

As posições Parent A e Parent B do Mixer são mostradas no mapa como destaques de **anel de diamante** para que você possa sempre ver quais entradas estão carregadas.

## Os controles deslizantes

| Controle deslizante | Efeito |
| :--- | :--- |
| **Mistura** | Move entre o Pai A (extremidade anti-horária) e o Pai B (extremidade CW). Em 0,0 o resultado corresponde ao Pai A; em 1,0 corresponde ao Pai B. |
| **Croma** | Dessatura a mistura em direção ao neutro da paleta. Valores mais altos produzem resultados mais suaves e terrosos. |
| **Tom** | Muda a luminosidade para misturar branco (direção da tonalidade) ou misturar preto (direção da sombra). |

## Controles de valor

**Value Lock** congela a luminosidade perceptiva (CIE L\*) em seu nível atual enquanto os outros controles deslizantes se movem. Use isto para explorar a variação de croma ou matiz sem alterar o valor de uma mixagem.

**Band Clamp** limita o resultado para permanecer dentro dos limites de sua faixa de valor atual (por exemplo, dentro de Lower Mid). O controle deslizante de tom ainda pode ser arrastado, mas a luminosidade de saída é fixada.

O controle deslizante Tom também reflete quaisquer lacunas de valor configuradas no Editor de paleta. As faixas de luminosidade que ficam dentro de uma lacuna são mostradas como faixas cinzas semitransparentes na barra deslizante. A alça do controle deslizante salta automaticamente sobre essas lacunas: arrastar através de uma região cinza salta para o limite de banda válido mais próximo do outro lado.

## Mixagem de pontos finais (branco, preto, neutro)

Os estágios de tom e croma requerem pontos finais de referência: um branco de mistura, um preto de mistura e um neutro. Lumi os descobre automaticamente pesquisando na paleta ativa os melhores candidatos:

- **Mixing White**: o primário de maior croma mais próximo do branco puro.
- **Mixing Black**: o primário de menor luminosidade.
- **Neutro**: o Primário mais próximo do acromático (croma mais baixo).

Eles podem ser substituídos manualmente clicando com o botão direito em uma entrada no Editor de Paleta.

## Salvando uma mixagemClique em **Adicionar à paleta** para salvar o resultado do mixer atual como uma **Mixagem salva** (entrada personalizada). Antes de salvar, o sistema aplica **Relocação de Melhor Correspondência**: ele pesquisa na paleta a receita ideal que produz a mesma cor final com o melhor ajuste espacial no Mapa de Paleta. Se uma receita mais próxima for encontrada, os controles deslizantes do mixer saltarão para refleti-la, confirmando que o sistema encontrou uma origem melhor e a posição da entrada salva se alinhará com seu ponto visual no Mapa.

As mixagens salvas armazenam sua receita completa (UIDs A/B principais, fator de mistura, tom, croma) para que possam ser reproduzidas com exatidão.

## Recuperação de receita

Clicar uma vez em uma entrada personalizada no Palette Editor restaura a receita dessa entrada no Mixer:

- Pai A e Pai B são recarregados.
- Os controles deslizantes de mistura, tom e croma retornam às suas posições originais.
- Qualquer Value Lock ou Band Clamp que estava ativo durante a criação é reativado.

Isso torna mais fácil retornar a uma cor e ajustá-la ainda mais, ou usá-la como ponto de partida para uma nova mistura.