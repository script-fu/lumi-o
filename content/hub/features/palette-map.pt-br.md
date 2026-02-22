---
title: "Mapa de paleta"
type: docs
---
O Mapa de Paletas responde a uma questão prática para os pintores: dado um conjunto de pigmentos, que cores podem realmente ser misturadas a partir deles? Começando pelos pigmentos de entrada da paleta, ela explora processualmente cada combinação (misturas de dois pigmentos, misturas de três vias, variações tonais) e mapeia os resultados em uma roda de cores. A saída é uma imagem do espaço de cores alcançável para aquele conjunto específico de pigmentos.

O Mapa também é uma ferramenta de navegação baseada em coordenadas. Ele organiza cada mistura gerada por matiz e luminosidade em uma grade circular, para que toda a paleta possa ser lida rapidamente e cada cor tenha um endereço residencial estável.

## Estrutura da grade

O Mapa está dividido em uma grade 36 × 15:

- **36 setores de matiz**: passos de 10° ao redor da roda, centrados nos principais nomes de matiz.
- **15 células de luminosidade**: 3 células por faixa de valor × 5 bandas (High Key, Upper Mid, Middle, Lower Mid, Deep), indo do branco na parte externa até o preto no centro.

Cada célula é uma pequena cunha na roda. Diz-se que uma entrada colocada em uma célula tem essa célula como sua **origem**: seu endereço residencial lógico no mapa.

## Cores nas células

Quando várias cores competem pela mesma célula, apenas um **vencedor** é exibido com destaque:

1. As inscrições **Primárias** sempre ganham seu celular, independentemente dos demais ocupantes.
2. Se nenhum Primário estiver presente, o mix gerado (Secundário ou Terciário) com o **croma mais alto** vence.

As inscrições que não ganham são vice-campeãs e permanecem acessíveis por meio de ciclos de cliques (veja abaixo).

Entradas personalizadas (mixes salvas) são renderizadas como pontos quadrados; mixagens e primárias geradas são renderizadas como pontos redondos.

## Clique em Ciclismo

Clicar em uma célula ocupada seleciona o vencedor como cor de primeiro plano. Clicar na mesma célula novamente passa para o próximo ocupante (misturas geradas pelo segundo colocado e, em seguida, quaisquer entradas personalizadas salvas nesse endereço de grade). Cada clique avança um passo na pilha.

**Clique com o botão esquerdo** direciona para o primeiro plano. Quando a cor alvo é definida como plano de fundo (na caixa de ferramentas), os cliques são direcionados para o plano de fundo.

## Shift-Select: Carregando pontos finais do mixer

Segure **Shift** para entrar no modo de carregamento do endpoint:

- **Clique com o botão esquerdo** atribui a entrada clicada como **Pai A (CCW)** no Palette Mixer.
- **Clique com o botão direito** atribui-o como **Pai B (CW)**.

Somente entradas Classe A (misturas primárias e personalizadas com proveniência intacta) são selecionáveis ​​neste modo. Os terciários ficam ocultos e os pontos que não são da Classe A ficam esmaecidos. Uma breve sobreposição confirma que o modo está ativo.

## Destaques dos pais do mixer

Quando o Palette Mixer tem pontos de extremidade Pai A e Pai B ativos, ambos são marcados no Mapa com **anéis de diamante** (um formato de diamante com uma borda preta). Esses destaques permanecem visíveis mesmo quando outros elementos de exibição são alternados, de forma que os pais da mesclagem ativa sejam sempre identificáveis.

## Origem vs Posição Visual

Cada entrada tem duas posições no Mapa:

- **Origem (Célula de Origem)**: O endereço da grade lógica à qual a entrada pertence, fixo durante seu tempo de vida.
- **Posição visual do ponto**: onde a cor realmente é renderizada com base em seu matiz e luminosidade perceptivos.

Com **Relocação de melhor correspondência**, quando uma mistura é salva, o sistema calcula a receita ideal para a cor final e define a origem para corresponder à posição visual da cor. Isso mantém as cores salvas próximas de sua localização visual na roda e torna o mapa espacialmente coerente.

## Arrastar mixagens salvas

Entradas personalizadas (mixes salvas) podem ser reposicionadas arrastando:1. Clique e segure em uma entrada personalizada (ponto quadrado) e arraste além do limite de 5 pixels.
2. O cursor muda para indicar o modo de arrastar. Os destaques dos pais são atualizados ao vivo conforme você se move pelo mapa para mostrar os novos pais combinados em cada posição candidata.
3. O ponto arrastado se ajusta à posição de amostra válida mais próxima.
4. Libere para confirmar. A entrada adota a receita da célula de destino: seus pais, mesclagem, tom e croma são atualizados para corresponder e sua origem é atualizada para corresponder à nova posição visual.

Os movimentos de arrastar podem ser desfeitos através de **Editar → Desfazer**.

## Clique duplo: alternando a área de trabalho do mapa

No **Editor de Paleta**, clicar duas vezes em qualquer entrada de paleta ativa e desativa a visualização da área de trabalho do Mapa de Paleta. Esta é uma maneira rápida de alternar entre navegar pelas cores salvas e misturá-las no mapa sem usar um menu. O comportamento do clique único (restauração da receita da entrada no Mixer) não é afetado.

## Sobreposição de tela

O Mapa de paleta pode ser invocado diretamente na tela da imagem como uma sobreposição de tela inteira clicando na **amostra de primeiro plano/plano de fundo** na caixa de ferramentas. Isto proporciona uma grande superfície de mixagem sem dedicar um painel permanente ao Mapa.

## Amostra de cor central

Uma amostra circular fica no centro do buraco do donut e reflete a cor da célula sobre a qual o cursor está:

- **Cor ao passar o mouse**: quando o cursor pousa em uma entrada do mapa, a amostra é atualizada imediatamente para mostrar a cor dessa entrada.
- **Cor selecionada como alternativa**: quando nenhuma célula é passada, a amostra mostra o resultado computado do Palette Mixer para a entrada atualmente selecionada. Se o mixer ainda não tiver sido resolvido, ele usará a cor de exibição base da entrada para que o local nunca fique em branco.
- Uma borda fina e escura delineia a amostra em todos os momentos.
- Após o cursor permanecer brevemente sobre a amostra central, um anel externo branco e preto aparece para sinalizar que a área é interativa.
- **Clicar na amostra central** fecha a sobreposição da tela, retornando à visualização normal da imagem (o mesmo que clicar fora do anel externo).

## Tecla Alt: Modo de comparação de tela

Quando a sobreposição da tela do Mapa de Paleta está aberta, segurar **Alt** revela temporariamente a imagem abaixo:

- Toda a interface do mapa de paleta fica invisível (sua opacidade cai para zero), revelando a tela.
- Uma amostra circular de 64 pixels segue o cursor, preenchida com a amostra de cor atual do Palette Mixer, para que você fique atento à mistura ativa enquanto inspeciona a imagem.
- Liberar Alt restaura o mapa de paleta com opacidade total.

Um rótulo de dica, *"Mantenha pressionada a tecla Alt para ver a imagem"*, é mostrado dentro da visualização da área de trabalho como um lembrete.