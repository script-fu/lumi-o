---
title: "Editor de paleta"
type: docs
---
O Palette Editor é onde você cria e gerencia uma paleta Lumi. Ele contém seu conjunto de pigmentos, armazena as misturas salvas no Palette Mixer, registra as cores que você realmente usou durante a pintura e permite configurar a estrutura de valores e gradientes da paleta.

## Selecionando uma paleta

Uma paleta é mais que uma coleção de pigmentos: é um compromisso estilístico. Muitos artistas trabalham com um conjunto pequeno e fixo de pigmentos que conhecem intimamente: a forma como se misturam, os neutros que produzem, as mudanças de temperatura entre eles. Essa familiaridade passa a fazer parte de sua voz visual. Um pintor pode manter uma paleta quente e de baixo croma para trabalhos de figuras e uma paleta separada de tons altos para paisagens, ou pode fazer todo o seu trabalho dentro de um único conjunto de quatro pigmentos como uma restrição deliberada que unifica um corpo de trabalho.

A Lumi apoia essa forma de trabalhar. Cada paleta tem seus próprios pigmentos, misturas, estrutura de valores e gradientes. A troca de paletas altera todo o sistema de cores: o Mapa, o Mixer e as misturas disponíveis são atualizados para refletir o novo conjunto.

Um menu suspenso na parte superior do Editor de paleta seleciona a paleta ativa. O Lumi vem com três paletas no grupo **Padrão**:

| Paleta | Personagem |
| :--- | :--- |
| **Padrão** | Uma paleta versátil de tendência quente que cobre toda a roda de matizes. Bom ponto de partida para a maioria dos assuntos. |
| **Mestre** | Uma grande paleta de espectro total para pintores que desejam cobertura máxima de matiz e controle explícito sobre eixos de cinza. |
| **Zorn** | Uma paleta limitada de quatro pigmentos baseada na abordagem de Anders Zorn. Cobre uma gama surpreendentemente ampla de tons de pele quentes e neutros de baixo croma a partir de um conjunto mínimo de pigmentos. |

As paletas também podem ser criadas, importadas ou duplicadas na guia Paletas.

## Pigmentos de paleta

A seção **Pigmentos da paleta** na parte superior da visualização da paleta lista suas entradas principais: os pigmentos de base a partir dos quais o resto da paleta é construído. Estas são as entradas para o sistema de mistura espectral. Secundários e terciários são gerados a partir deles automaticamente e são usados para preencher o Mapa de Paleta

## Misturas salvas

A seção **Saved Mixes** contém cores que você manteve explicitamente do Palette Mixer usando **Add to Palette**. Estas são as cores derivadas: os resultados da mistura espectral, tons e ajustes de croma salvos para reutilização.

As mixagens salvas são subdivididas em cinco faixas de valores:

| Banda | Faixa de luminosidade padrão |
| :--- | :--- |
| Chave alta | 80 – 100% |
| Médio Superior | 60 – 80% |
| Médio | 40 – 60% |
| Médio Inferior | 20 – 40% |
| Profundo | 0 – 20% |

Lumi coloca cada mix salvo na banda apropriada automaticamente com base em sua luminosidade perceptiva (CIE L\*). Isso organiza suas mixagens por valor, em vez de pesquisar em uma lista simples, e normalmente corresponde à maneira como um artista pensa sobre a cor.

As mixagens salvas podem ser renomeadas através do botão **Renomear personalizado** ou do menu de contexto.

## Misturas usadas

A seção **Mixes Usadas** é um histórico acionado por pintura. Cada vez que uma cor da paleta é aplicada na tela, ela é registrada aqui. As mixagens usadas são ordenadas da mais para a menos recente.

Esta seção é útil para recuperar uma cor com a qual você pintou, mas não salvou explicitamente. Para manter um mix usado permanentemente, selecione-o e clique em **Promover** e ele será movido para Mixes salvos na faixa de valor apropriada.

As mixagens usadas são armazenadas por paleta e persistem entre as sessões.

## Faixas de valorAs faixas de valores definem onde ficam os limites entre as cinco zonas de luminosidade. Por padrão, eles dividem a luminosidade uniformemente na faixa de 0 a 100%, mas você pode ajustá-los para corresponder à estrutura tonal do assunto. É útil para os pintores definir e gerenciar faixas de valor _e_ as lacunas entre elas.

### O controle deslizante da faixa de valor

O **expansor de faixas de valores** no Editor de paleta contém um controle deslizante com cinco divisórias arrastáveis. Arraste qualquer divisor para deslocar o limite entre as bandas adjacentes. O rótulo acima do controle deslizante mostra o nome e a faixa percentual exata da banda ativa.

**Botões:**

| Botão | Efeito |
| :--- | :--- |
| **Cancelar** | Reverte o controle deslizante para o último estado aplicado |
| **Copiar** | Copia a configuração da banda atual para a área de transferência |
| **Colar** | Cola uma configuração de banda copiada de outra paleta |
| **Padrões** | Restaura os padrões de divisão igual de fábrica |
| **Inscreva-se** | Confirma as alterações e regenera a paleta |

**Aplicar** é necessário para tornar as alterações permanentes. Ele aciona uma regeneração completa da paleta e remove quaisquer mixagens salvas cuja luminosidade não esteja mais dentro de nenhuma faixa. Lumi mostra uma caixa de diálogo de confirmação listando quantas mixagens seriam removidas antes de continuar.

### Faixas de valores e o mapa da paleta

O Mapa de Paleta exibe a paleta como uma roda de matiz com 36 setores de matiz (10° cada) e 15 células de luminosidade organizadas como anéis concêntricos. Cada banda corresponde a três anéis: as cinco bandas × 3 anéis = 15 células no total.

Ajustar as faixas de valor altera quais valores de luminosidade ficam em cada camada do anel. Uma faixa comprimida em direção à extremidade escura faz com que seus três anéis abranjam uma faixa tonal mais estreita; uma faixa larga dá aos seus três anéis mais difusão tonal. É assim que a mesma estrutura do Palette Map se adapta a paletas ajustadas para diferentes prioridades tonais.

## Gradientes da paleta

Cada paleta pode armazenar um ou mais **Gradientes**: progressões suaves derivadas de entradas de paleta que podem ser aplicadas à tela como preenchimentos de gradiente ou usadas como faixas de referência.

Os gradientes são gerenciados no **Expansor de gradientes**. A combinação na parte superior lista os gradientes na paleta atual. **Adicionar** cria um novo gradiente. **Remover** exclui o selecionado. **Renomear** renomeia.

### Editor de gradiente

O **expansor Editor de Gradiente** configura o gradiente selecionado. Cada gradiente tem três pontos finais (**A**, **B** e **C**) exibidos como amostras de cores. Clique em uma amostra para torná-la o endpoint ativo para edição.

Cada ponto final pode ser definido clicando em **Selecionar** e depois clicando em uma entrada da paleta no Mapa de paletas ou na visualização da paleta. O endpoint está vinculado a essa entrada da paleta por UID; se a entrada for alterada, o gradiente será atualizado.

**Controles por endpoint:**

| Controle | Efeito |
| :--- | :--- |
| **Força** | Quão fortemente a cor do ponto final contribui em relação aos seus vizinhos |
| **Opacidade** | Alfa da cor do ponto final no gradiente |
| **Curva** | Ajuste de gama para a queda de cor deste ponto final |

**Controles deslizantes de distribuição** (S1, S2, S3) definem onde os três pontos médios entre os pontos finais caem ao longo da faixa de gradiente. Redefini-los retorna os pontos médios para espaçamento igual.

A faixa de visualização do gradiente na parte superior do bloco Gradient Editor mostra o resultado do ponto final atual e das configurações de distribuição.

## Paleta encaixávelA **Paleta** encaixável (**Painéis > Paleta**) é um painel mais simples com foco na leitura para navegar e selecionar cores de qualquer paleta. Ele mostra a mesma visualização de três seções (Palette Pigments, Saved Mixes, Used Mixes) sem os expansores Value Bands e Gradients.

Um menu suspenso de seletor de paleta na parte superior permite alternar entre todas as paletas disponíveis. Clique em qualquer entrada para defini-la como cor de primeiro plano. Clique duas vezes para abrir o editor de nomes de cores. Para paletas graváveis, as ações Editar cor, Nova cor do FG e Excluir cor estão disponíveis na barra de botões.

A Paleta encaixável destina-se ao acesso rápido às cores durante a pintura, quando o Editor de Paletas completo ocupa muito espaço.

## Guia Paletas

A **Guia Paletas** (disponível como uma guia encaixável) mostra a paleta ativa no modo compacto. Exclui os pigmentos para focar nas misturas salvas