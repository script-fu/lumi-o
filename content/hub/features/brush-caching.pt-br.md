---
title: "Cache de pincel"
type: docs
---
O cache de pincéis foi projetado para fazer com que seus pincéis favoritos pareçam rápidos o mais cedo possível. Em vez de recalcular o mesmo carimbo de pincel transformado repetidamente, o Lumi pode manter um cache salvo das formas de pincel que você realmente usa e recarregar esse cache automaticamente mais tarde.

## Visão geral

O recurso é construído em torno da ideia de que muitos pincéis expressivos ainda revisitam as mesmas combinações práticas de tamanho, ângulo, dureza e proporção durante a pintura. Quando essas combinações são reutilizadas, o Lumi pode servir o carimbo de pincel transformado diretamente do cache, em vez de reconstruí-lo.

O resultado é:

- inicialização mais rápida do curso após um cache ter sido salvo
- uso repetido mais suave de predefinições favoritas
- menos recomputação desperdiçada durante longas sessões de pintura
- restauração automática de caches salvos quando a predefinição for usada novamente

## Intenção

O cache de pincel destina-se a pincéis aos quais você retorna com frequência: predefinições de pintura principais, ferramentas de tinta favoritas, pincéis secos texturizados e outros pincéis cujos carimbos transformados são caros o suficiente para serem notados.

O objetivo não é pré-preparar todos os estados teóricos do pincel. O objetivo é permitir que o uso real da pintura preencha primeiro os estados mais valiosos e, em seguida, salvar o cache preenchido para que o pincel já esteja quente na próxima vez que você usá-lo.

## Como funciona

O cache de pincel funciona em conjunto com a quantização de pincel.

Quando a quantização está habilitada para uma predefinição de dinâmica, as saídas que afetam a transformação são ajustadas em etapas discretas. Isso dá ao Lumi um conjunto finito de estados de pincel reutilizáveis. Enquanto você pinta:

1. O Lumi verifica se o carimbo transformado já existe no cache.
2. Se isso acontecer, o selo é reutilizado imediatamente.
3. Caso contrário, Lumi o constrói uma vez e o armazena.
4. Com o tempo, o cache é preenchido com os estados do pincel que você realmente usa.

Se você salvar esse cache, o Lumi poderá carregá-lo automaticamente mais tarde, para que o pincel comece mais próximo de um estado de aquecimento, em vez de reconstruir tudo do zero.

## Fluxo de trabalho típico

1. Escolha uma predefinição de pincel que você usa com frequência.
2. Habilite a quantização para sua dinâmica.
3. Pinte normalmente por um tempo para que o cache preencha organicamente.
4. Abra o **Editor de predefinições de ferramentas** e inspecione a seção **Cache de predefinições**.
5. Observe as métricas ao vivo:
   - **Taxa de acertos**
   - **Cobertura**
   - **Memória**
6. Clique em **Salvar** quando o cache parecer útil.
7. Em sessões posteriores, o Lumi carrega automaticamente o cache salvo quando a predefinição se torna ativa.

Isso faz com que a predefinição pareça mais rápida, especialmente para pincéis com transformações caras ou carimbos grandes.

## Onde encontrar

### Editor de Dinâmica

Use o **Editor Dinâmico** para controlar a quantização:

- ativar a quantização
- escolha a contagem global de passos
- opcionalmente, substituir contagens de passos por eixo de saída

A quantização é o que torna o cache prático, reduzindo a variação contínua em caixas reutilizáveis.

### Editor de predefinições de ferramentas

Use o **Editor de predefinições de ferramentas** para gerenciar o cache da predefinição atual:

- **Salvar** — persiste o cache atual da memória no disco
- **Carregar** — restaura um cache salvo anteriormente
- **Memória Livre** — libere o cache da memória sem excluir a cópia salva
- **Remover** — exclui o cache salvo do disco

O expansor **Cache predefinido** também mostra taxa de acertos ao vivo, cobertura e uso de memória.

## O que é armazenado em cache

O cache de pincel visa carimbos de pincel transformados: os caros resultados rasterizados após tamanho, ângulo, dureza, proporção de aspecto e entradas de transformação relacionadas terem sido resolvidos.

É mais útil quando:- o pincel tem um trabalho de transformação caro
- a mesma predefinição é usada em muitas sessões
- o pincel revisita estados dinâmicos semelhantes repetidamente
- a capacidade de resposta de inicialização rápida é importante

É menos útil para pincéis cujo estado de transformação muda muito e raramente se repete.

## Carregamento Automático

Os caches salvos têm como objetivo ajudar desde o início da sessão, não apenas depois de você já ter pintado por um tempo.

Quando existe um cache salvo para a predefinição ativa, o Lumi pode carregá-lo automaticamente para que seu pincel favorito comece com muitos estados úteis já disponíveis. Isso reduz o período de inicialização a frio e deixa a escova mais próxima do pico de capacidade de resposta imediatamente.

## Segurança de memória

O cache de pincel foi projetado para melhorar a velocidade sem assumir o controle da máquina.

Lumi rastreia o uso de memória cache, expõe-o na interface do usuário e aplica limites de tempo de execução sob pressão de memória. Se o sistema tiver pouca RAM disponível, o crescimento do cache será restringido automaticamente.

## Melhores casos de uso

O cache de pincel é especialmente bom para:

- escovas favoritas do motorista diário
- pincéis texturizados usados em uma pintura
- grandes pincéis expressivos com alto custo de transformação
- predefinições de pincel compartilhadas em fluxos de trabalho de ilustração repetidos
- predefinições que você deseja sentir "prontas" assim que selecioná-las

## Resumindo

O cache do pincel permite que o Lumi aprenda os estados do pincel que você realmente usa, salve-os e traga-os de volta automaticamente mais tarde. É um recurso prático de velocidade para predefinições favoritas: pinte com o pincel, deixe o cache preencher, salve e as sessões futuras começam mais rápido.