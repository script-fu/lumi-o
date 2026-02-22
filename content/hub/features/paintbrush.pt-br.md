---
title: "Ferramenta Pincel"
type: docs
---
O Pincel é a principal ferramenta de pintura, projetada para pinceladas responsivas e inteligentes, com controle total sobre pressão, velocidade, inclinação e dinâmica de espaçamento.

## Visão geral

A ferramenta Pincel oferece suporte a tipos de pincel raster, gerado processualmente e animado. Os traços podem ser estabilizados, suavizados e pós-processados. A dinâmica do pincel responde à entrada da caneta, proporcionando controle preciso sobre opacidade, tamanho, cor, ângulo e outras propriedades durante um traço.

## Tipos de pincel

### Pincéis raster
Imagens de pincel bitmap carregadas de arquivos `.png` ou `.vbr`. Suporta transparência alfa e quadros de tubos animados.

### Pincéis gerados
Formas renderizadas processualmente (Círculo, Quadrado, Diamante, Triângulo) com parâmetros ajustáveis: dureza, proporção de aspecto, ângulo, redondeza e raio do canto. Os pincéis gerados são leves e escaláveis.

### Pincéis animados (.anim)
Sequências de quadros sequenciais que avançam durante os traços. Os quadros podem ser alternados de forma incremental (avanços de quadro por dab), selecionados aleatoriamente por dab ou indexados por dinâmica (pressão, velocidade, inclinação, ângulo).

## Cursor de Pintura
O cursor se adapta ao estado atual da ferramenta para fornecer feedback claro e contextual:

- **Contorno do pincel**: o cursor rastreia a forma e o tamanho exatos do pincel, fornecendo uma visualização ao vivo de onde a tinta irá pousar.
- **Modo Apagar**: quando o apagamento está ativo, o contorno muda para um círculo tracejado para distinguir visualmente os traços de apagamento dos traços de pintura.
- **Limite de pincel simples**: para pincéis complexos ou muito grandes onde a renderização do contorno preciso é cara, ative **Limite de pincel simples** (em Opções adicionais) para usar um círculo simples.

## Opções de ferramentas

### Controles de nível superior
Presente em todos os momentos, fora de qualquer expansor:
- **Modo**: Modo de mesclagem de pintura (Normal, Multiplicar, Tela, etc.)
- **Opacidade**: Opacidade geral do traço (0–100).

### Opções de pincel
No expansor **Opções de pincel** (expandido por padrão):
- **Tamanho**: Diâmetro do pincel em pixels.
- **Proporção**: esmague ou estique o formato do pincel (-1,0–1,0). 0 = não modificado; valores negativos giram a abóbora 90°.
- **Ângulo**: gira o carimbo do pincel (-180–180°). Independente da dinâmica da direção do curso.
- **Espaçamento**: Distância entre salpicos pintados como uma porcentagem do tamanho do pincel. Inferior = traços mais suaves; superior = padrão disperso.
- **Dureza**: Desbotamento suave (0,0) até borda nítida (1,0).
- **Força**: Força de aplicação do pincel (0,0–1,0). Oculto para a ferramenta Lápis.
- **Jitter**: compensa aleatoriamente cada posição de salpico em até esse número de pixels (0–1024).
- **Borracha**: Multiplicador de tamanho aplicado quando este pincel é usado como borracha (0,1–10,0). Não mostrado na própria ferramenta Borracha.

### Efeitos de acidente vascular cerebral
No expansor **Efeitos de traço**:
- **Pós-processo**: aplica estabilização, compressão de velocidade e correção de repetição após a conclusão do golpe, melhorando a consistência sem latência.
  - **Limiar de giro**: Limite de ângulo (0–180°) para correção de direção em cantos agudos. 0 = correção de direção de salto.
  - **Velocidade de visualização**: Suprime a visualização pós-processamento quando a velocidade do traço excede esse valor (0 = sempre visualização).
- **Build-Up**: quando ativado, cada pincelada acumula opacidade em vez de ser composta como um único traço.#### Caligráfico
Quando ativo, a estampagem salpicada é substituída por um corredor geométrico contínuo:
- **Largura** e **Altura**: Dimensões do corredor caligráfico.
- **Ângulo**: Orientação da ponta (graus).
- **Opacidade Dinâmica**: Modula a opacidade dentro do traço com base nas mudanças de velocidade e direção. Funciona melhor em movimentos finos e controlados; os resultados são menos previsíveis em rabiscos rápidos. Experimental.
- **Crescimento de velocidade** (0–100%): Aumento de tamanho máximo permitido por amostra como uma porcentagem do tamanho da amostra anterior. Limita a rapidez com que uma dinâmica de tamanho orientada por velocidade pode crescer, evitando saltos repentinos quando o curso acelera.
- **Velocity Shrink** (0–100%): Diminuição máxima permitida do tamanho por amostra. Limita a rapidez com que o tamanho pode diminuir quando o curso desacelera.

#### Suavização
Ativa a suavização de entrada em tempo real aplicada ao caminho do traço conforme você pinta. Expande para revelar:
  - **Profundidade** (2–256): Número de amostras de entrada anteriores consideradas ao calcular a posição suavizada. Valores mais altos produzem um atraso mais longo e mais comprometido.
  - **Posição** (0–100): Intensidade de suavização aplicada à posição do pincel. Valores mais altos completam mudanças bruscas de direção.
  - **Pressão** (0–100): Suavização aplicada ao sinal de pressão da caneta, reduzindo picos de pressão e instabilidade.
  - **Direção** (0–100): Suavização aplicada à direção do traço, estabilizando a dinâmica sensível ao ângulo.

#### Dinâmica
Atribua a entrada da caneta ou outros valores ativos aos parâmetros de pintura:

- **Pressão** (caneta): controla tamanho, opacidade, taxa, dureza, cor e muito mais com base na pressão da caneta.
- **Velocidade**: mapeia a velocidade do traço para as propriedades do pincel.
- **Inclinação**: os ângulos de inclinação X e Y da caneta afetam o ângulo e outros parâmetros.
- **Roda**: entrada da roda do mouse ou da roda da caneta.
- **Direção**: Ângulo da direção do curso.
- **Fade**: esmaece a opacidade ou o tamanho em um número fixo de pinceladas.

Cada entrada dinâmica pode ser mapeada para diversas propriedades de forma independente. Abra **Opções de ferramentas** → **Dinâmica** para configurar.

#### Desbotamento e cor
No expansor **Fade and Colour** (aninhado dentro de Stroke Effects; visível apenas quando **Dynamics System** está ativado):

- **Ângulo Inicial Relativo**: O valor do **Ângulo Inicial** é interpretado em relação à direção do traço e não como um ângulo absoluto da tela.
- **Fade Initial Angle**: Desvanece do **Initial Angle** no início do traço em direção ao ângulo dinâmico ao vivo ao longo do curso do traço. Ativar isso força o **Ângulo Inicial Relativo** ativado.
- **Ângulo inicial** (-180–180°): O ângulo do pincel bem no início de uma pincelada, antes que a dinâmica assuma o controle.
- **Angle Blend Factor** (0,0–1,0): controla a rapidez com que o ângulo do pincel faz a transição do ângulo inicial para o ângulo dinâmico. 0 = mantém o ângulo inicial; 1 = utiliza imediatamente o ângulo totalmente dinâmico.
- **Estabilização de direção** (0–100 px): Atrasa a dinâmica sensível à direção, exigindo que o ponteiro percorra essa quantidade de pixels antes de atualizar a direção do traço. Ativo somente quando **Pós-processo** está desativado (O pós-processamento fornece sua própria estabilização). 0 = desabilitado (direção imediata, pode saltar no início do curso).
- **Fade Length**: Distância em unidades de tela sobre a qual o fade ocorre.
- **Repeat**: Como o fade é repetido quando o comprimento do fade se esgota (None, Loop, Sawtooth, Triangle).


### Cabeças de escova
Pinte com múltiplas cabeças de pincel independentes dispostas em um anel de formação ao redor do traçado. Os controles aparecem no expansor **Cabeças de pincel** no painel de opções de ferramentas.- **Cabeças**: Número de cabeças de escova simultâneas (1–16).
- **Tamanho**: tamanho renderizado de cada cabeça em relação ao tamanho global do pincel (0,1–1,0).
- **Rigidez**: quão rigidamente o raio da órbita segue o tamanho do pincel em escala dinâmica. 0 = a órbita rastreia o tamanho da dinâmica; 1 = a órbita permanece fixa ao tamanho da base.
- **Segue** (0,0–1,0): Quão fortemente o anel de formação acompanha a direção de deslocamento do curso. Em 1,0 (padrão), o anel é sempre perpendicular à direção de deslocamento. Em 0,0, ele é bloqueado no valor estático **Angle**. Os valores intermediários se misturam entre as duas orientações. Isto é independente do sistema Dynamics – nenhuma configuração de dinâmica angular é necessária.
- **Ângulo** (0–360°): Orientação estática do anel de formação, usado quando **Segue** está abaixo de 1,0. Quando **Bloquear para visualizar** está ativo, o ângulo é automaticamente compensado pela rotação da tela.
- **Variação**: variação de tamanho por cabeça e viés de pressão aplicado à dinâmica.
- **Variação de opacidade**: variação de opacidade por cabeça, independente da variação de tamanho.
- **Semente**: Semente aleatória corrigida para variação por cabeça. Aplica-se apenas quando **Random Bristles** está desativado.
- **Cerdas aleatórias**: Randomiza o caractere das cerdas a cada traço (ignora Seed).
- **Quadros independentes**: para pincéis animados — quando ativado, cada cabeçote avança seu quadro de animação de forma independente.

### Opções Adicionais

No expansor **Opções adicionais** (recolhido por padrão):

- **Bloquear na visualização**: mantém a aparência do pincel fixa em relação à visualização da tela — quando você gira a tela, o pincel gira com ela.
- **Limite do pincel simples**: usa um círculo simples para o contorno do cursor do pincel em vez de renderizar a forma completa do pincel. Útil para pincéis complexos ou grandes onde o desenho do limite preciso é caro.
- **Jitter uniforme**: Quando ativado, os deslocamentos de salpicos do controle deslizante **Jitter** são extraídos de uma distribuição uniforme (cada deslocamento é igualmente provável dentro do intervalo). Quando desativado, a distribuição é gaussiana (desloca o cluster em direção ao centro).
- **Restaurar as últimas cores usadas**: restaura as cores de primeiro plano e de fundo da sessão anterior na inicialização, em vez de usar preto e branco como padrão.
- **Horizontal Aleatório**: 50% de chance de espelhar cada carimbo da esquerda para a direita por pincelada.
- **Vertical Aleatório**: 50% de chance de virar cada carimbo de cabeça para baixo por pincelada.
- **Rotação aleatória**: gira aleatoriamente cada carimbo em 0°, 90°, 180° ou 270° por pincelada.
- **Redefinir animação**: para pincéis animados — quando ativado, a animação reinicia a partir do quadro 0 a cada novo traço; quando desligado, continua de onde o golpe anterior terminou.