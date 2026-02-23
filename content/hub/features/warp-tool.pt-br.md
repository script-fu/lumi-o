---
title: "Ferramenta Warp"
type: docs
---
A ferramenta Warp empurra, puxa e flui pixels livremente pela tela. No Lumi, ele vai além da maioria das implementações: ele pode distorcer um grupo inteiro de camadas — não importa quantas camadas e máscaras aninhadas ele contenha — como um único objeto unificado, sem achatar ou perder qualquer estrutura.

## Visão geral

Selecione uma camada e arraste-a para deslocar os pixels em qualquer direção. A distorção não é destrutiva enquanto você trabalha: você pode desfazer e refazer traços individuais, alterar o tamanho do pincel ou o comportamento entre os traços e continuar refinando até confirmar. A confirmação aplica o mapa de deslocamento acumulado de forma destrutiva aos dados de pixel da camada.

Quando uma **camada de grupo** é selecionada, a ferramenta opera no grupo como um todo. Você vê e interage com uma visualização ao vivo de todo o grupo composto. No commit, a mesma distorção é aplicada de forma precisa e independente a cada camada filha e máscara dentro do grupo, preservando a estrutura completa da camada.

## Grupo Warp

Distorcer um grupo é o principal recurso que diferencia a ferramenta de distorção do Lumi.

### O problema que ele resolve

Na maioria dos programas de pintura, distorcer uma ilustração multicamadas requer primeiro achatar o grupo (destruindo a estrutura da camada) ou distorcer cada camada separadamente e tentar combiná-las a olho nu (tedioso e impreciso). Nenhuma das abordagens preserva a estrutura original para futuras edições não destrutivas.

Lumi distorce todo o grupo como um item e então distribui exatamente a mesma transformação para cada camada dentro dele.

### Como funciona

Quando você seleciona um grupo e inicia um traçado de distorção, o Lumi cria uma **camada de visualização flutuante** a partir da projeção composta do grupo. Se o grupo tiver uma máscara, a máscara será inserida na visualização para que a visualização represente com precisão a aparência final. Você pinta seus traços de distorção diretamente nesta visualização – o que você vê é exatamente o que obtém.

No commit, Lumi:

1. Aplica o deslocamento a cada camada básica dentro do grupo (incluindo camadas profundamente aninhadas em subgrupos), expandindo a tela de cada camada apenas o suficiente para capturar toda a área de distorção.
2. Aplica o mesmo deslocamento a todas as máscaras dentro do grupo na mesma passagem.
3. Retoma o cálculo automático dos limites do grupo para que o grupo seja redimensionado para caber nos filhos recém-deformados.
4. Corta cada camada distorcida de volta ao seu conteúdo pintado real para manter o tamanho dos arquivos compacto.
5. Remove a camada de visualização e regenera a projeção do grupo dos filhos atualizados.

Tudo isso acontece em uma única etapa de desfazer. Após a confirmação, o grupo fica exatamente como na visualização, com todas as camadas e máscaras intactas.

### Máscaras

A opção **Warp Masks** (habilitada por padrão) faz com que as máscaras em cada camada e grupo dentro do alvo de distorção recebam a transformação de deslocamento idêntica. As máscaras de camada se movem com suas camadas: uma máscara que estava cortando o contorno de um personagem continua a cortar o mesmo contorno após a distorção.

Quando **Warp Masks** está desativado, apenas o conteúdo da camada é deslocado; as máscaras mantêm suas posições originais.

## Opções de ferramentas

### Comportamento

| Modo | Efeito |
| :--- | :--- |
| **Mover** | Empurra os pixels na direção do traço. O modo principal para a maioria dos trabalhos de deformação. |
| **Crescer** | Expande os pixels para fora do centro do pincel. |
| **Encolher** | Puxa os pixels para dentro em direção ao centro do pincel. |
| **Gire no sentido horário** | Gira os pixels no sentido horário ao redor do centro do pincel. |
| **Gire no sentido anti-horário** | Gira os pixels no sentido anti-horário ao redor do centro do pincel. |
| **Apagar** | Remove o deslocamento de distorção, restaurando os pixels às suas posições originais. |
| **Suave** | Difunde o deslocamento, suavizando transições abruptas entre áreas deformadas e não deformadas. |

### Controles de pincel

- **Tamanho**: Diâmetro do pincel warp em pixels. Pincéis maiores deslocam áreas mais amplas com uma queda mais suave; pincéis menores proporcionam controle preciso e localizado.
- **Dureza**: Queda do centro para a borda. A alta dureza produz um deslocamento uniforme em toda a área do pincel; a baixa dureza concentra o efeito no centro.
- **Força**: a distância que os pixels são deslocados por traço. A menor resistência permite uma modelagem sutil e gradual; maior força produz movimentos dramáticos e rápidos.

### Tempo de AVC

- **Stroke Durante o Movimento** (apenas modo Mover): Aplica distorção continuamente à medida que o mouse se move, em vez de apenas em um pulso de temporizador. Use para pinceladas fluidas, semelhantes a pincel, onde você deseja que o deslocamento siga o cursor diretamente.
- **Stroke Periodically**: Aplica warp em um intervalo de tempo fixo enquanto o botão do mouse é pressionado. Use para os modos Crescer, Encolher e Swirl onde a aplicação circular contínua é a intenção.
- **Taxa**: A frequência da aplicação periódica do golpe.

### Qualidade

- **Interpolação**: o método de amostragem usado ao confirmar. Linear é rápido e suave para a maioria dos trabalhos; Cubic e Nohalo proporcionam maior fidelidade para detalhes finos.
- **Visualização de alta qualidade**: usa o amostrador de qualidade de confirmação durante a visualização interativa. Mais lento, mas a visualização corresponde exatamente ao resultado confirmado.

### Opções de grupo

- **Expandir área de distorção** (somente distorção de grupo): O número de pixels adicionados como uma margem transparente ao redor da visualização do grupo em todos os lados. Isso dá espaço para o conteúdo deslocado se mover. O padrão de 256 px é suficiente para a maioria dos trabalhos; reduza-o para imagens grandes onde a memória é importante ou aumente-o para traços de deslocamento muito grandes.
- **Máscaras de distorção**: se a mesma distorção deve ser aplicada a máscaras de camada e de grupo. Ativado por padrão.

## Desfazer e refazer

Cada golpe é uma etapa discreta de desfazer na sessão de warp. **Ctrl+Z** remove o último traço e restaura o mapa de deslocamento ao seu estado anterior. **Ctrl+Y** (ou **Ctrl+Shift+Z**) reaplica-o. Você pode percorrer todo o histórico de AVC antes de cometer.

Pressionar **Escape** ou alternar ferramentas descarta todos os traços não confirmados e restaura as camadas ao seu estado original. Nenhuma alteração é gravada até que você confirme explicitamente.

## Comprometendo

Clique no botão **Commit** (ou pressione **Enter**) para aplicar a distorção acumulada de forma destrutiva. Para warps de grupo, isso aciona a aplicação multicamada completa descrita acima. O histórico de desfazer para o warp confirmado é então uma única entrada na pilha de desfazer da imagem, reversível com o padrão **Editar → Desfazer**.