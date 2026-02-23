---
title: "Espaços de trabalho"
type: docs
---
Um espaço de trabalho é um instantâneo salvo de todo o seu ambiente de UI: quais painéis estão abertos e onde, as decorações e preenchimento da tela para visualização Normal e Tela Cheia, o tema ativo e o conjunto de ícones, o layout da caixa de ferramentas, a paleta ativa e as configurações de sua ferramenta. O Lumi permite que você salve quantos espaços de trabalho nomeados desejar e alterne entre eles instantaneamente – todas as imagens abertas são atualizadas no local, sem necessidade de reinicialização.

## O que um espaço de trabalho economiza

Cada espaço de trabalho nomeado armazena o seguinte de forma independente:

| Componente | O que cobre |
| :--- | :--- |
| **Layout** | Posição e tamanho da janela, disposição de encaixe (colunas do painel esquerdo e direito, quais painéis estão abertos e em que ordem), modo de janela única versus janela múltipla, estado maximizado, visibilidade e posição da barra de guias |
| **Opções de ferramentas** | As configurações atuais para cada ferramenta (tamanho do pincel, dureza, comportamento de deformação, etc.) |
| **Dispositivos de entrada** | Configuração do dispositivo de entrada: curvas de pressão, atribuições de botões, mapeamentos de eixos para caneta e outros dispositivos |
| **Decorações em tela** | Padrões por espaço de trabalho para réguas, barras de rolagem, guias, grade, realce de seleção, limite de camada e limite de tela — definidos via **Preferências → Janelas de imagem → Aparência padrão** e **Aparência de tela cheia**, independentemente para visualização Normal e Tela cheia |
| **Preenchimento de tela** | Modo de preenchimento por área de trabalho e cor para visualização Normal e Tela Cheia — definido em **Preferências → Janelas de imagem → Aparência padrão** |
| **Tema e ícones** | Tema ativo, variante de cor clara/escura, conjunto de ícones, substituição de tamanho de ícone e escala de fonte |
| **Caixa de ferramentas** | Posição do widget FG/BG (superior/inferior/esquerda/direita), escala FG/BG, visibilidade do mascote Wilber, cabeçalhos de grupos de ferramentas |

A **paleta** ativa e a **predefinição de ferramenta** também são registradas por espaço de trabalho e restauradas quando você alterna.

> **As decorações e o preenchimento da tela** são controlados por
> **Preferências → Janelas de imagem → Opções avançadas da janela → Aparência padrão** (Visualização normal)
> e **Aparência em tela cheia** (visualização em tela cheia). Ajuste essas configurações ao seu gosto,
> salve o espaço de trabalho. Os itens do **menu Exibir** (réguas, guias, etc.) são locais para o
> janela de imagem atual e não são salvos por espaço de trabalho.

### Atualizações ao vivo no switch

Quando você alterna os espaços de trabalho, todas as janelas de imagem abertas são atualizadas imediatamente – réguas, guias, barras de rolagem, cores de preenchimento e todas as outras configurações de visualização são alteradas sem a necessidade de fechar e reabrir as imagens.

## Acesso

**Editar → Preferências → Espaço de trabalho**

A seção superior da página de preferências do espaço de trabalho lista todos os seus espaços de trabalho salvos e fornece controles para gerenciá-los.

## Criando um espaço de trabalho

Configure seus painéis, ferramentas e paleta exatamente como você deseja e então:

1. Abra **Editar → Preferências → Área de Trabalho**.
2. Clique em **Salvar layout como…**.
3. Insira um nome e clique em **Salvar**.

O novo espaço de trabalho aparece no menu suspenso **Layout ativo** e no menu **Windows**.

## Alternando espaços de trabalho

Existem duas maneiras de mudar:

- **Menu Windows**: os nomes dos layouts aparecem em **Janelas → Layout** para acesso rápido na tela.
- **Preferências → Espaço de trabalho**: selecione um layout no menu suspenso **Layout ativo** e clique em **Recarregar layout**.

A troca é imediata – o Lumi reconstrói o layout do painel, restaura as opções de ferramentas, recarrega as configurações do dispositivo, atualiza as decorações da tela, o preenchimento, o tema e o layout da caixa de ferramentas, tudo sem reiniciar.

## Gerenciando espaços de trabalho

Em **Editar → Preferências → Área de trabalho**:| Ação | Efeito |
| :--- | :--- |
| **Salvar layout** | Substitui o espaço de trabalho atual pelas configurações atuais. |
| **Salvar layout como…** | Cria um novo espaço de trabalho nomeado a partir das configurações atuais. |
| **Renomear layout…** | Renomeia o espaço de trabalho selecionado. |
| **Recarregar Layout** | Aplica o espaço de trabalho selecionado imediatamente. |
| **Excluir layout…** | Remove permanentemente o espaço de trabalho selecionado e seus arquivos. |

## Configurações de persistência

A parte inferior da página de preferências do Workspace controla o que o Lumi salva automaticamente:

- **Salvar posições de janela ao sair**: Quando ativado, as posições de encaixe e de janela são gravadas no disco sempre que você sai.
- **Abrir janelas no mesmo monitor**: Reabre cada janela do monitor em que estava durante a última sessão.
- **Salvar opções da ferramenta ao sair**: salva as configurações atuais da ferramenta ao sair.
- **Salvar configurações do dispositivo de entrada ao sair**: salva a caneta e a configuração do dispositivo ao sair.

Essas configurações se aplicam a cada espaço de trabalho – cada layout mantém seu próprio estado salvo de forma independente.

## Exemplos de fluxos de trabalho

Algumas maneiras pelas quais os artistas podem usar vários espaços de trabalho:

- **Pintura** — encaixes de pincéis grandes, cores de preenchimento quentes (definidas em Preferências → Janelas de imagem → Aparência padrão), sua variante de tema preferida
- **Tinta** — guias e limites da tela ativados, barras de rolagem ativadas (definidas em Preferências → Aparência padrão), cor de preenchimento neutra
- **Ásperos** — encaixes ocultos, sem réguas ou grade, preenchimento escuro, tamanho de ícone compacto para maximizar o espaço da tela
- **Foco em tela cheia** — diferentes cores de preenchimento e configurações de decoração em Aparência de tela cheia versus Aparência padrão, portanto, alternar a tela cheia proporciona um ambiente de trabalho genuinamente diferente
- **Scripting** — painel de script aberto, aumento no tamanho da fonte para facilitar a leitura, conjunto de ícones diferente