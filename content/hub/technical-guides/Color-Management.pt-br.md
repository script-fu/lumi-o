---
title: "Gerenciamento de cores"
type: docs
weight: 15
---
O Lumi-o está configurado para funcionar imediatamente. Contanto que você esteja trabalhando em uma imagem com **precisão de 16 bits ou superior**, o software já estará configurado para usar o pacote padrão de prova digital (CMYK) e perfis sRGB integrados; tudo deve funcionar sem qualquer configuração.

Para aqueles que precisam de um controle mais profundo, este guia explica o modelo central de gerenciamento de cores do Lumi, a diferença entre um perfil de imagem e um perfil de prova digital, onde ficam os controles e exatamente como os perfis padrão são agrupados com o aplicativo.

## Resumo rápido

Lumi usa três funções de perfil diferentes:

1. **Perfil de trabalho de imagem**
   - Define o que significam os números RGB ou em tons de cinza da imagem.
   - Usado para operações de atribuição/conversão.
   - Exemplos típicos: sRGB integrado, Adobe RGB.

2. **Exibir perfil**
   - Descreve seu monitor.
   - Usado para mostrar a imagem corretamente na tela.
   - Geralmente fornecido pelo sistema ou escolhido em Preferências.

3. **Perfil à prova de software**
   - Simula outro dispositivo de saída ou condição de impressão.
   - **não** redefine os valores de pixel da imagem.
   - Exemplos típicos: perfis de impressão CMYK como `CoatedFOGRA39`.

## Perfil de imagem versus perfil à prova de software

### Perfil de imagem

Use isto quando quiser dizer ao Lumi em que espaço de cores a imagem realmente está.

Duas operações comuns:

- **Atribuir perfil**
  - Altera a etiqueta do perfil anexada à imagem.
  - **não** converte valores de pixel.
  - Use somente quando os números dos pixels já estiverem no espaço desse perfil.

- **Converter para perfil**
  - Converte valores de pixel do perfil de imagem atual para um novo.
  - Use quando quiser que a imagem realmente se mova para um espaço de trabalho diferente.

**Localizações do menu:**
- Imagem > Gerenciamento de cores > Atribuir perfil de cores...
- Imagem > Gerenciamento de cores > Converter para perfil de cores...

### Perfil à prova de soft

Use isto quando quiser visualizar como a imagem seria reproduzida em um dispositivo de destino ou condição de impressão.

Impermeabilização suave:
- deixa o espaço de trabalho da imagem sozinho
- altera o pipeline de visualização
- pode marcar cores fora da gama
- destina-se à visualização, não à reatribuição de dados de imagem

**Localizações do menu:**
- Imagem > Gerenciamento de cores > Configurações de prova suave > Escolher perfil de prova suave...
- Imagem > Gerenciamento de cores > Configurações de prova suave > Intenção de renderização
- Imagem > Gerenciamento de cores > Configurações de prova suave > Compensação de ponto preto
- Exibir> Gerenciamento de cores> Ativar visualização de prova digital
- Exibir> Gerenciamento de cores> Marcar cores fora da gama

## Como ver a visualização da prova digital

Existem dois pontos de entrada principais para alternar as provas digitais.

### 1. Menu Ver

Usar:
- Exibir> Gerenciamento de cores> Ativar visualização de prova digital

Isso ativa ou desativa a simulação de visualização da exibição atual.

### 2. Alternar barra de status

Lumi também expõe a prova digital diretamente na barra de status inferior.

- **Clique esquerdo** (alternar): ativar ou desativar cores de prova
- **Clique com o botão direito**: abra o popover de prova digital onde você pode ajustar:
  - perfil atual
  - seletor de perfil
  - intenção de renderização
  - compensação de ponto preto
  - marcação fora da gama

{{< callout type="warning" >}}
**Nota importante sobre precisão**
A visualização de prova digital só está habilitada para imagens de **16 bits e 32 bits**.
Para imagens de **8 bits**, a alternância é desativada e o Lumi solicitará que você converta a precisão para uma profundidade maior antes de visualizar as cores com precisão.
{{< /callout >}}

## Preferências e padrões

Os padrões globais residem em:
- Editar > Preferências > Gerenciamento de coresSeções relevantes:
- **Perfil de monitor manual**
- **Perfil RGB preferido**
- **Perfil em tons de cinza preferido**
- **Prova suave**

### Padrões atuais do Lumi

#### Espaços de Trabalho

ICCs de espaço de trabalho agrupados atualmente oferecidos na pasta de dados compartilhada:
- `AdobeRGB1998.icc`
- `AppleRGB.icc`

Para trabalho sRGB padrão, o Lumi também fornece um **perfil de trabalho sRGB integrado internamente**.

#### Padrões de prova suave

Perfis à prova de software incluídos atualmente instalados:
- `CoatedFOGRA39.icc`
-`USWebCoatedSWOP.icc`
-`JapanColor2001Coated.icc`

Quando disponível, `CoatedFOGRA39.icc` é usado como perfil de referência CMYK/prova digital padrão.

## Fluxos de trabalho práticos

### Para pintura e trabalho normal na tela

- Mantenha a imagem no sRGB integrado ou em outro espaço de trabalho RGB válido.
- Deixe o Lumi usar o perfil do monitor do sistema, se disponível.

### Para visualização de impressão

- Mantenha a imagem em seu espaço de trabalho RGB padrão.
- Escolha um perfil de prova digital que corresponda à condição de impressão desejada (por exemplo, FOGRA39).
- Ative a visualização à prova de software.
- Opcionalmente, ative avisos de gama para ver intenções de renderização cortadas.