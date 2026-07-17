---
title: "Gestão de cores"
type: docs
weight: 15
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 60e00f1b5e0b4a7bb3034ca99dd3f8f51f6bc52b1629a9ab717d2ac2166393ee
url: "hub/technical-guides/Color-Management"
---
O Lumi-o está configurado para funcionar de imediato. Enquanto trabalhar numa imagem com **precisão de 16 bits ou superior**, o software já está preparado para usar a prova de impressão predefinida incluída (CMYK) e perfis sRGB integrados; tudo deve funcionar sem configuração adicional.

Para quem precisa de controlo mais profundo, este guia explica o modelo central de gestão de cores do Lumi, a diferença entre um perfil de imagem e um perfil de prova de impressão, onde ficam os controlos e como os perfis predefinidos são incluídos com a aplicação.

## Resumo rápido

O Lumi usa três papéis de perfil diferentes:

1. **Perfil de trabalho da imagem**
   - Define o que significam os valores RGB ou em escala de cinzentos da imagem.
   - Usado em operações de atribuição/conversão.
   - Exemplos típicos: sRGB integrado, Adobe RGB.

2. **Perfil de ecrã**
   - Descreve o monitor.
   - Usado para mostrar a imagem correctamente no ecrã.
   - Normalmente fornecido pelo sistema ou escolhido em Preferências.

3. **Perfil de prova de impressão**
   - Simula outro dispositivo de saída ou condição de impressão.
   - **Não** redefine os valores dos pixéis da imagem.
   - Exemplos típicos: perfis CMYK de impressão como `CoatedFOGRA39`.

## Perfil de imagem vs. perfil de prova de impressão

### Perfil de imagem

Use isto quando quiser indicar ao Lumi em que espaço de cor a imagem se encontra efectivamente.

Duas operações comuns:

- **Atribuir perfil**
  - Altera a etiqueta de perfil associada à imagem.
  - **Não** converte valores de pixéis.
  - Use apenas quando os valores dos pixéis já estão nesse espaço de perfil.

- **Converter para perfil**
  - Converte valores de pixéis do perfil actual da imagem para um novo.
  - Use quando quiser que a imagem passe efectivamente para outro espaço de trabalho.

**Localizações no menu:**
- Imagem > Gestão de cores > Atribuir perfil de cor...
- Imagem > Gestão de cores > Converter para perfil de cor...

### Perfil de prova de impressão

Use isto quando quiser pré-visualizar como a imagem seria reproduzida num dispositivo de destino ou condição de impressão.

A prova de impressão:

- mantém o espaço de trabalho da imagem inalterado
- altera o pipeline de pré-visualização
- pode marcar cores fora da gama
- destina-se à pré-visualização, não à reatribuição de dados da imagem

**Localizações no menu:**
- Imagem > Gestão de cores > Definições de prova de impressão > Escolher perfil de prova de impressão...
- Imagem > Gestão de cores > Definições de prova de impressão > Intenção de renderização
- Imagem > Gestão de cores > Definições de prova de impressão > Compensação do ponto preto
- Ver > Gestão de cores > Activar pré-visualização de prova de impressão
- Ver > Gestão de cores > Marcar cores fora da gama

## Como ver a pré-visualização de prova de impressão

Existem dois pontos de entrada principais para activar provas de impressão.

### 1. Menu Ver

Use:

- Ver > Gestão de cores > Activar pré-visualização de prova de impressão

Isto activa ou desactiva a simulação de pré-visualização no ecrã actual.

### 2. Alternador na barra de estado

O Lumi também expõe a prova de impressão directamente na barra de estado inferior.

- **Clique esquerdo** (alternar): activar ou desactivar cores de prova
- **Clique com o botão direito**: abrir o popover de prova de impressão, onde pode ajustar:
  - perfil actual
  - selector de perfil
  - intenção de renderização
  - compensação do ponto preto
  - marcação fora da gama

{{< callout type="warning" >}}
**Nota importante sobre precisão**
A pré-visualização de prova de impressão só está activa para imagens de **16 e 32 bits**.
Para imagens de **8 bits**, o alternador fica desactivado e o Lumi pede que converta a precisão para uma profundidade superior antes de pré-visualizar cores com exactidão.
{{< /callout >}}

## Preferências e predefinições

As predefinições globais estão em:

- Editar > Preferências > Gestão de cores

Secções relevantes:

- **Perfil de monitor manual**
- **Perfil RGB preferido**
- **Perfil em escala de cinzentos preferido**
- **Prova de impressão**

### Predefinições actuais do Lumi

#### Espaços de trabalho

ICCs de espaço de trabalho incluídos actualmente oferecidos a partir da pasta de dados partilhada:

- `AdobeRGB1998.icc`
- `AppleRGB.icc`

Para trabalho sRGB padrão, o Lumi também fornece internamente um **perfil de trabalho sRGB integrado**.

#### Predefinições de prova de impressão

Perfis de prova de impressão incluídos actualmente instalados:

- `CoatedFOGRA39.icc`
- `USWebCoatedSWOP.icc`
- `JapanColor2001Coated.icc`

Quando disponível, `CoatedFOGRA39.icc` é usado como perfil de referência CMYK/prova de impressão predefinido incluído.

## Fluxos de trabalho práticos

### Para pintura e trabalho normal no ecrã

- Mantenha a imagem em sRGB integrado ou noutro espaço de trabalho RGB válido.
- Deixe o Lumi usar o perfil de monitor do sistema, se disponível.

### Para pré-visualização de impressão

- Mantenha a imagem no seu espaço de trabalho RGB habitual.
- Escolha um perfil de prova de impressão que corresponda à condição de impressão pretendida (por exemplo, FOGRA39).
- Active a pré-visualização de prova de impressão.
- Opcionalmente, active avisos de gama para ver intenções de renderização cortadas.
