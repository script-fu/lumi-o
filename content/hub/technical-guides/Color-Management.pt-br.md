---
title: "Gerenciamento de cores"
type: docs
weight: 15
url: "hub/technical-guides/Color-Management"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 60e00f1b5e0b4a7bb3034ca99dd3f8f51f6bc52b1629a9ab717d2ac2166393ee
---

O Lumi-o está configurado para funcionar imediatamente. Contanto que você esteja trabalhando em uma imagem com **precisão de 16 bits ou superior**, o software já está configurado para usar o pacote padrão de soft proof (CMYK) e os perfis sRGB integrados; tudo deve funcionar sem nenhuma configuração.

Para quem precisa de controle mais avançado, este guia explica o modelo central de gerenciamento de cores do Lumi, a diferença entre um perfil de imagem e um perfil de soft proof, onde ficam os controles e exatamente como os perfis padrão são incluídos com o aplicativo.

## Resumo rápido

O Lumi usa três funções de perfil diferentes:

1. **Perfil de trabalho da imagem**
   - Define o que significam os valores RGB ou em tons de cinza da imagem.
   - Usado para operações de atribuição e conversão.
   - Exemplos típicos: sRGB integrado, Adobe RGB.

2. **Perfil de monitor**
   - Descreve seu monitor.
   - Usado para exibir a imagem corretamente na tela.
   - Geralmente fornecido pelo sistema ou escolhido em Preferências.

3. **Perfil de soft proof**
   - Simula outro dispositivo de saída ou condição de impressão.
   - **Não** redefine os valores de pixel da imagem.
   - Exemplos típicos: perfis de impressão CMYK como `CoatedFOGRA39`.

## Perfil de imagem versus perfil de soft proof

### Perfil de imagem

Use isto quando quiser informar ao Lumi em qual espaço de cores a imagem realmente está.

Duas operações comuns:

- **Atribuir perfil**
  - Altera a etiqueta do perfil anexada à imagem.
  - **Não** converte valores de pixel.
  - Use somente quando os valores de pixel já estiverem no espaço desse perfil.

- **Converter para perfil**
  - Converte valores de pixel do perfil de imagem atual para um novo.
  - Use quando quiser que a imagem realmente passe para um espaço de trabalho diferente.

**Localizações no menu:**
- Imagem > Gerenciamento de cores > Atribuir perfil de cores...
- Imagem > Gerenciamento de cores > Converter para perfil de cores...

### Perfil de soft proof

Use isto quando quiser visualizar como a imagem seria reproduzida em um dispositivo de destino ou em uma condição de impressão.

Soft proof:
- mantém o espaço de trabalho da imagem inalterado
- altera o pipeline de visualização
- pode marcar cores fora da gama
- destina-se à visualização, não à reatribuição de dados de imagem

**Localizações no menu:**
- Imagem > Gerenciamento de cores > Configurações de soft proof > Escolher perfil de soft proof...
- Imagem > Gerenciamento de cores > Configurações de soft proof > Intenção de renderização
- Imagem > Gerenciamento de cores > Configurações de soft proof > Compensação de ponto preto
- Exibir > Gerenciamento de cores > Ativar visualização de soft proof
- Exibir > Gerenciamento de cores > Marcar cores fora da gama

## Como ver a visualização de soft proof

Existem dois pontos de entrada principais para alternar soft proofs.

### 1. Menu Exibir

Use:
- Exibir > Gerenciamento de cores > Ativar visualização de soft proof

Isso liga ou desliga a simulação de visualização para a tela atual.

### 2. Alternador na barra de status

O Lumi também expõe o soft proofing diretamente na barra de status inferior.

- **Clique esquerdo** (alternar): ativar ou desativar cores de prova
- **Clique com o botão direito**: abra o popover de soft proofing, onde você pode ajustar:
  - perfil atual
  - seletor de perfil
  - intenção de renderização
  - compensação de ponto preto
  - marcação fora da gama

{{< callout type="warning" >}}
**Nota importante sobre precisão**
A visualização de soft proof só está habilitada para imagens de **16 bits e 32 bits**.
Para imagens de **8 bits**, o alternador fica desativado e o Lumi solicitará que você converta a precisão para uma profundidade maior antes de visualizar as cores com precisão.
{{< /callout >}}

## Preferências e padrões

Os padrões globais ficam em:
- Editar > Preferências > Gerenciamento de cores

Seções relevantes:
- **Perfil de monitor manual**
- **Perfil RGB preferido**
- **Perfil em tons de cinza preferido**
- **Soft proofing**

### Padrões atuais do Lumi

#### Espaços de trabalho

ICCs de espaço de trabalho incluídos, atualmente oferecidos na pasta de dados compartilhada:
- `AdobeRGB1998.icc`
- `AppleRGB.icc`

Para trabalho sRGB padrão, o Lumi também fornece um **perfil de trabalho sRGB integrado internamente**.

#### Padrões de soft proof

Perfis de soft proof incluídos, atualmente instalados:
- `CoatedFOGRA39.icc`
- `USWebCoatedSWOP.icc`
- `JapanColor2001Coated.icc`

Quando disponível, `CoatedFOGRA39.icc` é usado como perfil de referência padrão de soft proof/CMYK incluído.

## Fluxos de trabalho práticos

### Para pintura e trabalho normal na tela

- Mantenha a imagem no sRGB integrado ou em outro espaço de trabalho RGB válido.
- Deixe o Lumi usar o perfil de monitor do sistema, se disponível.

### Para visualização de impressão

- Mantenha a imagem em seu espaço de trabalho RGB padrão.
- Escolha um perfil de soft proof que corresponda à condição de impressão desejada (por exemplo, FOGRA39).
- Ative a visualização de soft proof.
- Opcionalmente, ative avisos de gama para ver intenções de renderização cortadas.
