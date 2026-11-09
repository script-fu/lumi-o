---
title: "Pintura digital de código aberto para Linux"
toc: false
type: docs
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 4f453bf4100140e9a2779d4b9806a0ef8f27e68af8a9b6f680d01a6a5d2f2341
---
[![gui-01](/images/screens/gui-00.jpg)](/images/screens/gui-01.jpg)

Lumi-o foi projetado para pintura raster em camadas: um software deliberadamente enxuto e estável. O suporte a tablets Wacom oferece a entrada fluida e responsiva exigida pela ilustração profissional.

O sistema de cores da Lumi, baseado em paletas, mistura espectralmente perfis de pigmentos do mundo real. Crie paletas a partir de relações cromáticas visualizadas, controle valores tonais e organize as cores em itens nomeados. Palette Map e Mixer ajudam a manter as decisões de cor coerentes em uma pintura ou projeto.

Construído sobre a arquitetura central do GIMP, o Lumi-o foi projetado e refinado por um artista. Suas ferramentas permanecem acessíveis e, ao mesmo tempo, oferecem a personalização necessária para fluxos de trabalho avançados. Carregamento e salvamento rápidos, além de um sistema de salvamento automático totalmente integrado, protegem seu trabalho sem interromper seu fluxo criativo.

Os traços de pincel podem receber uma textura de superfície com grão e dente, inclusive os traços caligráficos. Um traço novo, ou uma virada no traço, pode deslocar esse grão, de modo que o sombreamento de vai e vem se constrói a partir de uma fase nova a cada inversão. Os pincéis de cerdas podem ser moldados em detalhe fino, e as cerdas podem se deslocar e se renovar ao longo do traço. O OpenGL desenha a pilha de camadas na janela da imagem e pode pintar traços para predefinições de ferramenta individuais. Aerógrafos grandes e carimbos de textura grandes ficam muito mais rápidos, e a navegação na tela permanece fluida.

O carregamento diferido permite que o Lumi abra projetos grandes e complexos usando muito menos RAM. Quando um grupo está recolhido, o Lumi carrega a imagem composta salva, enquanto suas camadas, máscaras e grupos aninhados permanecem como marcadores de posição leves até que o grupo seja expandido novamente.

Lumi-o é desenvolvido no Debian 13 com o Cinnamon no X11. O GNOME no Wayland foi testado quanto à aparência geral do aplicativo e à renderização OpenGL. Se você vem do Windows, instalar o Debian é o primeiro passo. Consulte [Como instalar o Linux](/hub/install-linux/).

{{< cards >}}
  {{< card link="hub/about/" title="Sobre" icon="information-circle" >}}
  {{< card link="hub/quick-start/" title="Início rápido" icon="play" >}}
  {{< card link="hub/features/" title="Recursos" icon="book-open" >}}
{{< /cards >}}
