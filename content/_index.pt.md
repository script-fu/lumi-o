---
title: "Pintura digital de código aberto para Linux"
toc: false
type: docs
translation_provenance: ai-reviewed
translation_source_sha256: 4f453bf4100140e9a2779d4b9806a0ef8f27e68af8a9b6f680d01a6a5d2f2341
translation_lock: true
---
[![gui-01](/images/screens/gui-00.jpg)](/images/screens/gui-01.jpg)

O Lumi-o foi concebido para pintura raster em camadas, deliberadamente enxuto e estável. O suporte para tablet Wacom oferece a entrada suave e reactiva que a ilustração profissional exige.

O sistema de cor baseado em paletas do Lumi mistura espectralmente perfis de pigmentos do mundo real. Construa paletas em torno de relações cromáticas visualizadas, controle o valor tonal e organize as cores como entradas nomeadas. A Palette Map e o Mixer ajudam a manter as decisões de cor coerentes numa pintura ou num projecto.

Construído sobre a arquitectura central do GIMP, o Lumi-o foi concebido e refinado por um artista. As suas ferramentas permanecem acessíveis, ao mesmo tempo que oferecem a personalização necessária para fluxos de trabalho avançados. Carregamento e gravação rápidos, além de um sistema de gravação automática totalmente integrado, protegem o trabalho sem interromper o fluxo.

Os traços de pincel podem receber uma textura de superfície com grão e dente, incluindo os traços caligráficos. Um traço novo, ou uma viragem no traço, pode deslocar esse grão, de modo que o sombreado de vai-e-vem se constrói a partir de uma fase nova em cada inversão. Os pincéis de cerdas podem ser moldados em pormenor fino, e as cerdas podem deslocar-se e renovar-se ao longo do traço. O OpenGL desenha a pilha de camadas na janela da imagem e pode pintar traços para predefinições de ferramenta individuais. Os aerógrafos grandes e os carimbos de textura grandes são muito mais rápidos, e a navegação na tela mantém-se fluida.

O carregamento diferido permite que o Lumi abra projectos grandes e complexos usando muito menos RAM. Quando um grupo está recolhido, o Lumi carrega a imagem composta guardada, enquanto as suas camadas, máscaras e grupos aninhados permanecem como marcadores de posição leves até serem expandidos.

O Lumi-o é desenvolvido no Debian 13 com o Cinnamon em X11. O GNOME em Wayland foi testado quanto ao aspecto geral da aplicação e à renderização OpenGL. Se vem do Windows, instalar o Debian é o primeiro passo. Consulte [Como instalar Linux](/hub/install-linux/).

{{< cards >}}
  {{< card link="hub/about/" title="Sobre" icon="information-circle" >}}
  {{< card link="hub/quick-start/" title="Início rápido" icon="play" >}}
  {{< card link="hub/features/" title="Funcionalidades" icon="book-open" >}}
{{< /cards >}}
