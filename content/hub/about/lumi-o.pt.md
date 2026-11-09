---
title: "Lumi-o"
type: docs
url: "hub/about/lumi-o"
weight: 1
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 060cd1cd6261e7278b4bc6e6c7330a374ecb20b5368faedde03a6e15da424d12
---
O Lumi é uma aplicação rápida e eficiente, exclusiva para Linux, dedicada à criação de imagens raster, desenvolvida em aberto. As decisões de design, a documentação de arquitetura e o historial de desenvolvimento são públicos para que os utilizadores possam compreender a sua evolução.

O Lumi privilegia a pintura e a ilustração digitais, mantendo capacidade para edição estruturada de imagem e correção fotográfica. Ajustes corretivos, efeitos de estilo de lente, graduação de cor baseada no tom e camadas de filtro não destrutivas cobrem uma vasta gama de fluxos de edição.

O Lumi assenta na ideia de que um software de pintura digital deve comportar-se como uma ferramenta de estúdio fiável: previsível, transparente e centrada no acto de criar imagens.

## Objectivo

O Lumi suporta um método estruturado e não destrutivo de criação de imagens, quer a origem seja pintada, desenhada ou fotográfica. É uma alternativa focada e assertiva tanto a editores de imagem generalistas como a software de pintura dedicado: gratuito e de código aberto, sem subscrições, aprisionamento, dependência da cloud ou geração de imagens por IA.

A fiabilidade e o acesso a longo prazo são características centrais. O formato de ficheiro aberto baseado em directórios permanece legível sem software proprietário, com importação e exportação XCF e PSD.

## Fundação artística

O Lumi é desenvolvido por um artista independente com experiência em pixel art, desenho e pintura tradicionais, desenvolvimento de jogos, arte técnica, ilustração e animação 3D. Este percurso molda a sua abordagem à cor, ao traço, às camadas, ao desempenho, à recuperação de dados, ao scripting e à experiência de utilizador.

## Filosofia

O Lumi combina um sistema de cor baseado em pigmentos com um fluxo de trabalho responsivo, não destrutivo e baseado em camadas. O seu sistema de cor é um afastamento deliberado dos sliders HSV convencionais e dos selectores RGB arbitrários.

- **Cor centrada no pigmento**: perfis de pigmento reais (códigos Colour Index) são misturados espectralmente para que as paletes se comportem mais como tinta verdadeira.
- **Fluxo de trabalho orientado por paletes**: paletes guardadas e permutáveis organizam pigmentos, misturas, bandas de valor e gradientes, mantendo coerentes as decisões de cor numa pintura ou projecto.
- **Ferramentas tácteis e focadas**: os pincéis integram a pressão, a inclinação e a velocidade da caneta para um controlo directo e nuanceado. A textura de superfície pode dar a um traço o grão e o dente de uma superfície, incluindo nos traços caligráficos, e uma viragem pode deslocar esse grão. Os pincéis de cerdas podem ser moldados de perto e animados ao longo do traço.
- **Tela responsiva**: o OpenGL pode desenhar a pilha de camadas e pintar traços de pincel para predefinições de ferramenta individuais. Os aerógrafos grandes e os carimbos de textura grandes acompanham a mão, e mover-se pela tela mantém-se fluido.
- **Fiabilidade não destrutiva**: camadas e filtros editáveis escalam para projectos complexos mantendo-se previsíveis. Gravação automática, gravação rápida, gravações incrementais e recuperação protegem longas sessões de pintura e projectos grandes.
- **Espaços de trabalho activos**: perfis nomeados preservam docks, ferramentas, presets, paletes e associações de dispositivos, alternando-os atomicamente em runtime.
- **Scripting Scheme**: o Lumi prolonga a tradição Script-Fu com uma linguagem de plug-ins baseada em Scheme e funções utilitárias adicionais para construir plug-ins e automatizar fluxos de trabalho.

Os [filtros](/hub/features/filters/), incluindo desfoque de lente bokeh moldado, tilt shift, graduação tonal, nitidez e redução de ruído, podem permanecer editáveis juntamente com o trabalho de pincel.

## Limites

- **Focado, não exaustivo**: o Lumi não se destina ao web design, à editoração electrónica nem a todas as especialidades que um editor amplo como o GIMP tenta cobrir.
- **Apenas Linux**: o Lumi é desenvolvido no Debian com Cinnamon (X11). O GNOME em Wayland foi testado quanto ao aspecto geral da aplicação e à renderização OpenGL. Windows e macOS não são suportados.

## Agradecimentos

O Lumi assenta no GNU Image Manipulation Program (GIMP). O Lumi reconhece e agradece profundamente os muitos anos de trabalho de programadores, artistas e colaboradores.

![Lumi logo placeholder](/images/lumi.png)
