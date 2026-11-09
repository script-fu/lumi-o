---
title: "OpenGL"
type: docs
url: "hub/features/opengl"
translation_provenance: ai-reviewed
translation_source_sha256: af2a57a988eced14801b7c4673253fe79e9a6b6ab4fcf53a075b62d4a9035351
translation_lock: true
---

O Lumi pode desenhar a pilha de camadas com OpenGL e pintar traços de pincel do mesmo modo. A navegação na tela mantém-se fluida. Os aerógrafos grandes e os carimbos de textura grandes são muito mais rápidos. O facto de uma ferramenta pintar assim fica guardado na predefinição dessa ferramenta.

## A pilha de camadas

A janela da imagem mostra a pilha de camadas composta. Com OpenGL, essa imagem é retida em blocos no processador gráfico, já preparada para o ecrã. A panorâmica, a ampliação, a rotação e o espelhamento mudam a vista desses blocos. Numa tela grande os gestos mantêm-se fluidos, porque a navegação é uma mudança de vista sobre uma imagem que já lá está.

As edições actualizam os blocos em que tocam. As guias, o cursor do pincel e o contorno da selecção desenham-se por cima dessa imagem.

Enquanto a caneta está pousada, um traço que usa OpenGL pode mostrar-se no seu lugar, entre as camadas à volta, incluindo camadas acima daquela que está a ser pintada. A marca escreve-se na camada quando a caneta se levanta.

## Traços, por predefinição de ferramenta

Pintar com OpenGL é uma escolha da predefinição de ferramenta, a par do pincel e das dinâmicas. Uma predefinição pode pintar no processador gráfico. Outra pode manter o caminho de traço habitual. Regressar a uma ferramenta traz essa escolha de volta.

O ganho é maior onde os carimbos são grandes. No caminho habitual, um aerógrafo largo fica para trás do stylus à medida que a marca cresce: as pinceladas entram em fila, e o traço abre falhas, mesmo enquanto a caneta está pousada e o pincel continua a emitir. Com OpenGL esse aerógrafo continua a assentar marcas ao ritmo da mão.

Um carimbo de textura grande mostra a mesma mudança. O caminho habitual demora mais em cada pincelada à medida que o carimbo cresce, até o traço estacar. O traço OpenGL acompanha o gesto. Os carimbos que levam um grão de superfície também podem ser desenhados com OpenGL.

As marcas de cerdas também se podem desenhar desta forma. Um carimbo de cerdas de ponta redonda pode ser pintado com OpenGL enquanto as cerdas mudam ao longo do traço. Um traço caligráfico pode ser desenhado como uma fita contínua, e a textura de superfície pode mascarar essa fita. A [textura de superfície](/hub/features/paintbrush/#textura-de-superfície) e as [cerdas ao longo do traço](/hub/features/paintbrush/#cerdas-ao-longo-do-traço) pertencem ao próprio pincel.

Alguns traços ficam no caminho habitual. Entre eles estão a mistura espectral, um filtro na camada pintada e alguns modos de mistura. Essas marcas continuam a ser pintadas.

## Quando permanece a tela habitual

O OpenGL usa-se quando o equipamento gráfico o pode executar e a imagem cabe na memória reservada à tela. Caso contrário, o Lumi desenha a janela como antes. A AppImage publicada traz as suas próprias bibliotecas gráficas. Num controlador que essas bibliotecas não conseguem usar, incluindo o controlador proprietário da NVIDIA, a tela permanece nesse caminho habitual.

## Cinnamon em X11 e GNOME em Wayland

O Lumi é desenvolvido no Debian com Cinnamon e X11. Esse continua a ser o ambiente de desenvolvimento. O GNOME em Wayland foi testado quanto ao aspecto geral da aplicação e a esta renderização OpenGL. A navegação na tela também aí se mantém fluida.
