---
title: "OpenGL"
type: docs
url: "hub/features/opengl"
translation_provenance: ai-reviewed
translation_source_sha256: af2a57a988eced14801b7c4673253fe79e9a6b6ab4fcf53a075b62d4a9035351
translation_lock: true
---

O Lumi pode desenhar a pilha de camadas com OpenGL e pintar traços de pincel do mesmo jeito. A navegação na tela permanece fluida. Aerógrafos grandes e carimbos de textura grandes ficam muito mais rápidos. Se uma determinada ferramenta pinta assim, isso fica guardado no preset dessa ferramenta.

## A pilha de camadas

A janela da imagem mostra a pilha de camadas composta. Com OpenGL, essa imagem fica retida em blocos no processador gráfico, já preparada para a exibição. A panorâmica, o zoom, a rotação e o espelhamento mudam a vista desses blocos. Numa tela grande os gestos permanecem fluidos, porque a navegação é uma mudança de vista sobre uma imagem que já está lá.

As edições atualizam os blocos que elas tocam. As guias, o cursor do pincel e o contorno da seleção são desenhados sobre essa imagem.

Enquanto a caneta está abaixada, um traço que usa OpenGL pode aparecer no lugar, entre as camadas ao redor, inclusive camadas acima daquela que está sendo pintada. A marca é escrita na camada quando a caneta levanta.

## Traços, por preset de ferramenta

Pintar com OpenGL é uma escolha no preset da ferramenta, junto com o pincel e a dinâmica. Um preset pode pintar no processador gráfico. Outro pode manter o caminho de traço comum. Voltar a uma ferramenta traz essa escolha de volta.

O ganho é maior onde os carimbos são grandes. No caminho comum, um aerógrafo largo fica para trás da caneta conforme a marca cresce: as pinceladas entram na fila, e o traço abre falhas, inclusive enquanto a caneta está abaixada e o pincel continua emitindo. Com OpenGL esse aerógrafo continua colocando marcas no ritmo da mão.

Um carimbo de textura grande mostra a mesma mudança. O caminho comum demora mais em cada pincelada conforme o carimbo cresce, até o traço travar. O traço OpenGL acompanha o gesto. Carimbos que carregam um grão de superfície também podem ser desenhados com OpenGL.

Marcas de cerdas também podem ser desenhadas assim. Um carimbo de cerdas com ponta redonda pode ser pintado com OpenGL enquanto as cerdas mudam ao longo do traço. Um traço caligráfico pode ser desenhado como uma fita contínua, e a textura de superfície pode mascarar essa fita. A [textura de superfície](/hub/features/paintbrush/#textura-de-superfície) e as [cerdas ao longo do traço](/hub/features/paintbrush/#cerdas-ao-longo-do-traço) pertencem ao próprio pincel.

Alguns traços ficam no caminho comum. A mistura espectral, um filtro na camada pintada e alguns modos de mistura estão entre eles. Essas marcas continuam sendo pintadas.

## Quando a tela comum permanece

O OpenGL é usado quando o hardware gráfico consegue executá-lo e a imagem cabe na memória reservada para a tela. Caso contrário, o Lumi desenha a janela como antes. O AppImage publicado traz as próprias bibliotecas gráficas. Num driver que essas bibliotecas não conseguem usar, inclusive o driver proprietário da NVIDIA, a tela permanece nesse caminho comum.

## Cinnamon no X11 e GNOME no Wayland

O Lumi é desenvolvido no Debian com Cinnamon e X11. Esse continua sendo o ambiente de desenvolvimento. O GNOME no Wayland foi testado quanto à aparência geral do aplicativo e a esta renderização OpenGL. A navegação na tela também fica fluida nesse ambiente.
