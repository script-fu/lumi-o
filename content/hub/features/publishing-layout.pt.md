---
title: "Layout de publicação"
type: docs
translation_provenance: ai-reviewed
translation_source_sha256: dc0367028ed8f6b4e1508c309384967daa43a4148f8d70f00880173a0a1fca7d
url: "hub/features/publishing-layout"
translation_lock: true
---
A ilustração para impressão e publicação exige frequentemente mais do que um tamanho de tela. As páginas têm bordas de corte, as páginas em spread têm medianizes centrais e o conteúdo importante pode precisar de ficar fora de áreas que serão cortadas ou encadernadas na medianiz. As ferramentas de layout de publicação do Lumi mantêm estas preocupações visíveis durante a pintura, sem as achatar na obra.

Os limites de layout são guardados por imagem, com o projecto, e podem ser desactivados quando não são necessários. O objectivo é dar a livros, banda desenhada e fluxos de impressão uma noção clara da estrutura de página, enquanto a imagem em camadas permanece totalmente editável por baixo.

## Sangria e corte

A sangria define até onde a obra se estende para além da borda final da página. O Lumi mostra a área de corte como o limite activo da página dentro da tela, com a margem de sangria como sobreposição sombreada à volta. Isto facilita pintar fundos e detalhes de borda que devem sobreviver ao corte, sem adivinhar onde a página acabada terminará.

As medidas podem ser definidas nas unidades adequadas ao trabalho, para pensar a sangria em polegadas, milímetros ou outra unidade de impressão familiar, e não apenas em pixels.

## Medianiz e spreads

Para spreads de duas páginas, a medianiz marca a zona protegida em torno da costura central onde o conteúdo importante deve ser evitado. Quando activada, o Lumi mostra faixas de medianiz no spread, para manter rostos, texto e pontos focais fora da área de encadernação, enquanto o spread completo permanece uma tela contínua.

Isto é especialmente útil para banda desenhada, livros ilustrados e qualquer obra impressa como páginas opostas, e não como folhas isoladas.

## Guias de composição

Guias de borda opcionais marcam a área de página cortada com marcas subtis de composição. Os guias podem seguir divisões por página ou leitura de spread completo, e usar terços, secções áureas ou quintos, consoante a forma como o layout deve ser avaliado.

Os guias servem como referência discreta durante layout e acabamento. Ajudam a posicionar a leitura face à página que será realmente impressa, e não apenas face à tela digital completa.

## Ver layout na tela

As sobreposições de layout são controladas no menu Ver. As áreas de sangria, medianiz e guias podem ser mostradas individualmente ou em conjunto, para o artista se concentrar na parte da estrutura de publicação que importa naquele momento.

Imagem > Activar layout activa ou desactiva os limites de layout para a imagem actual. Quando o layout está desactivado, as sobreposições ficam ocultas e os alternadores de vista recuam, mas as definições de limite permanecem guardadas no ficheiro para uso posterior.

## Guardado com o projecto

As definições de layout acompanham o projecto `.lum`. Abrir a imagem mais tarde restaura sangria, medianiz, aparência das sobreposições, escolhas de guias e se o layout está activo para esse ficheiro. Isto mantém a configuração orientada para publicação como parte do estado de trabalho da obra, e não como preferência de visualização temporária.

Para artistas que alternam entre esboço, pintura e preparação de impressão, o fluxo permanece num só sítio: a mesma imagem em camadas, com estrutura de publicação disponível sempre que a página precisar.
