---
title: "Camadas"
type: docs
translation_provenance: ai-reviewed
translation_source_sha256: ff151a84a2bca18cbd1389f1e7048fda7231ee8c1adf0bc16b1d7513c224f3ce
url: "hub/features/layers"
translation_lock: true
---
O sistema de camadas do Lumi dá estrutura à ilustração. Permite separar esboço, cor, sombreado, textura, máscaras, ajustes, experiências e detalhe final sem gravar cada decisão directamente numa imagem plana.

As camadas não são apenas uma pilha de pixels. Transportam visibilidade, mistura, máscaras, bloqueios, efeitos, agrupamento e comportamento de composição, o que as torna a base de fluxos de pintura flexíveis e não destrutivos.

![layers](/images/screens/layers.jpg)

## Pintura estruturada

Uma imagem em camadas constrói-se por fases. Marcas brutas podem ficar sob linhas limpas, a cor pode ser assentada separadamente da iluminação, a textura pode ser isolada e variantes podem permanecer disponíveis sem perturbar a composição principal.

Os grupos tornam essa estrutura legível. Elementos relacionados de uma ilustração podem mover-se em conjunto, misturar-se em conjunto ou ser tratados como parte comum da obra, enquanto as camadas individuais permanecem editáveis.

## Mistura e máscaras

A mistura de camadas controla como uma parte da ilustração interage com o que está por baixo. Torna possível sombrear, clarear, tingir, texturizar ou corrigir a cor sem repintar as formas subjacentes.

As máscaras acrescentam outro nível de controlo. Permitem pintar, suavizar, ocultar, restaurar ou modelar a visibilidade independentemente do conteúdo cromático da camada. As decisões de contorno e as transições tonais mantêm-se flexíveis ao longo da vida da obra.

## Selecção e navegação

Ilustrações complexas podem conter muitas peças pequenas. O Lumi suporta navegação directa orientada por camadas, para voltar da tela à pilha de camadas sem perder o fio da pintura.

O objectivo é que o trabalho em camadas pareça espacial e não administrativo: se uma marca está visível na tela, o sistema de camadas deve ajudar o artista a encontrá-la rapidamente.

## Protecção e intenção

As camadas podem ser protegidas de diferentes formas para que o trabalho concluído, as máscaras, as posições, a transparência ou as escolhas de cor não sejam alteradas por acidente. Estas salvaguardas são úteis quando a imagem se torna densa e algumas partes precisam de permanecer estáveis enquanto outras continuam a evoluir.

Esta protecção favorece fluxos de trabalho deliberados: esboçar livremente onde a mudança é bem-vinda, bloquear as áreas resolvidas e continuar a desenvolver a imagem sem receio de danos acidentais.

## Cor da tinta bloqueada

Uma camada preenchida com uma única cor uniforme pode ser bloqueada a uma cor da paleta. Seleccionar a camada selecciona então a cor associada na paleta, e alterar essa cor da paleta actualiza imediatamente a camada.

Pintar numa camada com cor da tinta bloqueada usa sempre a cor associada da paleta. Isto cria uma ligação viva entre a paleta e a camada, para que as cores possam ser ajustadas dinamicamente enquanto a paleta ainda está a ser concebida.

## Efeitos não destrutivos

Filtros e efeitos podem fazer parte do estado editável de uma camada, em vez de se tornarem imediatamente pixels permanentes. Isto mantém as alterações visuais ajustáveis e permite que uma pilha de efeitos permaneça parte da composição de trabalho.

Para os artistas, isto significa que a experimentação pode permanecer reversível. Um aspecto pode ser testado, ocultado, reordenado, refinado ou, por fim, confirmado quando passa a fazer parte da imagem final.

## Desempenho para ficheiros profundos

Ilustrações em camadas podem tornar-se complexas, especialmente quando grupos, máscaras e efeitos interagem. O sistema de camadas do Lumi foi concebido para manter as acções comuns de pintura responsivas, evitando recomposições desnecessárias sempre que possível.

O resultado é um fluxo de camadas que visa controlo e velocidade: suficientemente detalhado para trabalho cuidadoso e não destrutivo, mas ainda prático para a pintura quotidiana.
