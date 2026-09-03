---
title: "Camadas"
type: docs
url: "hub/features/layers"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: ff151a84a2bca18cbd1389f1e7048fda7231ee8c1adf0bc16b1d7513c224f3ce
---

O sistema de camadas do Lumi dá estrutura a uma ilustração. Artistas podem separar esboço, cor, sombreamento, textura, máscaras, ajustes, experimentos e detalhes finais sem registrar cada decisão diretamente em uma imagem plana.

As camadas não são apenas uma pilha de pixels. Elas carregam visibilidade, mesclagem, máscaras, bloqueios, efeitos, agrupamento e comportamento de composição — a base para fluxos de pintura flexíveis e não destrutivos.

![layers](/images/screens/layers.jpg)

## Pintura estruturada

Uma imagem em camadas pode ser construída em etapas. Marcas ásperas podem ficar abaixo de linhas limpas, a cor pode ser bloqueada separadamente da iluminação, a textura isolada, e ideias alternativas permanecer disponíveis sem perturbar a composição principal.

Os grupos tornam essa estrutura legível. Partes relacionadas de uma ilustração podem se mover juntas, mesclar-se ou ser tratadas como uma parte compartilhada da arte, enquanto suas camadas individuais permanecem editáveis.

## Mesclagem e máscaras

A mesclagem de camadas controla como uma parte da arte interage com o que está abaixo. Isso permite sombrear, clarear, tingir, texturizar ou corrigir cores sem repintar as formas subjacentes.

As máscaras acrescentam outro nível de controle. A visibilidade pode ser pintada, suavizada, ocultada, restaurada ou modelada independentemente do conteúdo de cor da camada. Decisões de borda e transições tonais permanecem flexíveis durante todo o processo.

## Seleção e navegação

Pinturas complexas podem conter muitos pedaços pequenos. O Lumi oferece navegação direta orientada por camadas, para que artistas voltem da tela à pilha de camadas sem perder o fluxo da pintura.

O trabalho em camadas deve parecer espacial, não administrativo: se uma marca está visível na tela, o sistema de camadas ajuda o artista a retornar a ela rapidamente.

## Proteção e intenção

As camadas podem ser protegidas de diferentes formas para que trabalho finalizado, máscaras, posições, transparência ou decisões de cor não sejam alterados acidentalmente. Isso ajuda quando a imagem fica densa e algumas partes precisam permanecer estáveis enquanto outras evoluem.

Essa proteção apoia fluxos deliberados: esboce livremente onde a mudança é bem-vinda, trave áreas resolvidas e continue desenvolvendo a imagem sem medo de danos acidentais.

## Cor da tinta bloqueada

Uma camada preenchida com uma única cor uniforme pode ser bloqueada a uma cor da paleta. Selecionar a camada seleciona então a cor vinculada na paleta, e alterar essa cor da paleta atualiza a camada imediatamente.

Pintar em uma camada com cor da tinta bloqueada usa sempre a cor vinculada da paleta. Isso cria uma conexão viva entre a paleta e a camada, para que as cores possam ser ajustadas dinamicamente enquanto a paleta ainda está sendo projetada.

## Efeitos não destrutivos

Filtros e efeitos podem fazer parte do estado editável de uma camada em vez de se tornarem pixels permanentes imediatamente. As alterações visuais permanecem ajustáveis, e uma pilha de efeitos pode continuar como parte da composição de trabalho.

Para os artistas, isso significa que a experimentação pode permanecer reversível. Um visual pode ser testado, ocultado, reordenado, refinado ou eventualmente confirmado quando passa a fazer parte da imagem final.

## Desempenho para arquivos profundos

Ilustrações em camadas tornam-se complexas, especialmente quando grupos, máscaras e efeitos interagem. O sistema de camadas do Lumi mantém ações comuns de pintura responsivas, evitando recomposições desnecessárias sempre que possível.

O resultado é um fluxo que equilibra controle e velocidade: detalhado o suficiente para trabalho cuidadoso e não destrutivo, mas ainda prático para a pintura do dia a dia.
