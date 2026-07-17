---
title: "Lumi-o"
type: docs
weight: 1
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 1bf50df22bdb2af7931727f82bc3c90eee5242be66847535aec8e41c47087e53
url: "hub/about/lumi-o"
---

Lumi é um aplicativo rápido e eficiente, exclusivo para Linux, para criação de imagens raster, desenvolvido em código aberto. Decisões de design, documentos de arquitetura e histórico de desenvolvimento são públicos, para que os usuários possam acompanhar como o software evolui.

Lumi prioriza pintura e ilustração digital, mantendo capacidade para edição estruturada de imagens e correção fotográfica. Ajustes corretivos, efeitos de estilo de lente, gradação de cor baseada em tons e camadas de filtro não destrutivas suportam uma ampla variedade de fluxos de edição.

Lumi foi construído em torno da ideia de que software de pintura digital deve se comportar como uma ferramenta de estúdio confiável: previsível, transparente e focada no ato de criar imagens.

## Objetivo

Lumi suporta um método estruturado e não destrutivo de criação de imagens, seja a fonte pintada, desenhada ou fotográfica. É uma alternativa focada e opinativa tanto a editores de imagem genéricos quanto a software dedicado à pintura: gratuito e de código aberto, sem assinaturas, aprisionamento, dependência de nuvem ou geração de imagens por IA.

Confiabilidade e acesso de longo prazo são recursos centrais. O formato de arquivo aberto baseado em diretórios permanece legível sem software proprietário, com importação e exportação de XCF e PSD.

## Base artística

Lumi é desenvolvido por um artista independente com experiência em pixel art, desenho e pintura tradicional, desenvolvimento de jogos, arte técnica, ilustração e animação 3D. Esse histórico molda a abordagem a cor, traço, camadas, desempenho, recuperação de dados, scripts e experiência do usuário.

## Filosofia

Lumi combina um sistema de cores baseado em pigmentos com um fluxo de trabalho responsivo, não destrutivo e em camadas. Seu sistema de cores é um afastamento deliberado dos controles deslizantes HSV convencionais e seletores RGB arbitrários.

- **Cor centrada em pigmentos**: perfis de pigmentos do mundo real (códigos Colour Index) são misturados espectralmente, para que paletas se comportem mais como tinta real.
- **Fluxo de trabalho orientado por paleta**: paletas salvas e alternáveis organizam pigmentos, misturas, faixas de valor e gradientes, mantendo decisões de cor coerentes em uma pintura ou projeto.
- **Ferramentas táteis e focadas**: pincéis integram pressão, inclinação e velocidade da caneta para controle direto e refinado; controles apoiam decisões deliberadas sem complexidade desnecessária.
- **Confiabilidade não destrutiva**: camadas e filtros editáveis escalam para projetos complexos e permanecem previsíveis. Salvamento automático, salvamento rápido, salvamentos incrementais e recuperação protegem longas sessões de pintura e projetos grandes.
- **Espaços de trabalho dinâmicos**: perfis nomeados preservam docks, ferramentas, predefinições, paletas e vínculos de dispositivos e os alternam atomicamente em tempo de execução.
- **Scheme scripting**: Lumi estende a tradição Script-Fu com uma linguagem de plug-in baseada em Scheme e funções utilitárias adicionais para criar plug-ins e automatizar fluxos de trabalho.

[Filtros](/hub/features/filters/) — incluindo desfoque de lente bokeh com forma, tilt-shift, gradação tonal, nitidez e redução de ruído — podem permanecer editáveis junto com o trabalho de pincel.

## Limites

- **Focado, não exaustivo**: Lumi não visa web design, editoração eletrônica ou cada nicho que um editor amplo como o GIMP tenta cobrir.
- **Somente Linux**: Lumi é otimizado especificamente para Linux e não oferece suporte a Windows ou macOS.

## Agradecimentos

Lumi é construído sobre a base do GNU Image Manipulation Program (GIMP). Lumi reconhece e agradece profundamente pelos muitos anos de trabalho de desenvolvedores, artistas e colaboradores.

![Lumi logo placeholder](/images/lumi.png)
