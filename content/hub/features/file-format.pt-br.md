---
title: "Formato de arquivo (.lum)"
type: docs
url: "hub/features/file-format"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: f26bd5ecb0cb647cd3180b1ab39402ee6943085b7ad899518a406ee6ae98c4c9
---

O formato nativo do Lumi foi desenvolvido para projetos de pintura em camadas que precisam permanecer confiáveis, inspecionáveis e recuperáveis ao longo do tempo. Ele reflete a realidade do trabalho de ilustração: muitas camadas, telas grandes, informações de cor incorporadas, máscaras, efeitos e dados de recuperação.

Em vez de tratar um projeto como um único bloco opaco, o formato mantém a estrutura da arte visível para o aplicativo. Isso permite que o Lumi salve, carregue e recupere imagens grandes de forma mais inteligente, preservando a organização da qual os artistas dependem.

## Estrutura de projeto aberta

Um projeto Lumi mantém as partes da arte separadas: estrutura da imagem, conteúdo das camadas, máscaras, dados de cor, metadados e informações de recuperação — cada uma com um papel claro. Isso torna o formato mais fácil de entender e mais adequado ao acesso de longo prazo do que um contêiner fechado e monolítico.

O objetivo não é apenas armazenar pixels, mas guardar o estado de trabalho de uma ilustração. As camadas permanecem camadas, as máscaras permanecem máscaras, e o arquivo continua refletindo a forma como a arte foi construída.

## Projetado para pinturas grandes

Imagens grandes em camadas ficam pesadas rapidamente. O formato do Lumi suporta fluxos de trabalho em que nem todos os dados de imagem precisam ser carregados na memória de uma só vez. Os projetos permanecem responsivos ao carregar apenas as partes da imagem necessárias para visualização, edição, composição ou exportação.

Essa abordagem ajuda arquivos complexos a parecerem gerenciáveis, especialmente quando uma obra contém muitas camadas ocultas, arquivadas, experimentais ou agrupadas.

## Salvar sem interromper o fluxo

O formato de arquivo suporta tanto o salvamento normal do projeto quanto instantâneos leves de recuperação. Isso dá aos artistas uma forma de proteger o trabalho com frequência, sem transformar cada ponto de verificação em uma duplicata completa da imagem inteira.

Como as informações de recuperação pertencem à estrutura do projeto, o Lumi pode manter um histórico útil próximo à arte, permitindo que salvamentos automáticos de segurança fiquem separados do arquivo de trabalho.

## Intercâmbio e exportação

O formato nativo destina-se ao trabalho contínuo no Lumi; os formatos de exportação servem para compartilhar resultados achatados ou focados em compatibilidade. A importação ajuda a trazer arte existente para o ambiente em camadas do Lumi, e a exportação permite que peças finalizadas deixem o formato do projeto quando estiverem prontas para publicação, entrega ou processamento posterior.

Assim, o arquivo de trabalho permanece rico e editável, enquanto as imagens finais podem ser produzidas em formatos externos comuns.

## Confiabilidade a longo prazo

Em resumo, o formato `.lum` é um contêiner prático para trabalho de pintura sério: aberto o suficiente para inspecionar, estruturado o suficiente para recuperar e flexível o suficiente para lidar economicamente com imagens complexas em camadas.
