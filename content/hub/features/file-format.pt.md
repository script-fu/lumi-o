---
title: "Formato de ficheiro (.lum)"
type: docs
translation_provenance: ai-reviewed
translation_source_sha256: f26bd5ecb0cb647cd3180b1ab39402ee6943085b7ad899518a406ee6ae98c4c9
url: "hub/features/file-format"
translation_lock: true
---
O formato nativo do Lumi foi concebido para projectos de pintura em camadas que precisam de permanecer fiáveis, inspecionáveis e recuperáveis ao longo do tempo. Assenta na realidade do trabalho de ilustração: muitas camadas, telas grandes, informação de cor incorporada, máscaras, efeitos e dados de recuperação.

Em vez de tratar um projecto como um único bloco opaco, o formato mantém a estrutura da obra visível para a aplicação. Isto permite ao Lumi gravar, carregar e recuperar imagens grandes de forma mais inteligente, preservando a organização de que os artistas dependem.

## Estrutura de projecto aberta

Um projecto Lumi mantém as partes da obra separadas: estrutura da imagem, conteúdo das camadas, máscaras, dados de cor, metadados e informação de recuperação, cada um com um papel claro. Isto torna o formato mais fácil de compreender e mais adequado ao acesso a longo prazo do que um contentor fechado e monolítico.

O objectivo não é apenas armazenar pixels, mas o estado de trabalho de uma ilustração. As camadas permanecem camadas, as máscaras permanecem máscaras e o ficheiro continua a reflectir a forma como a obra foi construída.

## Concebido para pinturas grandes

Imagens grandes em camadas podem tornar-se pesadas rapidamente. O formato do Lumi suporta fluxos em que nem todos os dados de imagem precisam de ser carregados na memória de uma só vez. Os projectos podem permanecer responsivos carregando as partes da imagem realmente necessárias para visualização, edição, composição ou exportação.

Esta abordagem ajuda ficheiros complexos a parecerem geríveis, especialmente quando uma obra contém muitas camadas ocultas, arquivadas, experimentais ou agrupadas.

## Gravar sem interromper o fluxo

O formato suporta gravação normal de projectos e instantâneos leves de recuperação. Isto dá aos artistas uma forma de proteger o trabalho com frequência, sem transformar cada ponto de controlo numa duplicata completa de toda a imagem.

Como a informação de recuperação pertence à estrutura do projecto, o Lumi pode manter historial útil perto da obra, permitindo que gravações automáticas de segurança vivam separadas do ficheiro de trabalho.

## Intercâmbio e exportação

O formato nativo destina-se ao trabalho contínuo no Lumi, enquanto os formatos de exportação servem para partilhar resultados achatados ou orientados para compatibilidade. O suporte de importação ajuda a trazer obras existentes para o ambiente em camadas do Lumi, e o suporte de exportação permite que peças concluídas saiam do formato de projecto quando estão prontas para publicação, entrega ou processamento posterior.

A distinção mantém o ficheiro de trabalho rico e editável, permitindo produzir imagens finais em formatos externos comuns.

## Fiabilidade a longo prazo

Em suma, o formato `.lum` é um contentor prático para trabalho de pintura sério: suficientemente aberto para inspecionar, suficientemente estruturado para recuperar e suficientemente flexível para lidar economicamente com imagens complexas em camadas.
