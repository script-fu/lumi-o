---
title: "Formato de arquivo (.lum)"
type: docs
---
O formato de arquivo nativo do Lumi foi desenvolvido para projetos de pintura em camadas que precisam permanecer confiáveis, inspecionáveis ​​e recuperáveis ​​ao longo do tempo. Ele foi projetado em torno da realidade do trabalho de ilustração: muitas camadas, telas grandes, informações de cores incorporadas, máscaras, efeitos e dados de recuperação.

Em vez de tratar um projeto como um único blob opaco, o formato mantém a estrutura da arte visível para o aplicativo. Isso permite que o Lumi salve, carregue e recupere imagens grandes de maneira mais inteligente, preservando a organização da qual os artistas dependem.

## Abra a estrutura do projeto

Um projeto Lumi mantém as partes da arte separadas: estrutura da imagem, conteúdo da camada, máscaras, dados de cores, metadados e informações de recuperação, cada um tem uma função clara. Isso torna o formato mais fácil de raciocinar e mais adequado para acesso de longo prazo do que um contêiner fechado e monolítico.

O objetivo não é apenas armazenar pixels, mas armazenar o estado de funcionamento de uma ilustração. As camadas permanecem camadas, as máscaras permanecem máscaras e o arquivo continua a refletir a forma como o trabalho artístico foi construído.

## Projetado para pinturas grandes

Imagens grandes em camadas podem ficar pesadas rapidamente. O formato do Lumi oferece suporte a fluxos de trabalho em que nem todos os dados de imagem precisam ser colocados na memória de uma só vez. Os projetos podem permanecer responsivos carregando as partes da imagem que são realmente necessárias para visualização, edição, composição ou exportação.

Essa abordagem ajuda arquivos complexos a parecerem gerenciáveis, especialmente quando uma obra de arte contém muitas camadas ocultas, arquivadas, experimentais ou agrupadas.

## Economizando sem interromper o fluxo

O formato de arquivo suporta salvamento normal de projetos e instantâneos leves de estilo de recuperação. Isso dá aos artistas uma maneira de proteger o trabalho com frequência, sem transformar cada ponto de verificação em uma duplicata completa da imagem inteira.

Como as informações de recuperação pertencem à estrutura do projeto, o Lumi pode manter o histórico útil próximo à obra de arte, ao mesmo tempo que permite que os salvamentos automáticos de segurança fiquem separados do arquivo de trabalho.

## Intercâmbio e exportação

O formato nativo é destinado ao trabalho contínuo do Lumi, enquanto os formatos de exportação são usados para compartilhar resultados nivelados ou focados em compatibilidade. O suporte à importação ajuda a trazer a arte existente para o ambiente em camadas do Lumi, e o suporte à exportação permite que as peças finalizadas deixem o formato do projeto quando estiverem prontas para publicação, entrega ou processamento posterior.

A distinção mantém o arquivo de trabalho rico e editável, ao mesmo tempo que permite que as imagens finais sejam produzidas em formatos externos comuns.

## Confiabilidade a longo prazo

Resumindo, o formato `.lum` é um recipiente prático para trabalhos de pintura sérios: aberto o suficiente para inspecionar, estruturado o suficiente para recuperar e flexível o suficiente para lidar economicamente com imagens complexas em camadas.