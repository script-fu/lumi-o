---
title: "Formato de arquivo (.lum)"
type: docs
url: "hub/features/file-format"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: c5e414a0d2870f1b111751c8c462e7ac5a4530103d70cb9be52a9fbd11417028
---

O formato nativo `.lum` do Lumi é um diretório de projeto, não um arquivo único e lacrado. Ele foi feito para ilustração em camadas: árvores de camadas profundas, telas grandes, máscaras, efeitos não destrutivos e pontos de verificação que não precisam duplicar a pintura inteira.

O papel do formato é preservar essa estrutura de trabalho — para que um projeto possa ser reaberto com fidelidade, inspecionado quando algo der errado e recuperado a partir de um ponto de verificação recente, sem tratar a arte como um único bloco opaco.

## Partes separadas, de propósito

Um projeto `.lum` é uma pasta. A árvore de camadas e as propriedades da imagem ficam em XML legível. Cada camada e cada máscara mantém o próprio buffer de pixels, batizado com o nome da arte, não com um ID interno. Os caminhos vetoriais são gravados como SVG comum. As configurações pesadas de filtros ficam em arquivos próprios, ao lado da imagem. Os perfis ICC são armazenados uma vez na raiz do projeto, para que os instantâneos de recuperação os referenciem em vez de copiá-los.

É essa separação que torna o restante do formato possível. Camadas inalteradas podem ser deixadas em paz no disco. Um buffer danificado falha sozinho, em vez de levar o arquivo inteiro junto. Pixels de camada ausentes viram camadas vazias que ainda têm nome, posição e ajustes de mesclagem; uma composição de grupo ausente é reconstruída a partir dos filhos. O projeto continua sendo um mapa de como a pintura foi construída.

As paletas de pigmento permanecem nas ferramentas de cor do Lumi. Um projeto pode lembrar qual paleta estava associada à imagem, mas a biblioteca de paletas em si fica fora do `.lum`.

## Estado de edição, não uma imagem achatada

O arquivo guarda a pintura em andamento. Camadas continuam camadas, grupos de camadas continuam grupos, e máscaras continuam máscaras — inclusive deslocamentos, bloqueios, comportamento de mesclagem e pilhas de filtros. Filtros não destrutivos são salvos como operações e parâmetros, não como pixels já aplicados. Uma camada que é uma única cor chapada nem precisa de arquivo de pixels.

Grupos recolhidos também guardam uma visualização composta de si mesmos. Essa prévia composta em cache é o que aparece na tela quando o grupo está fechado, então os filhos não precisam ser reconstruídos só para se olhar o quadro. Modos de inspeção só de exibição ficam de fora desse cache: mostrar uma máscara ou o alfa para edição é restaurado como metadados, não incorporado no grupo salvo.

## Arquivos grandes podem permanecer parcialmente no disco

Abrir um `.lum` não exige carregar todos os pixels. O conteúdo de grupos recolhidos pode ficar no disco enquanto a composição salva do grupo é exibida na hora. Só ao expandir um grupo essas camadas, máscaras e grupos aninhados entram na memória. Grupos que permanecem fechados continuam leves.

O arquivo também registra quais grupos estavam de fato em uso. Grupos no caminho da seleção ativa podem reabrir já expandidos; os demais grupos são armazenados recolhidos, mesmo que estivessem abertos na sessão anterior. Assim, um arquivo profundo não carrega na memória todos os ramos ociosos no instante em que é aberto.

Agrupar é, portanto, uma escolha de desempenho tanto quanto de organização. Fundos grandes, experimentos arquivados e variantes sem uso podem ficar em grupos fechados sem ocupar a mesma memória das camadas em que se pinta. O salvamento segue a mesma regra: buffers ainda ocultos são copiados ou ignorados como arquivos, sem serem trazidos de volta à memória só para serem gravados de novo.

## Pontos de verificação que salvam só o que mudou

Arquivo → Salvar atualiza o projeto de trabalho. Salvamentos incrementais e o salvamento automático escrevem numa árvore de recuperação, e gravam apenas dados modificados — buffers de camada alterados, não uma segunda cópia da imagem inteira. Cada ponto de verificação ainda carrega uma descrição completa da árvore de camadas, de modo que qualquer ponto desse histórico pode ser aberto preenchendo os pixels inalterados a partir de pontos de verificação mais antigos e, se preciso, do próprio arquivo de trabalho.

O salvamento automático usa o mesmo padrão em um cache separado, para que a proteção automática não precise reescrever o arquivo no disco. Se um projeto for aberto quando existirem pontos de verificação mais novos que o último salvamento completo, o Lumi pode oferecê-los em vez de descartar em silêncio o trabalho mais recente. Imagens recuperadas abrem com um nome distinto, para que um salvamento rápido não possa sobrescrever o original.

## Um formato para continuar a pintar

O `.lum` serve para seguir pintando no Lumi. Formatos achatados ou de compatibilidade servem para publicação, entrega e outros aplicativos. Como um projeto é um diretório com muitos arquivos, deve ser arquivado se precisar viajar.

O arquivo de trabalho permanece rico e editável. As exportações são o modo de uma imagem pronta ou compartilhada deixar essa estrutura.
