---
title: "Formato de ficheiro (.lum)"
type: docs
translation_provenance: ai-reviewed
translation_source_sha256: c5e414a0d2870f1b111751c8c462e7ac5a4530103d70cb9be52a9fbd11417028
url: "hub/features/file-format"
translation_lock: true
---

O formato nativo `.lum` do Lumi é um directório de projecto, não um ficheiro único e selado. Foi concebido para ilustração em camadas: árvores de camadas profundas, telas grandes, máscaras, efeitos não destrutivos e pontos de controlo que não têm de duplicar a pintura inteira.

O objectivo do formato é manter essa estrutura de trabalho intacta — para que um projecto se possa reabrir com fidelidade, ser inspeccionado quando algo corre mal e ser recuperado a partir de um ponto de controlo recente, sem tratar a obra como um único bloco opaco.

## Peças à parte, de propósito

Um projecto `.lum` é uma pasta. A árvore de camadas e as propriedades da imagem ficam em XML legível. Cada camada e cada máscara guarda o seu próprio buffer de pixels, com o nome da obra e não de um identificador interno. Os caminhos vectoriais armazenam-se como SVG comum. Os parâmetros pesados de filtros ficam em ficheiros próprios, ao lado da imagem. Os perfis ICC guardam-se uma só vez na raiz do projecto, para que os instantâneos de recuperação os referenciem em vez de os copiar.

É essa divisão que torna possível o resto do formato. As camadas inalteradas podem ficar intactas no disco. Um buffer danificado falha sozinho, em vez de levar o ficheiro inteiro consigo. Pixels de camada em falta tornam-se camadas vazias que ainda têm nome, posição e definições de mistura; uma composição de grupo em falta reconstrói-se a partir dos filhos. O projecto continua a ser um mapa de como a pintura foi construída.

As paletas de pigmento ficam nas ferramentas de cor do Lumi. Um projecto pode lembrar-se de qual paleta estava associada à imagem, mas a biblioteca de paletas em si fica fora do `.lum`.

## Estado editável, não um achatamento

O ficheiro armazena a pintura de trabalho. As camadas continuam camadas, os grupos de camadas continuam grupos e as máscaras continuam máscaras — incluindo deslocamentos, bloqueios, comportamento de mistura e pilhas de filtros. Os filtros não destrutivos gravam-se como operações e parâmetros, não como pixels já fundidos. Uma camada que é uma única cor plana não precisa sequer de um ficheiro de pixels.

Os grupos fechados guardam também uma vista composta de si próprios. Essa pré-visualização composta em cache é o que aparece na tela quando o grupo está fechado, pelo que os filhos não têm de ser reconstruídos só para se olhar para a imagem. Os modos de inspecção só para ecrã ficam de fora dessa cache: mostrar uma máscara ou o alfa para edição restaura-se como metadados, não fica incorporado no grupo gravado.

## Ficheiros grandes podem ficar em parte no disco

Abrir um `.lum` não obriga a carregar todos os pixels. O conteúdo dentro de grupos fechados pode permanecer no disco, enquanto a composição gravada do grupo aparece de imediato. É ao expandir um grupo que essas camadas, máscaras e grupos aninhados entram na memória. Os grupos que se mantêm fechados continuam leves.

O ficheiro também regista quais os grupos que estavam realmente em uso. Os grupos no caminho da selecção activa podem voltar a abrir-se já expandidos; os outros grupos ficam armazenados como fechados, mesmo que estivessem abertos na última sessão. Assim, um ficheiro profundo não carrega para a memória todos os ramos por usar no instante em que se abre.

Agrupar é, portanto, uma escolha de desempenho tanto quanto de organização. Fundos grandes, experiências arquivadas e variantes por usar podem ficar em grupos fechados sem ocupar a mesma memória que as camadas em que se pinta. A gravação segue a mesma regra: os buffers ainda ocultos copiam-se ou omitem-se como ficheiros, sem serem carregados de novo para a memória só para os voltar a gravar.

## Pontos de controlo que gravam só o que mudou

Ficheiro → Guardar actualiza o projecto de trabalho. As gravações incrementais e a gravação automática escrevem numa árvore de recuperação, e só escrevem dados alterados — buffers de camada modificados, não uma segunda cópia da imagem inteira. Cada ponto de controlo leva ainda uma descrição completa da árvore de camadas, pelo que qualquer ponto desse historial se pode abrir preenchendo os pixels inalterados a partir de pontos de controlo mais antigos e, se for preciso, a partir do próprio ficheiro de trabalho.

A gravação automática usa o mesmo padrão numa cache à parte, para que a protecção automática não tenha de reescrever o ficheiro no disco. Se um projecto se abrir quando existem pontos de controlo mais recentes do que a última gravação completa, o Lumi pode oferecê-los em vez de descartar em silêncio o trabalho mais recente. As imagens recuperadas abrem com um nome distinto, para que uma gravação rápida não possa substituir o original.

## Um formato de trabalho

O `.lum` serve para continuar uma pintura no Lumi. Os formatos achatados ou de compatibilidade servem para publicar, entregar e usar noutras aplicações. Como um projecto é um directório de muitos ficheiros, deve ser arquivado se precisar de viajar.

O ficheiro de trabalho permanece rico e editável. As exportações são a forma de uma imagem acabada ou partilhada sair dessa estrutura.
