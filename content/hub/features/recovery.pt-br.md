---
title: "Recuperação de arquivos"
type: docs
url: "hub/features/recovery"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 59495d24302cb3493b90bc61a6dd1ffb9bb9c30b179f7be388882fe4f45a5075
---

O sistema de recuperação do Lumi protege trabalhos de pintura contra travamentos, erros e sessões interrompidas. Oferece aos projetos uma rede de segurança sem forçar artistas a duplicar arquivos manualmente o tempo todo.

A recuperação se baseia em duas ideias: proteção automática em segundo plano e pontos de verificação intencionais. Juntos, ajudam a preservar trabalhos recentes e ainda permitem retornar a momentos anteriores de um projeto.

![recover](/images/screens/recover.jpg)

## Proteção automática

Enquanto uma imagem está sendo editada, o Lumi pode manter os dados de recuperação separados do arquivo de trabalho principal. O projeto em si não precisa ser reescrito toda vez que um instantâneo de segurança é criado.

Se algo der errado, o estado de recuperação automática pode fornecer uma versão recente da arte, possivelmente mais nova que o último salvamento deliberado. O objetivo é simples: reduzir a quantidade de trabalho perdido quando uma sessão termina inesperadamente.

## Pontos de verificação intencionais

Alguns momentos de uma pintura valem a pena preservar de propósito: antes de uma grande mudança de cor, depois de um esboço bem-sucedido, antes de decisões de achatamento ou ao tentar uma direção arriscada.

O Lumi oferece pontos de verificação em nível de projeto para esses momentos. Eles são mais leves do que manter uma cópia completa separada para cada experimento, mas ainda permitem voltar a pontos significativos na história do trabalho.

## Recuperação com contexto

Os estados de recuperação são apresentados como versões da arte, não como arquivos brutos para vasculhar manualmente. O artista pode comparar salvamentos automáticos recentes e pontos de verificação deliberados e, em seguida, abrir o estado que melhor corresponde ao trabalho a partir do qual deseja continuar.

As imagens recuperadas abrem como documentos de trabalho, permitindo inspecioná-las antes de decidir como salvar ou prosseguir.

## Manter a recuperação prática

Um sistema de recuperação útil também precisa permanecer gerenciável. O Lumi mantém os dados de recuperação organizados e permite remover estados antigos quando não são mais necessários.

Isso evita que a segurança vire desordem. A recuperação pode permanecer ativa em segundo plano, enquanto os artistas ainda controlam quanto histórico é retido ao longo do tempo.

## Confiança durante o trabalho

O objetivo da recuperação de arquivos não é substituir o salvamento, mas tornar o trabalho criativo menos frágil. Os artistas podem pintar, experimentar e correr riscos sabendo que o Lumi mantém caminhos adicionais de retorno quando uma sessão, arquivo ou decisão dá errado.
