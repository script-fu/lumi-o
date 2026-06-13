---
title: "Recuperação de arquivos"
type: docs
---
O sistema de recuperação do Lumi foi projetado para proteger os trabalhos de pintura contra travamentos, erros e sessões interrompidas. Ele oferece aos projetos uma rede de segurança sem forçar os artistas a duplicar arquivos manualmente.

A recuperação é construída em torno de duas ideias: proteção automática em segundo plano e pontos de verificação intencionais. Juntos, eles ajudam a preservar trabalhos recentes, ao mesmo tempo que permitem ao artista retornar a momentos anteriores de um projeto.

![recover](/images/screens/recover.jpg)

## Proteção automática

Enquanto uma imagem está sendo editada, o Lumi pode manter os dados de recuperação separados do arquivo de trabalho principal. Isto significa que o projeto em si não precisa ser reescrito toda vez que um instantâneo de segurança é feito.

Se algo der errado, o estado de recuperação automática poderá fornecer uma versão recente da arte que pode ser mais recente do que o último salvamento deliberado. O objetivo é simples: reduzir a quantidade de trabalho perdido quando uma sessão termina inesperadamente.

## Pontos de verificação intencionais

Alguns momentos de uma pintura merecem ser preservados deliberadamente: antes de uma grande mudança de cor, depois de um esboço bem-sucedido, antes de decisões de nivelamento ou ao tentar uma direção arriscada.

Lumi oferece suporte a pontos de verificação em nível de projeto para esses momentos. Eles são mais leves do que manter uma cópia completa separada para cada experimento, mas ainda dão ao artista uma maneira de voltar a pontos significativos na história do trabalho.

## Recuperando com contexto

Os estados de recuperação são apresentados como versões da arte, e não como arquivos brutos para serem pesquisados manualmente. Isso permite que um artista compare salvamentos automáticos recentes e pontos de verificação deliberados e, em seguida, abra o estado que melhor corresponde ao trabalho a partir do qual deseja continuar.

As imagens recuperadas abrem como documentos de trabalho, permitindo ao artista inspecioná-las antes de decidir como salvá-las ou continuar.

## Mantendo a recuperação prática

Um sistema de recuperação útil também deve permanecer gerenciável. O Lumi foi projetado para manter os dados de recuperação organizados e tornar os estados antigos removíveis quando não forem mais necessários.

Isso evita que a segurança se torne uma desordem. A recuperação pode permanecer ativa em segundo plano, enquanto os artistas ainda têm uma maneira de controlar quanto histórico é retido ao longo do tempo.

## Confiança durante o trabalho

O objetivo da recuperação de arquivos não é substituir o salvamento, mas sim tornar o trabalho criativo menos frágil. Os artistas podem pintar, experimentar e correr riscos sabendo que Lumi mantém caminhos adicionais quando uma sessão, arquivo ou decisão dá errado.