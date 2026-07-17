---
title: "Recuperação de ficheiros"
type: docs
translation_provenance: ai-reviewed
translation_source_sha256: 59495d24302cb3493b90bc61a6dd1ffb9bb9c30b179f7be388882fe4f45a5075
url: "hub/features/recovery"
translation_lock: true
---
O sistema de recuperação do Lumi foi concebido para proteger o trabalho de pintura de falhas, erros e sessões interrompidas. Oferece aos projectos uma rede de segurança sem forçar os artistas a duplicar ficheiros manualmente de forma constante.

A recuperação assenta em duas ideias: protecção automática em segundo plano e pontos de controlo intencionais. Juntas, ajudam a preservar o trabalho recente, permitindo ainda regressar a momentos anteriores de um projecto.

![recover](/images/screens/recover.jpg)

## Protecção automática

Enquanto uma imagem está a ser editada, o Lumi pode manter dados de recuperação separados do ficheiro de trabalho principal. Isto significa que o projecto não precisa de ser reescrito cada vez que é feito um instantâneo de segurança.

Se algo correr mal, o estado de recuperação automática pode fornecer uma versão recente da obra, por vezes mais recente do que a última gravação deliberada. O objectivo é simples: reduzir a quantidade de trabalho perdido quando uma sessão termina inesperadamente.

## Pontos de controlo intencionais

Alguns momentos de uma pintura valem a pena preservar deliberadamente: antes de uma grande mudança de cor, depois de um esboço bem-sucedido, antes de decisões de achatamento ou ao tentar uma direcção arriscada.

O Lumi suporta pontos de controlo ao nível do projecto para estes momentos. São mais leves do que manter uma cópia completa separada para cada experiência, mas ainda dão ao artista uma forma de recuar para pontos significativos no historial da obra.

## Recuperar com contexto

Os estados de recuperação são apresentados como versões da obra, e não como ficheiros brutos a procurar manualmente. Isto permite comparar gravações automáticas recentes e pontos de controlo deliberados e abrir o estado que melhor corresponde ao trabalho a continuar.

As imagens recuperadas abrem como documentos de trabalho, permitindo inspeccioná-las antes de decidir como gravar ou continuar.

## Manter a recuperação prática

Um sistema de recuperação útil também tem de permanecer gerível. O Lumi foi concebido para manter os dados de recuperação organizados e tornar estados antigos removíveis quando já não são necessários.

Isto evita que a segurança se torne desordem. A recuperação pode permanecer activa em segundo plano, enquanto os artistas mantêm forma de controlar quanto historial é retido ao longo do tempo.

## Confiança durante o trabalho

O objectivo da recuperação de ficheiros não é substituir a gravação, mas tornar o trabalho criativo menos frágil. Os artistas podem pintar, experimentar e arriscar sabendo que o Lumi mantém formas adicionais de regressar quando uma sessão, um ficheiro ou uma decisão corre mal.
