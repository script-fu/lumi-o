---
title: "Compilar uma versão de depuração"
type: docs
url: "hub/technical-guides/Building-a-Debug-Version"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: fecc781e73b4f30881c5150c958ae9b2df4164acd4cf86926186acb8e2021d5f
---
Este guia descreve o **fluxo de trabalho de depuração local** do Lumi usando scripts em `build/lumi/scripts`.

O fluxo de trabalho foi concebido para:

- usar artefactos de compilação locais (sem descarregamento de símbolos),
- verificar que os símbolos de depuração estão efectivamente presentes,
- lançar o GDB com modo de símbolos offline por predefinição.

## Pré-requisitos

- Linux baseado em Debian (referência do projecto: Debian 13)
- Árvore de código-fonte do Lumi já clonada

## Configuração única do GDB (opcional, mas recomendada)

Instale as ferramentas GDB:

```bash
sudo apt update
sudo apt install gdb gdbserver
```

Configuração opcional de registo local:

```bash
mkdir -p ~/code/gdb_logs
cat > ~/.gdbinit <<'EOF'
set logging file ~/code/gdb_logs/gdb_log.txt
set logging enabled on
set logging overwrite on
EOF
```

Nota: os scripts de depuração local do Lumi desactivam `debuginfod` por predefinição para manter a resolução de símbolos local e reproduzível.

## Início rápido

A partir do directório de scripts:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
```

### Compilação de depuração + lançamento (predefinição)

Use isto para sessões normais de depuração.

```bash
bash lumi-debug-local.sh lumi-dev build
```

Este comando:

1. compila o Lumi em modo de depuração,
2. verifica os símbolos de depuração,
3. lança o Lumi sob o GDB.

### Apenas compilação de depuração (para TTY/sessão remota posterior)

Use isto quando quiser compilar agora e lançar/depurar mais tarde.

```bash
bash lumi-build-debug.sh lumi-dev build
```

## Usar TTYs no Linux

Os TTYs (consolas de texto) costumam ser a forma mais fiável de depurar bloqueios graves.

- Mude para um TTY com `Ctrl + Alt + F1` até `Ctrl + Alt + F6`
- Inicie sessão a partir do prompt de texto
- Regresse à sessão gráfica com `Ctrl + Alt + F7` (ou `F2` em alguns sistemas)

Porque isto importa: se a sessão gráfica estiver bloqueada, um TTY muitas vezes continua a responder, permitindo ligar o GDB, capturar um backtrace e recuperar dados úteis de falhas.

## Opcional: depuração remota/TTY

Para bloqueios graves ou bloqueios de ecrã, use `gdbserver`:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash gdbserver.sh
```

Depois, a partir de um TTY (recomendado para cenários de bloqueio) ou outro terminal:

```bash
gdb /home/mark/code/lumi-dev/bin/lumi-0.1
(gdb) target remote localhost:9999
(gdb) continue
```

Para um lançamento local do GDB (caminho sem TTY):

```bash
bash lumi-debug-launch.sh --repo lumi-dev
```

## Nota de desempenho

As compilações de depuração são mais lentas por concepção. Quando terminar a depuração, volte a uma compilação mais rápida:

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# Reposição completa em release de todos os componentes principais
bash lumi-debug-reset-release.sh lumi-dev

# Variante local opcional mais rápida
bash lumi-build-script.sh --scope build --dir lumi-dev --type debugoptimized
```
