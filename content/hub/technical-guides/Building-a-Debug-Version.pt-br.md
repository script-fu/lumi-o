---
title: "Compilar versão de depuração"
type: docs
url: "hub/technical-guides/Building-a-Debug-Version"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: fecc781e73b4f30881c5150c958ae9b2df4164acd4cf86926186acb8e2021d5f
---

Este guia descreve o **fluxo de trabalho local de depuração** do Lumi usando scripts em `build/lumi/scripts`.

O fluxo foi projetado para:

- usar artefatos de build locais (sem download de símbolos),
- verificar se os símbolos de depuração estão realmente presentes,
- iniciar o GDB com modo de símbolos offline por padrão.

## Pré-requisitos

- Linux baseado em Debian (linha de base do projeto: Debian 13)
- Árvore de código-fonte do Lumi já clonada

## Configuração única do GDB (opcional, mas recomendada)

Instale as ferramentas GDB:

```bash
sudo apt update
sudo apt install gdb gdbserver
```

Configuração opcional de log local:

```bash
mkdir -p ~/code/gdb_logs
cat > ~/.gdbinit <<'EOF'
set logging file ~/code/gdb_logs/gdb_log.txt
set logging enabled on
set logging overwrite on
EOF
```

Nota: os scripts locais de depuração do Lumi desabilitam `debuginfod` por padrão para manter a resolução de símbolos local e reproduzível.

## Início rápido

No diretório de scripts:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
```

### Build de depuração + iniciar (padrão)

Use isto para sessões normais de depuração.

```bash
bash lumi-debug-local.sh lumi-dev build
```

Este comando:

1. compila o Lumi em modo de depuração,
2. verifica os símbolos de depuração,
3. inicia o Lumi sob o GDB.

### Apenas build de depuração (para sessão TTY/remota posterior)

Use isto quando quiser compilar agora e iniciar/depurar depois.

```bash
bash lumi-build-debug.sh lumi-dev build
```

## Usando TTYs no Linux

TTYs (consoles de texto) costumam ser a forma mais confiável de depurar travamentos completos.

- Mude para um TTY com `Ctrl + Alt + F1` até `Ctrl + Alt + F6`
- Faça login no prompt de texto
- Volte à sessão gráfica com `Ctrl + Alt + F7` (ou `F2` em alguns sistemas)

Por que isso importa: se a sessão da área de trabalho travar, um TTY geralmente ainda responde, permitindo anexar o GDB, capturar um backtrace e recuperar dados úteis de falha.

## Opcional: depuração remota/TTY

Para travamentos completos ou bloqueio de exibição, use `gdbserver`:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash gdbserver.sh
```

Depois, a partir de um TTY (recomendado em travamentos) ou outro terminal:

```bash
gdb /home/mark/code/lumi-dev/bin/lumi-0.1
(gdb) target remote localhost:9999
(gdb) continue
```

Para iniciar o GDB localmente (sem TTY):

```bash
bash lumi-debug-launch.sh --repo lumi-dev
```

## Nota de desempenho

Builds de depuração são mais lentos por design. Quando terminar de depurar, volte a um build mais rápido:

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# Full release reset of all major components
bash lumi-debug-reset-release.sh lumi-dev

# Optional faster local-only variant
bash lumi-build-script.sh --scope build --dir lumi-dev --type debugoptimized
```
