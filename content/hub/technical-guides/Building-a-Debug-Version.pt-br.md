---
title: "Construindo uma versão de depuração"
type: docs
url: "hub/technical-guides/Building-a-Debug-Version"
---
Este guia descreve o **fluxo de trabalho de depuração local** para Lumi usando scripts em `build/lumi/scripts`.

O fluxo de trabalho foi projetado para:

- usar artefatos de construção local (não é necessário fazer download de símbolos),
- verifique se os símbolos de depuração estão realmente presentes,
- inicie o GDB com modo de símbolo offline por padrão.

## Pré-requisitos

- Linux baseado em Debian (linha de base do projeto: Debian 13)
- Árvore fonte Lumi já clonada

## Configuração única do GDB (opcional, mas recomendada)

Instale as ferramentas GDB:

```bash
sudo apt update
sudo apt install gdb gdbserver
```

Configuração opcional de registro local:

```bash
mkdir -p ~/code/gdb_logs
cat > ~/.gdbinit <<'EOF'
set logging file ~/code/gdb_logs/gdb_log.txt
set logging enabled on
set logging overwrite on
EOF
```

Nota: Os scripts de depuração local do Lumi desabilitam `debuginfod` por padrão para manter a resolução do símbolo local e reproduzível.

## Início rápido

No diretório de scripts:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
```

### Debug Build + Launch (padrão)

Use isso para sessões normais de depuração.

```bash
bash lumi-debug-local.sh lumi-dev build
```

Este comando:

1. constrói Lumi em modo de depuração,
2. verifica símbolos de depuração,
3. lança o Lumi no GDB.

### Debug Build Only (para TTY/sessão remota posterior)

Use isto quando quiser compilar agora e iniciar/depurar mais tarde.

```bash
bash lumi-build-debug.sh lumi-dev build
```

## Usando TTYs no Linux

TTYs (consoles de texto) costumam ser a maneira mais confiável de depurar congelamentos graves.

- Mude para um TTY com `Ctrl + Alt + F1` através de `Ctrl + Alt + F6`
- Faça login a partir do prompt de texto
- Retorne à sessão gráfica com `Ctrl + Alt + F7` (ou `F2` em alguns sistemas)

Por que isso é importante: se a sessão da área de trabalho estiver paralisada, um TTY geralmente ainda responde, para que você possa anexar o GDB, capturar um backtrace e recuperar dados úteis de falhas.

## Opcional: depuração remota/TTY

Para congelamentos bruscos ou bloqueios de exibição, use `gdbserver`:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash gdbserver.sh
```

Então, de um TTY (recomendado para cenários de congelamento) ou outro terminal:

```bash
gdb /home/mark/code/lumi-dev/bin/lumi-0.1
(gdb) target remote localhost:9999
(gdb) continue
```

Para uma inicialização local do GDB (caminho não TTY):

```bash
bash lumi-debug-launch.sh --repo lumi-dev
```

## Nota de desempenho

As compilações de depuração são mais lentas por design. Quando terminar a depuração, volte para uma compilação mais rápida:

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# Full release reset of all major components
bash lumi-debug-reset-release.sh lumi-dev

# Optional faster local-only variant
bash lumi-build-script.sh --scope build --dir lumi-dev --type debugoptimized
```