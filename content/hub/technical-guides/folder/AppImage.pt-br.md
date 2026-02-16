---
title: "AppImage"
type: docs
url: "hub/technical-guides/folder/AppImage"
---
Um AppImage é um pacote de aplicativos Linux de arquivo único. Você baixa um arquivo, marca-o como executável e executa-o sem instalar software em todo o sistema.

Site oficial da AppImage: https://appimage.org/

O AppImage fornece uma versão portátil do Lumi que roda sem instalação ou modificação do sistema. É ideal para artistas que desejam usar o software imediatamente, sem gerenciar dependências, compilar código-fonte ou configurar um ambiente de desenvolvimento.

Como um executável independente, o AppImage pode ser armazenado em qualquer lugar do sistema. Isso facilita testar novos lançamentos, manter múltiplas versões ou mover o software entre máquinas.

Para o processo de desenvolvimento do Lumi, o AppImage funciona como uma construção de teste portátil que se aproxima da saída de integração contínua. Isso permite testes confiáveis ​​em um ambiente consistente, ao mesmo tempo que mantém as compilações de fontes locais focadas no trabalho de desenvolvimento.

## Lançamento vs Desenvolvimento AppImage

- **Lançamento AppImage**: ainda não disponível (Lumi não foi lançado).
- **AppImage de desenvolvimento (artefato de CI)**: gerado automaticamente a partir de commits de desenvolvimento contínuo para teste.

Este guia cobre principalmente o fluxo de trabalho **desenvolvimento AppImage**.

Página atual do artefato:

https://gitlab.gnome.org/pixelmixer/lumi-dev/-/artifacts

## CI AppImage Noções básicas de download

CI produz arquivos zip de artefato (por exemplo `lumi-appimage*.zip`).

Fluxo manual básico:

1. Baixe o zip do artefato CI mais recente.
2. Extraia-o.
3. Execute o arquivo `Lumi*.AppImage` incluído.

Os scripts abaixo são auxiliares opcionais que automatizam essas etapas.

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# Unpack latest downloaded CI zip from ~/Downloads
bash lumi-appimage-unpack-zip.sh

# Launch AppImage with terminal output
bash lumi-appimage-launch.sh
```

## Scripts auxiliares opcionais

-`lumi-appimage-unpack-zip.sh`
  - encontra o `lumi-appimage*.zip` mais recente em `~/Downloads`
  - instala AppImage em `~/AppImage/Lumi/Lumi_CI.AppImage`
  - instala recursos de desktop em `~/.local/share/applications/lumi.desktop`

-`lumi-appimage-launch.sh`
  - inicia o AppImage em um terminal
  - ativa a saída em tempo de execução (`APPIMAGE_DEBUG=1`)

## Notas Comuns

- Se você executar o AppImage manualmente (sem scripts auxiliares), torne-o executável primeiro:

```bash
chmod +x ~/AppImage/Lumi/Lumi_CI.AppImage
```

`lumi-appimage-unpack-zip.sh` já aplica permissões executáveis ​​automaticamente.

- Se o Lumi já estiver rodando de outro build, feche-o antes de iniciar o AppImage.