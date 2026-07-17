---
title: "AppImage"
type: docs
url: "hub/technical-guides/AppImage"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 939beb6f4f1657ab77f785d1753385360cca7920b6291dbcd09f4687bfdfc502
---

Um AppImage é um pacote de aplicativo Linux em arquivo único. Você baixa um arquivo, marca-o como executável e o executa sem instalar software em todo o sistema.

Site oficial do AppImage: https://appimage.org/

O AppImage fornece uma versão portátil do Lumi que roda sem instalação nem modificação do sistema. É ideal para artistas que querem usar o software imediatamente, sem gerenciar dependências, compilar código-fonte ou configurar um ambiente de desenvolvimento.

Como executável autônomo, o AppImage pode ser armazenado em qualquer lugar do sistema. Isso facilita testar novos lançamentos, manter várias versões ou mover o software entre máquinas.

No processo de desenvolvimento do Lumi, o AppImage funciona como um build de teste portátil que corresponde de perto à saída da integração contínua. Isso permite testes confiáveis em um ambiente consistente, mantendo as compilações locais focadas no trabalho de desenvolvimento.

Nota: o CI constrói o AppImage usando as fontes de dependência integradas do Lumi no repositório (BABL/GEGL/GTK3), de modo que a pilha de dependências é consistente com o fluxo local `lumi-build-script.sh`.

## AppImage de release vs. desenvolvimento

- **Release AppImage**: ainda não disponível (Lumi ainda não foi lançado).
- **Development AppImage (artefato de CI)**: gerado automaticamente a partir de commits de desenvolvimento em andamento para testes.

Este guia cobre principalmente o fluxo de trabalho do **development AppImage**.

Página atual de artefatos:

https://gitlab.gnome.org/pixelmixer/lumi-dev/-/artifacts

## Noções básicas de download do AppImage de CI

O CI produz arquivos zip de artefatos (por exemplo `lumi-appimage*.zip`).

Fluxo manual básico:

1. Baixe o zip de artefato de CI mais recente.
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

- `lumi-appimage-unpack-zip.sh`
  - encontra o `lumi-appimage*.zip` mais recente em `~/Downloads`
  - instala o AppImage em `~/AppImage/Lumi/Lumi_CI.AppImage`
  - instala recursos de desktop em `~/.local/share/applications/lumi.desktop`

- `lumi-appimage-launch.sh`
  - inicia o AppImage em um terminal
  - habilita saída de runtime (`APPIMAGE_DEBUG=1`)

## Observações gerais

- Se você executar o AppImage manualmente (sem scripts auxiliares), torne-o executável primeiro:

```bash
chmod +x ~/AppImage/Lumi/Lumi_CI.AppImage
```

`lumi-appimage-unpack-zip.sh` já aplica permissões de execução automaticamente.

- Se o Lumi já estiver em execução a partir de outro build, feche-o antes de iniciar o AppImage.
