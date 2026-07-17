---
title: "AppImage"
type: docs
url: "hub/technical-guides/AppImage"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 939beb6f4f1657ab77f785d1753385360cca7920b6291dbcd09f4687bfdfc502
---
Um AppImage é um pacote de aplicação Linux num único ficheiro. Descarrega-se um ficheiro, marca-se como executável e executa-se sem instalar software em todo o sistema.

Site oficial do AppImage: https://appimage.org/

O AppImage fornece uma versão portátil do Lumi que corre sem instalação nem modificação do sistema. É ideal para artistas que querem usar o software de imediato, sem gerir dependências, compilar código-fonte ou configurar um ambiente de desenvolvimento.

Como executável autónomo, o AppImage pode ser guardado em qualquer local do sistema. Isto facilita testar novos lançamentos, manter várias versões ou mover o software entre máquinas.

No processo de desenvolvimento do Lumi, o AppImage funciona como uma compilação de teste portátil muito próxima da saída de integração contínua. Permite testes fiáveis num ambiente consistente, mantendo as compilações locais a partir do código-fonte focadas no trabalho de desenvolvimento.

Nota: a CI compila o AppImage usando fontes de dependências integradas no repositório do Lumi (BABL/GEGL/GTK3), pelo que a pilha de dependências é consistente com o fluxo de trabalho local `lumi-build-script.sh`.

## AppImage de lançamento vs. de desenvolvimento

- **AppImage de lançamento**: ainda não disponível (o Lumi ainda não foi lançado).
- **AppImage de desenvolvimento (artefacto de CI)**: gerado automaticamente a partir de commits de desenvolvimento contínuo para testes.

Este guia cobre sobretudo o fluxo de trabalho do **AppImage de desenvolvimento**.

Página actual de artefactos:

https://gitlab.gnome.org/pixelmixer/lumi-dev/-/artifacts

## Noções básicas de descarregamento do AppImage de CI

A CI produz ficheiros zip de artefactos (por exemplo `lumi-appimage*.zip`).

Fluxo manual básico:

1. Descarregue o zip do artefacto de CI mais recente.
2. Extraia-o.
3. Execute o ficheiro `Lumi*.AppImage` incluído.

Os scripts abaixo são auxiliares opcionais que automatizam estes passos.

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# Descompactar o zip de CI mais recente descarregado de ~/Downloads
bash lumi-appimage-unpack-zip.sh

# Lançar o AppImage com saída no terminal
bash lumi-appimage-launch.sh
```

## Scripts auxiliares opcionais

- `lumi-appimage-unpack-zip.sh`
  - encontra o `lumi-appimage*.zip` mais recente em `~/Downloads`
  - instala o AppImage em `~/AppImage/Lumi/Lumi_CI.AppImage`
  - instala recursos de secretária em `~/.local/share/applications/lumi.desktop`

- `lumi-appimage-launch.sh`
  - lança o AppImage num terminal
  - activa a saída em tempo de execução (`APPIMAGE_DEBUG=1`)

## Notas comuns

- Se executar o AppImage manualmente (sem scripts auxiliares), torne-o executável primeiro:

```bash
chmod +x ~/AppImage/Lumi/Lumi_CI.AppImage
```

`lumi-appimage-unpack-zip.sh` já aplica permissões de execução automaticamente.

- Se o Lumi já estiver a correr a partir de outra compilação, feche-o antes de lançar o AppImage.
