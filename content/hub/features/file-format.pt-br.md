---
title: "Formato de arquivo (.lum)"
type: docs
---
Lumi usa um formato de arquivo aberto baseado em diretório (`.lum`) projetado para desempenho, confiabilidade e acessibilidade de longo prazo.

## Visão geral

Um arquivo `.lum` é na verdade um diretório que contém:
- **Metadados** (camadas, modos de mesclagem, propriedades).
- **Buffers de camada** (dados de pixel individuais para cada camada).
- **Máscaras** (dados em escala de cinza para máscaras de camada).
- **Histórico de recuperação** (instantâneos incrementais).

Essa estrutura permite salvar rapidamente, carregar lentamente arquivos grandes e recuperar o trabalho mesmo após uma falha.

## Principais Propriedades

### Aberto e legível

O formato `.lum` usa metadados XML e buffers binários compactados. Você pode inspecionar a estrutura da camada, propriedades e modos de mesclagem em texto simples. Nenhum codec proprietário; os dados de pixel são armazenados no formato de buffer GEGL padrão.

### Economia incremental

O salvamento incremental deve ser habilitado por projeto na caixa de diálogo **Salvar como** (uma caixa de seleção **Salvar incremental** e um botão giratório **Max Saves**). Uma vez ativado, Ctrl+S grava apenas as camadas modificadas em vez de reescrever todo o projeto, reduzindo drasticamente o tempo de economia. A configuração é armazenada com o projeto e persiste entre as sessões.

### Carregamento lento

Grandes projetos abrem rapidamente. Os pixels da camada são carregados do disco somente quando:
- A camada fica visível.
- Você pinta na camada.
- A camada é exportada ou composta.

Projetos muito grandes (mais de 500 camadas, vários gigabytes de dados) permanecem responsivos. O carregamento lento é ativado por padrão e pode ser alternado em **Editar → Preferências → Desempenho → Recursos de memória**.

### Salvamento automático

Lumi salva automaticamente as alterações em um **local de cache separado** (`~/.cache/lumi/autosave/`) em intervalos regulares. Os salvamentos automáticos são independentes do arquivo de trabalho e não o modificam. O intervalo e a localização do cache são configuráveis ​​em **Editar → Preferências → Desempenho**.

## Acesso

### Salvar e salvar como

- **Arquivo** → **Salvar** (Ctrl+S): Salva no diretório `.lum` atual.
- **Arquivo** → **Salvar como** (Shift+Ctrl+S): Salve em um novo arquivo `.lum`. A caixa de diálogo Salvar como inclui opções para o tipo de compactação e uma alternância **Salvar incremental** (com um limite de **Salvamentos máximos**) para ativar ou desativar o salvamento incremental para este projeto.

As alterações não salvas são indicadas por um asterisco (*) no título da janela.

### Exportar

- **Arquivo** → **Exportar como** (Shift+Ctrl+E): exporte para PNG, JPEG, TIFF ou outros formatos.
- **Arquivo** → **Sobrescrever** (Ctrl+E): Exporte novamente para o último arquivo exportado.

A exportação nivela as camadas visíveis e converte do espaço de cores espectral para sRGB.

### Importar

- **Arquivo** → **Abrir** (Ctrl+O): Carrega um projeto `.lum`.
- **Arquivo** → **Abrir como camadas** (Shift+Ctrl+O): Importe arquivos `.lum`, XCF ou PSD como novas camadas.
- **Arquivo** → **Arquivos recentes**: acesso rápido a projetos abertos recentemente.

Os arquivos PSD e XCF são convertidos para o formato nativo do Lumi na importação.

## Compatibilidade de importação e exportação

### Formatos de importação suportados
- **.lum**: formato nativo do Lumi.
- **.xcf**: formato nativo do GIMP (camadas e propriedades básicas preservadas).
- **.psd**: formato Photoshop (camadas e modos de mesclagem preservados).
- **PNG, JPEG, TIFF, etc.**: Importação de imagem achatada.

### Formatos de exportação suportados
- **PNG**: Sem perdas, com transparência alfa.
- **JPEG**: com perdas, achatado.
- **TIFF**: Sem perdas ou compactado em LZW.
- **XCF**: formato de compatibilidade com GIMP. Somente exportação; camadas e propriedades básicas preservadas.

## Recuperação de ProjetoO Lumi mantém salvamentos automáticos em segundo plano e pontos de verificação incrementais manuais, ambos acessíveis em **Arquivo** → **Recuperar imagem**. Consulte a página [File Recovery](../recovery) para obter detalhes completos.

## Organização

Um arquivo `.lum` é um diretório com uma estrutura fixa:

```
my-painting.lum/
  ├── metadata.xml                       (image structure, layer tree, properties)
  ├── thumbnail-YYYYMMDD-HHMMSS.png      (last-saved thumbnail)
  ├── drawables/
  │   ├── layer-<name>.geglbuf           (pixel data per layer)
  │   └── mask-<name>.geglbuf            (mask data, shares layer name)
  ├── icc/                               (embedded colour profiles)
  ├── parasites/                         (per-image metadata)
  ├── paths/                             (vector paths as SVG)
  ├── configs/                           (non-destructive filter configurations)
  └── recovery/
      └── primary-01.lum/                (incremental save checkpoint)
          ├── metadata.xml
          ├── drawables/                 (only modified buffers)
          ├── delta-0001.lum/            (Ctrl+S checkpoint)
          └── delta-0002.lum/
```

Os buffers de camada recebem o nome da camada (`layer-Background.geglbuf`), não numerados sequencialmente. Os espaços nos nomes das camadas são armazenados como sublinhados; camadas de grupo recebem um sufixo `-GROUP`. As máscaras compartilham o nome da camada (`mask-Background.geglbuf`).

Cada `recovery/primary-NN.lum/` é um salvamento de linha de base completo. Os pressionamentos subsequentes de Ctrl+S acrescentam `delta-NNNN.lum/` subdiretórios contendo apenas os buffers modificados desde a última linha de base, mantendo os pontos de verificação salvos rapidamente, independentemente do tamanho do projeto.

Os salvamentos automáticos seguem a mesma estrutura, mas são armazenados separadamente em `~/.cache/lumi/autosave/`, deixando o arquivo de trabalho intacto.
- **Projetos muito grandes**: um projeto com mais de 1.000 camadas e terabytes de dados se beneficiará mais com o carregamento lento; entretanto, a exportação final para o formato de imagem plana pode levar algum tempo.
- **Unidades de rede**: há suporte para salvar em diretórios montados na rede, mas é mais lento que o armazenamento local devido à latência de E/S.