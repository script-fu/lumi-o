---
title: "CI do GitLab"
type: docs
url: "hub/technical-guides/folder/GitLab-CI"
---
Integração Contínua (CI) é uma forma de testar, construir e validar automaticamente seu código sempre que alterações são feitas.

**GitLab** fornece recursos integrados de CI/CD por meio de seu arquivo `.gitlab-ci.yml`. Este arquivo, colocado na raiz do seu repositório, informa ao GitLab como construir e testar seu projeto. Ele define estágios e scripts que são executados em um ambiente limpo sempre que alterações são enviadas.

Este documento descreve como funciona o pipeline GitLab CI/CD da Lumi, incluindo a função do arquivo `.gitlab-ci.yml`, scripts de shell e ferramentas externas como Meson e Ninja.

Para documentação técnica detalhada do processo de construção do Lumi CI, consulte [README-CI.md](https://gitlab.gnome.org/pixelmixer/lumi/-/blob/main/build/linux/appimage/README-CI.md) no repositório.

## Noções básicas de CI/CD do GitLab

O CI é controlado por um arquivo denominado `.gitlab-ci.yml`. Este arquivo define:

- **Estágios**: grupos ordenados de trabalhos (por exemplo, `build-this`, `build-that`, `package-up`)
- **Trabalhos**: tarefas individuais a serem executadas em cada estágio
- **Scripts**: comandos Shell executados para cada trabalho
- **Runners**: computadores que o GitLab usa para executar trabalhos definidos no pipeline.

No Lumi, as etapas do pipeline são:

- `dependencies`
-`build lumi`
- `appimage`

## Construções baseadas em contêiner

O pipeline Lumi usa conteinerização para construções consistentes:

1. **Criando o Build Container**: O primeiro estágio usa Buildah para criar uma imagem Docker com todas as dependências
2. **Usando o contêiner**: os estágios subsequentes são executados dentro deste contêiner, garantindo um ambiente consistente
3. **Construções reproduzíveis**: o isolamento do contêiner garante os mesmos resultados em diferentes executores

Essa abordagem garante que os builds funcionem da mesma maneira em qualquer executor do GitLab e fornece um ambiente controlado para processos de build complexos.

## Função dos scripts Shell

Jobs em `.gitlab-ci.yml` normalmente invocam comandos shell diretamente. Operações complexas são frequentemente movidas para scripts separados armazenados no repositório.

O Lumi CI usa scripts de shell modulares para organizar a lógica de construção:

**Exemplo de invocação de script:**
```yaml
script:
  - bash build/linux/appimage/lumi-goappimage.sh 2>&1 | tee appimage_creation.log
```

**Benefícios desta abordagem:**
- **Clean YAML**: Mantém o arquivo `.gitlab-ci.yml` focado na estrutura do trabalho
- **Manutenção**: lógica complexa é mais fácil de depurar e modificar em scripts shell
- **Reutilização**: os scripts podem ser usados em diferentes contextos ou ambientes
- **Modularidade**: Diferentes aspectos da construção podem ser separados em scripts específicos

Isso mantém a configuração do CI limpa enquanto permite processos de construção sofisticados.

## Integração com sistemas de construção

Lumi usa **Meson** e **Ninja** para preparar e depois construir o código.

Por exemplo:

```
script:
  - meson setup _build-${CI_RUNNER_TAG} -Dprefix="${LUMI_PREFIX}"
  - ninja -C _build-${CI_RUNNER_TAG}
  - ninja -C _build-${CI_RUNNER_TAG} install
```

Aqui:

- `meson setup` prepara o diretório de construção e gera `build.ninja`
- `ninja` executa os comandos de construção conforme definido

## Estrutura do sistema de construção Meson

O sistema de compilação **Meson** usa um arquivo raiz `meson.build` colocado no diretório raiz do projeto. Este arquivo define a configuração de compilação de nível superior e o ponto de entrada para o processo de compilação.

- A raiz `meson.build` normalmente está localizada no mesmo diretório que `.gitlab-ci.yml`
- A partir daí, ele **cascata recursivamente** em subdiretórios, cada um dos quais pode ter seu próprio arquivo `meson.build`
- Esses arquivos de subdiretórios definem destinos, fontes, dependências e instruções de construção relevantes para esse diretório

## Variáveis de ambiente

As principais variáveis no pipeline do Lumi incluem:

```yaml
variables:
  DEBIAN_FRONTEND: "noninteractive"  # Prevents interactive prompts
  DEB_VERSION: "trixie"              # Debian version for consistency
  CI_RUNNER_TAG: "x86_64"            # Architecture specification
```

**Variáveis específicas do trabalho:**
```yaml
build-lumi:
  variables:
    COMPILER: "clang"                                           # Compiler selection
    LINKER: "lld"                                               # Linker selection
    LUMI_PREFIX: "${CI_PROJECT_DIR}/_install-${CI_RUNNER_TAG}"  # Installation path
    DEPS_PREFIX: "/opt/lumi-deps"                               # Prebuilt dependency prefix
    MESON_OPTIONS: "-Dpkgconfig.relocatable=true -Drelocatable-bundle=yes"  # Build configuration
```Essas variáveis ​​controlam o comportamento de construção e garantem consistência entre diferentes estágios e executores.

## Exemplo de estrutura

```
project-root/
├── .gitlab-ci.yml
├── meson.build              <-- Root Meson file
├── src/
│   ├── meson.build          <-- Subdirectory Meson file
│   └── some_source.c
├── data/
│   ├── meson.build
│   └── icons/
```

Nesta estrutura:

- O arquivo raiz `meson.build` configura o ambiente geral de construção
- Os arquivos do subdiretório `meson.build` lidam com detalhes de compilação para componentes ou módulos específicos
- Este layout hierárquico mantém a lógica de construção modular e sustentável

## Artefatos entre estágios

Artefatos são arquivos gerados por trabalhos necessários nas etapas subsequentes:

```yaml
build-lumi:
  # ...job configuration...
  artifacts:
    paths:
      - "${LUMI_PREFIX}/"      # Installation files
      - _build-${CI_RUNNER_TAG}/meson-logs/meson-log.txt  # Build logs
```

## Estágios e dependências do pipeline

O pipeline Lumi consiste em três etapas principais:

1. **Dependências**: Cria um ambiente de construção em contêiner com todas as ferramentas e bibliotecas necessárias
2. **Build Lumi**: Compila Lumi usando Meson e Ninja no ambiente preparado
3. **AppImage**: empacota o aplicativo criado em um formato AppImage distribuível

**Dependências do Estágio:**
```yaml
build-lumi:
  needs: [deps-debian]  # Waits for dependency container

lumi-appimage:
  needs: [build-lumi] # Waits for application build
```

Cada estágio é executado somente depois que suas dependências são concluídas com êxito, garantindo a ordem de construção adequada e a disponibilidade do artefato.

## Nomes de empregos atuais

O Lumi `.gitlab-ci.yml` atualmente define estes nomes de trabalho:

- `deps-debian`
-`build-lumi`
- `lumi-appimage`

## Resumo

- `.gitlab-ci.yml` define a estrutura e lógica do pipeline
- Os trabalhos contêm comandos shell ou scripts externos
- Ferramentas como Meson e Ninja são usadas dentro de jobs como parte do processo de construção

Lumi usa GitLab CI para construir automaticamente seu AppImage para plataformas baseadas em Debian. O pipeline cria dependências, compila o Lumi e, em seguida, empacota um AppImage.

Para detalhes no nível da fonte, use:

- `.gitlab-ci.yml` na raiz do repositório Lumi
- `build/linux/appimage/lumi-goappimage.sh`
- `build/linux/appimage/README-CI.md`

Para obter detalhes técnicos abrangentes sobre o processo de construção do Lumi CI, incluindo configuração do ambiente, arquitetura de script e solução de problemas, consulte [README-CI.md](https://gitlab.gnome.org/pixelmixer/lumi/-/blob/main/build/linux/appimage/README-CI.md).