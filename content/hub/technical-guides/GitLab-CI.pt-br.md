---
title: "GitLab CI"
type: docs
url: "hub/technical-guides/GitLab-CI"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 9917cebc417adeeae24d91b05b919b679a397d5db652cf4442d4330c0f8eeea5
---

Integração Contínua (CI) é uma forma de testar, compilar e validar automaticamente seu código sempre que alterações são feitas.

**GitLab** oferece recursos integrados de CI/CD por meio do arquivo `.gitlab-ci.yml`. Esse arquivo, colocado na raiz do repositório, informa ao GitLab como compilar e testar seu projeto. Ele define stages e scripts executados em um ambiente limpo sempre que alterações são enviadas.

Este documento descreve como funciona o pipeline CI/CD do GitLab no Lumi, incluindo o papel do arquivo `.gitlab-ci.yml`, scripts shell e ferramentas externas como Meson e Ninja.

Para documentação técnica detalhada do processo de build CI do Lumi, consulte [README-CI.md](https://gitlab.gnome.org/pixelmixer/lumi/-/blob/main/build/linux/appimage/README-CI.md) no repositório.

## Noções básicas de GitLab CI/CD

O CI é controlado por um arquivo chamado `.gitlab-ci.yml`. Esse arquivo define:

- **Stages**: grupos ordenados de jobs (por exemplo, `build-this`, `build-that`, `package-up`)
- **Jobs**: tarefas individuais executadas em cada stage
- **Scripts**: comandos shell executados para cada job
- **Runners**: computadores que o GitLab usa para executar jobs definidos no pipeline

No Lumi, os stages do pipeline são:

- `dependencies`
- `build lumi`
- `appimage`

## Builds baseados em contêiner

O pipeline do Lumi usa conteinerização para builds consistentes:

1. **Criação do contêiner de build**: o primeiro stage usa Buildah para criar uma imagem Docker com todas as dependências
2. **Uso do contêiner**: stages subsequentes rodam dentro desse contêiner, garantindo um ambiente consistente
3. **Builds reproduzíveis**: o isolamento do contêiner garante os mesmos resultados em diferentes runners

Essa abordagem garante que os builds funcionem da mesma forma em qualquer runner do GitLab e oferece um ambiente controlado para processos de build complexos.

### Fontes de dependências integradas

A imagem de dependências CI do Lumi compila a stack forkada a partir de **fontes integradas no repositório** (sem clones externos):

- `lumi-babl/` (BABL)
- `lumi-gegl/` (GEGL)
- `lumi-gtk3/` (GTK3)

Esses diretórios são copiados para o contexto de build do contêiner e compilados no prefixo de dependências (normalmente `/opt/lumi-deps`). Isso mantém o CI reproduzível e garante que o build do AppImage use a mesma fonte da verdade do desenvolvimento local.

## Papel dos scripts shell

Jobs em `.gitlab-ci.yml` normalmente invocam comandos shell diretamente. Operações complexas costumam ser movidas para scripts separados armazenados no repositório.

O CI do Lumi usa scripts shell modulares para organizar a lógica de build:

**Exemplo de invocação de script:**
```yaml
script:
  - bash build/linux/appimage/lumi-goappimage.sh 2>&1 | tee appimage_creation.log
```

**Benefícios dessa abordagem:**
- **YAML limpo**: mantém o arquivo `.gitlab-ci.yml` focado na estrutura dos jobs
- **Manutenibilidade**: lógica complexa é mais fácil de depurar e modificar em scripts shell
- **Reutilização**: scripts podem ser usados em diferentes contextos ou ambientes
- **Modularidade**: diferentes aspectos do build podem ser separados em scripts focados

Isso mantém a configuração CI limpa enquanto permite processos de build sofisticados.

## Integração com sistemas de build

O Lumi usa **Meson** e **Ninja** para preparar e depois compilar o código.

Por exemplo:

```
script:
  - meson setup _build-${CI_RUNNER_TAG} -Dprefix="${LUMI_PREFIX}"
  - ninja -C _build-${CI_RUNNER_TAG}
  - ninja -C _build-${CI_RUNNER_TAG} install
```

Aqui:

- `meson setup` prepara o diretório de build e gera `build.ninja`
- `ninja` executa os comandos de build conforme definido

## Estrutura do sistema de build Meson

O sistema de build **Meson** usa um arquivo raiz `meson.build` colocado no diretório raiz do projeto. Esse arquivo define a configuração de build de nível superior e o ponto de entrada do processo de build.

- O `meson.build` raiz normalmente fica no mesmo diretório que `.gitlab-ci.yml`
- A partir daí, ele **cascateia recursivamente** para subdiretórios, cada um podendo ter seu próprio arquivo `meson.build`
- Esses arquivos de subdiretório definem targets, fontes, dependências e instruções de build relevantes para aquele diretório

## Variáveis de ambiente

Variáveis-chave no pipeline do Lumi incluem:

```yaml
variables:
  DEBIAN_FRONTEND: "noninteractive"  # Prevents interactive prompts
  DEB_VERSION: "trixie"              # Debian version for consistency
  CI_RUNNER_TAG: "x86_64"            # Architecture specification
```

**Variáveis específicas do job:**
```yaml
build-lumi:
  variables:
    COMPILER: "clang"                                           # Compiler selection
    LINKER: "lld"                                               # Linker selection
    LUMI_PREFIX: "${CI_PROJECT_DIR}/_install-${CI_RUNNER_TAG}"  # Installation path
    DEPS_PREFIX: "/opt/lumi-deps"                               # Prebuilt dependency prefix
    MESON_OPTIONS: "-Dpkgconfig.relocatable=true -Drelocatable-bundle=yes"  # Build configuration
```

Essas variáveis controlam o comportamento do build e garantem consistência entre diferentes stages e runners.

## Estrutura de exemplo

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

Nessa estrutura:

- O arquivo raiz `meson.build` configura o ambiente geral de build
- Arquivos `meson.build` em subdiretórios tratam dos detalhes de compilação de componentes ou módulos específicos
- Esse layout hierárquico mantém a lógica de build modular e fácil de manter

## Artifacts entre stages

Artifacts são arquivos gerados por jobs que são necessários em stages subsequentes:

```yaml
build-lumi:
  # ...job configuration...
  artifacts:
    paths:
      - "${LUMI_PREFIX}/"      # Installation files
      - _build-${CI_RUNNER_TAG}/meson-logs/meson-log.txt  # Build logs
```

## Stages do pipeline e dependências

O pipeline do Lumi consiste em três stages principais:

1. **Dependencies**: cria um ambiente de build conteinerizado com todas as ferramentas e bibliotecas necessárias
2. **Build Lumi**: compila o Lumi usando Meson e Ninja no ambiente preparado
3. **AppImage**: empacota a aplicação compilada em um formato AppImage distribuível

**Dependências entre stages:**
```yaml
build-lumi:
  needs: [deps-debian]  # Waits for dependency container

lumi-appimage:
  needs: [build-lumi] # Waits for application build
```

Cada stage só é executado depois que suas dependências são concluídas com sucesso, garantindo a ordem correta de build e a disponibilidade de artifacts.

## Nomes de jobs atuais

O `.gitlab-ci.yml` do Lumi atualmente define estes nomes de jobs:

- `deps-debian`
- `build-lumi`
- `lumi-appimage`

## Resumo

- `.gitlab-ci.yml` define a estrutura e a lógica do pipeline
- Jobs contêm comandos shell ou scripts externos
- Ferramentas como Meson e Ninja são usadas dentro de jobs como parte do processo de build

O Lumi usa GitLab CI para compilar automaticamente seu AppImage para plataformas baseadas em Debian. O pipeline compila dependências, compila o Lumi e depois empacota um AppImage.

Para detalhes no nível do código-fonte, use:

- `.gitlab-ci.yml` na raiz do repositório Lumi
- `build/linux/appimage/lumi-goappimage.sh`
- `build/linux/appimage/README-CI.md`

Para detalhes técnicos completos sobre o processo de build CI do Lumi, incluindo configuração de ambiente, arquitetura de scripts e solução de problemas, consulte [README-CI.md](https://gitlab.gnome.org/pixelmixer/lumi/-/blob/main/build/linux/appimage/README-CI.md).
