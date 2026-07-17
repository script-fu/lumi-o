---
title: "GitLab CI"
type: docs
url: "hub/technical-guides/GitLab-CI"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 9917cebc417adeeae24d91b05b919b679a397d5db652cf4442d4330c0f8eeea5
---
A Integração Contínua (CI) é uma forma de testar, compilar e validar automaticamente o código sempre que são feitas alterações.

O **GitLab** fornece funcionalidades integradas de CI/CD através do ficheiro `.gitlab-ci.yml`. Este ficheiro, colocado na raiz do repositório, indica ao GitLab como compilar e testar o projecto. Define fases e scripts executados num ambiente limpo sempre que são enviadas alterações.

Este documento descreve o funcionamento do pipeline GitLab CI/CD do Lumi, incluindo o papel do ficheiro `.gitlab-ci.yml`, scripts shell e ferramentas externas como Meson e Ninja.

Para documentação técnica detalhada do processo de compilação CI do Lumi, consulte [README-CI.md](https://gitlab.gnome.org/pixelmixer/lumi/-/blob/main/build/linux/appimage/README-CI.md) no repositório.

## Noções básicas de CI/CD do GitLab

A CI é controlada por um ficheiro chamado `.gitlab-ci.yml`. Este ficheiro define:

- **Fases**: grupos ordenados de jobs (por exemplo, `build-this`, `build-that`, `package-up`)
- **Jobs**: tarefas individuais a executar em cada fase
- **Scripts**: comandos shell executados para cada job
- **Runners**: computadores que o GitLab usa para executar jobs definidos no pipeline.

No Lumi, as fases do pipeline são:

- `dependencies`
- `build lumi`
- `appimage`

## Compilações baseadas em contentores

O pipeline do Lumi usa contentorização para compilações consistentes:

1. **Criar o contentor de compilação**: a primeira fase usa Buildah para criar uma imagem Docker com todas as dependências
2. **Usar o contentor**: fases subsequentes correm dentro deste contentor, garantindo um ambiente consistente
3. **Compilações reproduzíveis**: o isolamento do contentor garante os mesmos resultados em diferentes runners

Esta abordagem garante que as compilações funcionam da mesma forma em qualquer runner GitLab e fornece um ambiente controlado para processos de compilação complexos.

### Fontes de dependências integradas

A imagem de dependências CI do Lumi compila a pilha derivada a partir de **fontes integradas no repositório** (não clones externos):

- `lumi-babl/` (BABL)
- `lumi-gegl/` (GEGL)
- `lumi-gtk3/` (GTK3)

Estes directórios são copiados para o contexto de compilação do contentor e compilados no prefixo de dependências (normalmente `/opt/lumi-deps`). Isto mantém a CI reproduzível e garante que a compilação do AppImage usa a mesma fonte de verdade que o desenvolvimento local.

## Papel dos scripts shell

Os jobs em `.gitlab-ci.yml` invocam normalmente comandos shell directamente. Operações complexas são frequentemente movidas para scripts separados armazenados no repositório.

A CI do Lumi usa scripts shell modulares para organizar a lógica de compilação:

**Exemplo de invocação de script:**
```yaml
script:
  - bash build/linux/appimage/lumi-goappimage.sh 2>&1 | tee appimage_creation.log
```

**Benefícios desta abordagem:**
- **YAML limpo**: mantém o ficheiro `.gitlab-ci.yml` focado na estrutura dos jobs
- **Manutenção**: lógica complexa é mais fácil de depurar e modificar em scripts shell
- **Reutilização**: scripts podem ser usados em contextos ou ambientes diferentes
- **Modularidade**: diferentes aspectos da compilação podem ser separados em scripts focados

Isto mantém a configuração CI limpa, permitindo processos de compilação sofisticados.

## Integração com sistemas de compilação

O Lumi usa **Meson** e **Ninja** para preparar e depois compilar o código.

Por exemplo:

```
script:
  - meson setup _build-${CI_RUNNER_TAG} -Dprefix="${LUMI_PREFIX}"
  - ninja -C _build-${CI_RUNNER_TAG}
  - ninja -C _build-${CI_RUNNER_TAG} install
```

Aqui:

- `meson setup` prepara o directório de compilação e gera `build.ninja`
- `ninja` executa os comandos de compilação conforme definido

## Estrutura do sistema de compilação Meson

O sistema de compilação **Meson** usa um ficheiro raiz `meson.build` colocado no directório raiz do projecto. Este ficheiro define a configuração de compilação de nível superior e o ponto de entrada do processo de compilação.

- A raiz `meson.build` está normalmente no mesmo directório que `.gitlab-ci.yml`
- A partir daí, **propaga-se recursivamente** para subdirectórios, cada um dos quais pode ter o seu próprio ficheiro `meson.build`
- Estes ficheiros de subdirectório definem alvos, fontes, dependências e instruções de compilação relevantes para esse directório

## Variáveis de ambiente

As principais variáveis no pipeline do Lumi incluem:

```yaml
variables:
  DEBIAN_FRONTEND: "noninteractive"  # Evita prompts interactivos
  DEB_VERSION: "trixie"              # Versão Debian para consistência
  CI_RUNNER_TAG: "x86_64"            # Especificação de arquitectura
```

**Variáveis específicas do job:**
```yaml
build-lumi:
  variables:
    COMPILER: "clang"                                           # Selecção do compilador
    LINKER: "lld"                                               # Selecção do linker
    LUMI_PREFIX: "${CI_PROJECT_DIR}/_install-${CI_RUNNER_TAG}"  # Caminho de instalação
    DEPS_PREFIX: "/opt/lumi-deps"                               # Prefixo de dependências pré-compiladas
    MESON_OPTIONS: "-Dpkgconfig.relocatable=true -Drelocatable-bundle=yes"  # Configuração de compilação
```

Estas variáveis controlam o comportamento da compilação e garantem consistência entre fases e runners diferentes.

## Exemplo de estrutura

```
project-root/
├── .gitlab-ci.yml
├── meson.build              <-- Ficheiro Meson raiz
├── src/
│   ├── meson.build          <-- Ficheiro Meson de subdirectório
│   └── some_source.c
├── data/
│   ├── meson.build
│   └── icons/
```

Nesta estrutura:

- O ficheiro raiz `meson.build` configura o ambiente geral de compilação
- Os ficheiros `meson.build` de subdirectório tratam dos detalhes de compilação de componentes ou módulos específicos
- Este esquema hierárquico mantém a lógica de compilação modular e sustentável

## Artefactos entre fases

Artefactos são ficheiros gerados por jobs necessários em fases subsequentes:

```yaml
build-lumi:
  # ...configuração do job...
  artifacts:
    paths:
      - "${LUMI_PREFIX}/"      # Ficheiros de instalação
      - _build-${CI_RUNNER_TAG}/meson-logs/meson-log.txt  # Registos de compilação
```

## Fases e dependências do pipeline

O pipeline do Lumi consiste em três fases principais:

1. **Dependências**: cria um ambiente de compilação contentorizado com todas as ferramentas e bibliotecas necessárias
2. **Build Lumi**: compila o Lumi com Meson e Ninja no ambiente preparado
3. **AppImage**: empacota a aplicação compilada num formato AppImage distribuível

**Dependências de fase:**
```yaml
build-lumi:
  needs: [deps-debian]  # Aguarda o contentor de dependências

lumi-appimage:
  needs: [build-lumi] # Aguarda a compilação da aplicação
```

Cada fase só corre depois de as dependências concluírem com sucesso, garantindo a ordem correcta de compilação e a disponibilidade de artefactos.

## Nomes actuais dos jobs

O `.gitlab-ci.yml` do Lumi define actualmente estes nomes de job:

- `deps-debian`
- `build-lumi`
- `lumi-appimage`

## Resumo

- `.gitlab-ci.yml` define a estrutura e a lógica do pipeline
- Os jobs contêm comandos shell ou scripts externos
- Ferramentas como Meson e Ninja são usadas dentro dos jobs como parte do processo de compilação

O Lumi usa GitLab CI para compilar automaticamente o AppImage para plataformas baseadas em Debian. O pipeline compila dependências, compila o Lumi e depois empacota um AppImage.

Para detalhes ao nível do código-fonte, use:

- `.gitlab-ci.yml` na raiz do repositório Lumi
- `build/linux/appimage/lumi-goappimage.sh`
- `build/linux/appimage/README-CI.md`

Para detalhes técnicos abrangentes sobre o processo de compilação CI do Lumi, incluindo configuração do ambiente, arquitectura de scripts e resolução de problemas, consulte [README-CI.md](https://gitlab.gnome.org/pixelmixer/lumi/-/blob/main/build/linux/appimage/README-CI.md).
