---
title: "GitLab CI"
type: docs
url: "hub/technical-guides/GitLab-CI"
---
CI(지속적 통합)는 변경 사항이 있을 때마다 코드를 자동으로 테스트, 구축 및 검증하는 방법입니다.

**GitLab**은 `.gitlab-ci.yml` 파일을 통해 내장된 CI/CD 기능을 제공합니다. 저장소의 루트에 있는 이 파일은 GitLab에게 프로젝트를 빌드하고 테스트하는 방법을 알려줍니다. 변경 사항이 푸시될 때마다 깨끗한 환경에서 실행되는 단계와 스크립트를 정의합니다.

이 문서에서는 `.gitlab-ci.yml` 파일의 역할, 쉘 ​​스크립트, Meson 및 Ninja와 같은 외부 도구를 포함하여 Lumi의 GitLab CI/CD 파이프라인이 작동하는 방식을 간략하게 설명합니다.

Lumi CI 빌드 프로세스에 대한 자세한 기술 문서는 저장소의 [README-CI.md](https://gitlab.gnome.org/pixelmixer/lumi/-/blob/main/build/linux/appimage/README-CI.md)을 참조하세요.

## GitLab CI/CD 기본 사항

CI는 `.gitlab-ci.yml`이라는 파일로 제어됩니다. 이 파일은 다음을 정의합니다.

- **스테이지**: 작업 그룹 순서(예: `build-this`, `build-that`, `package-up`)
- **작업**: 각 단계 내에서 실행할 개별 작업
- **스크립트**: 각 작업에 대해 실행되는 셸 명령
- **실행자**: GitLab이 파이프라인에 정의된 작업을 실행하는 데 사용하는 컴퓨터입니다.

Lumi의 파이프라인 단계는 다음과 같습니다.

- `dependencies`
- `build lumi`
- `appimage`

## 컨테이너 기반 빌드

Lumi 파이프라인은 일관된 빌드를 위해 컨테이너화를 사용합니다.

1. **빌드 컨테이너 생성**: 첫 번째 단계에서는 Buildah를 사용하여 모든 종속성이 포함된 Docker 이미지를 생성합니다.
2. **컨테이너 사용**: 후속 단계가 이 컨테이너 내부에서 실행되어 일관된 환경을 보장합니다.
3. **재현 가능한 빌드**: 컨테이너 격리를 통해 다양한 실행자 간에 동일한 결과가 보장됩니다.

이 접근 방식은 빌드가 모든 GitLab 실행기에서 동일한 방식으로 작동하도록 보장하고 복잡한 빌드 프로세스를 위한 제어된 환경을 제공합니다.

## 쉘 스크립트의 역할

`.gitlab-ci.yml`의 작업은 일반적으로 셸 명령을 직접 호출합니다. 복잡한 작업은 저장소에 저장된 별도의 스크립트로 이동되는 경우가 많습니다.

Lumi CI는 모듈식 셸 스크립트를 사용하여 빌드 로직을 구성합니다.

**스크립트 호출의 예:**
```yaml
script:
  - bash build/linux/appimage/lumi-goappimage.sh 2>&1 | tee appimage_creation.log
```

**이 접근 방식의 이점:**
- **Clean YAML**: 작업 구조에 초점을 맞춘 `.gitlab-ci.yml` 파일을 유지합니다.
- **유지관리성**: 쉘 스크립트에서 복잡한 로직을 디버그하고 수정하기가 더 쉽습니다.
- **재사용성**: 스크립트는 다양한 컨텍스트나 환경에서 사용될 수 있습니다.
- **모듈화**: 빌드의 다양한 측면을 집중된 스크립트로 분리할 수 있습니다.

이는 정교한 빌드 프로세스를 허용하면서 CI 구성을 깨끗하게 유지합니다.

## 빌드 시스템과 통합

Lumi는 **Meson** 및 **Ninja**를 사용하여 코드를 준비하고 빌드합니다.

예를 들면:

```
script:
  - meson setup _build-${CI_RUNNER_TAG} -Dprefix="${LUMI_PREFIX}"
  - ninja -C _build-${CI_RUNNER_TAG}
  - ninja -C _build-${CI_RUNNER_TAG} install
```

여기:

- `meson setup`은 빌드 디렉터리를 준비하고 `build.ninja`을 생성합니다.
- `ninja`은 정의된 대로 빌드 명령을 실행합니다.

## Meson 빌드 시스템 구조

**Meson** 빌드 시스템은 프로젝트의 루트 디렉터리에 있는 루트 `meson.build` 파일을 사용합니다. 이 파일은 빌드 프로세스의 최상위 빌드 구성과 진입점을 정의합니다.

- 루트 `meson.build`은 일반적으로 `.gitlab-ci.yml`과 동일한 디렉터리에 있습니다.
- 거기서부터 하위 디렉터리로 **반복적으로** 계단식으로 배열되며, 각 하위 디렉터리에는 자체 `meson.build` 파일이 있을 수 있습니다.
- 이러한 하위 디렉터리 파일은 해당 디렉터리와 관련된 대상, 소스, 종속성 및 빌드 지침을 정의합니다.

## 환경 변수

Lumi 파이프라인의 주요 변수는 다음과 같습니다.

```yaml
variables:
  DEBIAN_FRONTEND: "noninteractive"  # Prevents interactive prompts
  DEB_VERSION: "trixie"              # Debian version for consistency
  CI_RUNNER_TAG: "x86_64"            # Architecture specification
```

**직업별 변수:**
```yaml
build-lumi:
  variables:
    COMPILER: "clang"                                           # Compiler selection
    LINKER: "lld"                                               # Linker selection
    LUMI_PREFIX: "${CI_PROJECT_DIR}/_install-${CI_RUNNER_TAG}"  # Installation path
    DEPS_PREFIX: "/opt/lumi-deps"                               # Prebuilt dependency prefix
    MESON_OPTIONS: "-Dpkgconfig.relocatable=true -Drelocatable-bundle=yes"  # Build configuration
```이러한 변수는 빌드 동작을 제어하고 다양한 단계와 실행기에서 일관성을 보장합니다.

## 예제 구조

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

이 구조에서:

- 루트 `meson.build` 파일은 전체 빌드 환경을 구성합니다.
- 하위 디렉터리 `meson.build` 파일은 특정 구성 요소 또는 모듈에 대한 컴파일 세부 정보를 처리합니다.
- 이 계층적 레이아웃은 빌드 로직을 모듈화하고 유지 관리 가능하게 유지합니다.

## 단계 간 아티팩트

아티팩트는 후속 단계에 필요한 작업에 의해 생성된 파일입니다.

```yaml
build-lumi:
  # ...job configuration...
  artifacts:
    paths:
      - "${LUMI_PREFIX}/"      # Installation files
      - _build-${CI_RUNNER_TAG}/meson-logs/meson-log.txt  # Build logs
```

## 파이프라인 단계 및 종속성

Lumi 파이프라인은 세 가지 주요 단계로 구성됩니다.

1. **종속성**: 필요한 모든 도구와 라이브러리를 포함하여 컨테이너화된 빌드 환경을 만듭니다.
2. **Build Lumi**: 준비된 환경에서 Meson 및 Ninja를 사용하여 Lumi를 컴파일합니다.
3. **AppImage**: 빌드된 애플리케이션을 배포 가능한 AppImage 형식으로 패키징합니다.

**단계 종속성:**
```yaml
build-lumi:
  needs: [deps-debian]  # Waits for dependency container

lumi-appimage:
  needs: [build-lumi] # Waits for application build
```

각 단계는 종속성이 성공적으로 완료된 후에만 실행되므로 적절한 빌드 순서와 아티팩트 가용성이 보장됩니다.

## 현재 직업 이름

Lumi `.gitlab-ci.yml`은 현재 다음 작업 이름을 정의합니다.

- `deps-debian`
- `build-lumi`
- `lumi-appimage`

## 요약

- `.gitlab-ci.yml`은 파이프라인의 구조와 논리를 정의합니다.
- 작업에는 셸 명령이나 외부 스크립트가 포함되어 있습니다.
- Meson 및 Ninja와 같은 도구는 빌드 프로세스의 일부로 작업 내부에서 사용됩니다.

Lumi는 GitLab CI를 사용하여 Debian 기반 플랫폼용 AppImage를 자동으로 구축합니다. 파이프라인은 종속성을 구축하고 Lumi를 컴파일한 다음 AppImage를 패키징합니다.

소스 수준 세부정보를 보려면 다음을 사용하세요.

- Lumi 저장소 루트의 `.gitlab-ci.yml`
- `build/linux/appimage/lumi-goappimage.sh`
- `build/linux/appimage/README-CI.md`

환경 설정, 스크립트 아키텍처 및 문제 해결을 포함하여 Lumi CI 빌드 프로세스에 대한 포괄적인 기술 세부 정보는 [README-CI.md](https://gitlab.gnome.org/pixelmixer/lumi/-/blob/main/build/linux/appimage/README-CI.md)을 참조하세요.