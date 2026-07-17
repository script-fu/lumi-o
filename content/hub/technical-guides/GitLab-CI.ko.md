---
title: "GitLab CI"
type: docs
url: "hub/technical-guides/GitLab-CI"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 9917cebc417adeeae24d91b05b919b679a397d5db652cf4442d4330c0f8eeea5
---

지속적 통합(CI)은 변경이 있을 때마다 코드를 자동으로 테스트, 빌드, 검증하는 방식입니다.

**GitLab**은 `.gitlab-ci.yml` 파일을 통해 내장 CI/CD 기능을 제공합니다. 이 파일을 저장소 루트에 두면 GitLab에 프로젝트를 빌드하고 테스트하는 방법을 알려줍니다. 변경을 푸시할 때마다 깨끗한 환경에서 실행되는 단계와 스크립트를 정의합니다.

이 문서에서는 `.gitlab-ci.yml` 파일, 셸 스크립트, Meson 및 Ninja 같은 외부 도구의 역할을 포함해 Lumi의 GitLab CI/CD 파이프라인이 어떻게 동작하는지 설명합니다.

Lumi CI 빌드 프로세스에 대한 자세한 기술 문서는 저장소의 [README-CI.md](https://gitlab.gnome.org/pixelmixer/lumi/-/blob/main/build/linux/appimage/README-CI.md)를 참조하세요.

## GitLab CI/CD 기본 사항

CI는 `.gitlab-ci.yml`이라는 파일로 제어됩니다. 이 파일은 다음을 정의합니다:

- **스테이지**: 순서가 있는 작업 그룹(예: `build-this`, `build-that`, `package-up`)
- **작업**: 각 스테이지 내에서 실행할 개별 작업
- **스크립트**: 각 작업에 대해 실행되는 셸 명령
- **러너**: 파이프라인에 정의된 작업을 실행하는 GitLab이 사용하는 컴퓨터

Lumi의 파이프라인 스테이지는 다음과 같습니다:

- `dependencies`
- `build lumi`
- `appimage`

## 컨테이너 기반 빌드

Lumi 파이프라인은 일관된 빌드를 위해 컨테이너화를 사용합니다:

1. **빌드 컨테이너 생성**: 첫 번째 스테이지에서 Buildah로 모든 종속성이 포함된 Docker 이미지를 만듭니다
2. **컨테이너 사용**: 후속 스테이지는 이 컨테이너 안에서 실행되어 환경이 일관됩니다
3. **재현 가능한 빌드**: 컨테이너 격리로 서로 다른 러너에서도 같은 결과를 보장합니다

이 방식은 어떤 GitLab 러너에서도 빌드가 같은 방식으로 동작하게 하고, 복잡한 빌드 프로세스를 제어된 환경에서 실행할 수 있게 합니다.

### 통합 종속성 소스

Lumi의 CI 종속성 이미지는 **저장소 내 통합 소스**(외부 클론이 아님)에서 포크된 스택을 빌드합니다:

- `lumi-babl/` (BABL)
- `lumi-gegl/` (GEGL)
- `lumi-gtk3/` (GTK3)

이 디렉터리는 컨테이너 빌드 컨텍스트에 복사되어 종속성 접두사(일반적으로 `/opt/lumi-deps`)에 컴파일됩니다. 이를 통해 CI 재현성을 유지하고 AppImage 빌드가 로컬 개발과 같은 소스를 사용하도록 보장합니다.

## 셸 스크립트의 역할

`.gitlab-ci.yml`의 작업은 보통 셸 명령을 직접 호출합니다. 복잡한 작업은 저장소에 있는 별도 스크립트로 옮기는 경우가 많습니다.

Lumi CI는 모듈식 셸 스크립트로 빌드 로직을 구성합니다.

**스크립트 호출 예:**
```yaml
script:
  - bash build/linux/appimage/lumi-goappimage.sh 2>&1 | tee appimage_creation.log
```

**이 방식의 장점:**
- **깔끔한 YAML**: `.gitlab-ci.yml`을 작업 구조에 집중시킬 수 있습니다
- **유지보수성**: 복잡한 로직은 셸 스크립트에서 디버그하고 수정하기 더 쉽습니다
- **재사용성**: 스크립트는 다른 컨텍스트나 환경에서도 사용할 수 있습니다
- **모듈성**: 빌드의 각 부분을 독립된 스크립트로 분리할 수 있습니다

이렇게 하면 CI 설정을 깔끔하게 유지하면서 정교한 빌드 프로세스를 구현할 수 있습니다.

## 빌드 시스템과의 통합

Lumi는 **Meson**과 **Ninja**로 코드를 준비하고 빌드합니다.

예:

```
script:
  - meson setup _build-${CI_RUNNER_TAG} -Dprefix="${LUMI_PREFIX}"
  - ninja -C _build-${CI_RUNNER_TAG}
  - ninja -C _build-${CI_RUNNER_TAG} install
```

여기서:

- `meson setup`은 빌드 디렉터리를 준비하고 `build.ninja`를 생성합니다
- `ninja`는 정의된 대로 빌드 명령을 실행합니다

## Meson 빌드 시스템 구조

**Meson** 빌드 시스템은 프로젝트 루트 디렉터리에 있는 루트 `meson.build` 파일을 사용합니다. 이 파일이 최상위 빌드 구성과 빌드 프로세스의 진입점을 정의합니다.

- 루트 `meson.build`는 보통 `.gitlab-ci.yml`과 같은 디렉터리에 있습니다
- 거기서 **재귀적으로** 하위 디렉터리로 이어지며, 각 하위 디렉터리에 자체 `meson.build`가 있을 수 있습니다
- 하위 디렉터리 파일은 해당 디렉터리와 관련된 대상, 소스, 종속성, 빌드 지침을 정의합니다

## 환경 변수

Lumi 파이프라인의 주요 변수는 다음과 같습니다:

```yaml
variables:
  DEBIAN_FRONTEND: "noninteractive"  # Prevents interactive prompts
  DEB_VERSION: "trixie"              # Debian version for consistency
  CI_RUNNER_TAG: "x86_64"            # Architecture specification
```

**작업별 변수:**
```yaml
build-lumi:
  variables:
    COMPILER: "clang"                                           # Compiler selection
    LINKER: "lld"                                               # Linker selection
    LUMI_PREFIX: "${CI_PROJECT_DIR}/_install-${CI_RUNNER_TAG}"  # Installation path
    DEPS_PREFIX: "/opt/lumi-deps"                               # Prebuilt dependency prefix
    MESON_OPTIONS: "-Dpkgconfig.relocatable=true -Drelocatable-bundle=yes"  # Build configuration
```

이 변수들은 빌드 동작을 제어하고 스테이지와 러너 간 일관성을 보장합니다.

## 구조 예

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

- 루트 `meson.build` 파일이 전체 빌드 환경을 구성합니다
- 하위 디렉터리 `meson.build` 파일이 특정 구성 요소나 모듈의 컴파일 세부 사항을 처리합니다
- 이 계층 구조는 빌드 로직을 모듈화하고 유지보수하기 쉽게 유지합니다

## 스테이지 간 아티팩트

아티팩트는 후속 스테이지에서 필요한, 작업이 생성한 파일입니다:

```yaml
build-lumi:
  # ...job configuration...
  artifacts:
    paths:
      - "${LUMI_PREFIX}/"      # Installation files
      - _build-${CI_RUNNER_TAG}/meson-logs/meson-log.txt  # Build logs
```

## 파이프라인 스테이지와 종속성

Lumi 파이프라인은 세 가지 주요 스테이지로 구성됩니다:

1. **Dependencies**: 필요한 모든 도구와 라이브러리를 갖춘 컨테이너화 빌드 환경을 만듭니다
2. **Build Lumi**: 준비된 환경에서 Meson과 Ninja로 Lumi를 컴파일합니다
3. **AppImage**: 빌드된 애플리케이션을 배포 가능한 AppImage 형식으로 패키징합니다

**스테이지 종속성:**
```yaml
build-lumi:
  needs: [deps-debian]  # Waits for dependency container

lumi-appimage:
  needs: [build-lumi] # Waits for application build
```

각 스테이지는 종속성이 성공적으로 완료된 후에만 실행되어 올바른 빌드 순서와 아티팩트 가용성을 보장합니다.

## 현재 작업 이름

Lumi `.gitlab-ci.yml`에서 현재 정의된 작업 이름은 다음과 같습니다:

- `deps-debian`
- `build-lumi`
- `lumi-appimage`

## 요약

- `.gitlab-ci.yml`이 파이프라인의 구조와 로직을 정의합니다
- 작업에는 셸 명령이나 외부 스크립트가 포함됩니다
- Meson과 Ninja 같은 도구는 빌드 프로세스의 일부로 작업 안에서 사용됩니다

Lumi는 GitLab CI를 사용해 Debian 기반 플랫폼용 AppImage를 자동으로 빌드합니다. 파이프라인은 종속성을 구축하고 Lumi를 컴파일한 뒤 AppImage를 패키징합니다.

소스 수준 세부 정보는 다음을 참조하세요:

- Lumi 저장소 루트의 `.gitlab-ci.yml`
- `build/linux/appimage/lumi-goappimage.sh`
- `build/linux/appimage/README-CI.md`

환경 설정, 스크립트 아키텍처, 문제 해결을 포함한 Lumi CI 빌드 프로세스의 포괄적인 기술 세부 정보는 [README-CI.md](https://gitlab.gnome.org/pixelmixer/lumi/-/blob/main/build/linux/appimage/README-CI.md)를 참조하세요.
