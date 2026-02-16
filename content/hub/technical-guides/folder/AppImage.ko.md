---
title: "앱이미지"
type: docs
url: "hub/technical-guides/folder/AppImage"
---
AppImage는 단일 파일 Linux 애플리케이션 패키지입니다. 하나의 파일을 다운로드하고 실행 가능으로 표시한 다음 시스템 전체에 소프트웨어를 설치하지 않고도 실행할 수 있습니다.

공식 앱 이미지 사이트: https://appimage.org/

AppImage는 설치나 시스템 수정 없이 실행되는 Lumi의 휴대용 버전을 제공합니다. 종속성을 관리하거나 소스 코드를 컴파일하거나 개발 환경을 구성하지 않고도 소프트웨어를 즉시 사용하려는 아티스트에게 이상적입니다.

자체 포함 실행 파일인 AppImage는 시스템의 어느 곳에나 저장할 수 있습니다. 이를 통해 새 릴리스를 쉽게 테스트하고, 여러 버전을 유지하고, 컴퓨터 간에 소프트웨어를 이동할 수 있습니다.

Lumi의 개발 프로세스에서 AppImage는 지속적인 통합 출력과 밀접하게 일치하는 휴대용 테스트 빌드로 작동합니다. 이를 통해 로컬 소스 빌드를 개발 작업에 집중시키는 동시에 일관된 환경에서 안정적인 테스트가 가능합니다.

## 출시 vs 개발 AppImage

- **AppImage 출시**: 아직 사용할 수 없습니다(Lumi는 출시되지 않았습니다).
- **개발 AppImage(CI 아티팩트)**: 테스트를 위해 진행 중인 개발 커밋에서 자동으로 생성됩니다.

이 가이드에서는 주로 **AppImage 개발** 워크플로를 다룹니다.

현재 이슈 페이지:

https://gitlab.gnome.org/pixelmixer/lumi-dev/-/artifacts

## CI AppImage 다운로드 기본 사항

CI는 아티팩트 zip 파일(예: `lumi-appimage*.zip`)을 생성합니다.

기본 수동 흐름:

1. 최신 CI 아티팩트 zip을 다운로드합니다.
2. 추출하세요.
3. 포함된 `Lumi*.AppImage` 파일을 실행합니다.

아래 스크립트는 이러한 단계를 자동화하는 선택적 도우미입니다.

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# Unpack latest downloaded CI zip from ~/Downloads
bash lumi-appimage-unpack-zip.sh

# Launch AppImage with terminal output
bash lumi-appimage-launch.sh
```

## 선택적 도우미 스크립트

- `lumi-appimage-unpack-zip.sh`
  - `~/Downloads`에서 최신 `lumi-appimage*.zip`을 찾습니다.
  - AppImage를 `~/AppImage/Lumi/Lumi_CI.AppImage`에 설치합니다.
  - `~/.local/share/applications/lumi.desktop`에 데스크톱 리소스를 설치합니다.

- `lumi-appimage-launch.sh`
  - 터미널에서 AppImage를 실행합니다.
  - 런타임 출력 활성화(`APPIMAGE_DEBUG=1`)

## 공통 참고 사항

- AppImage를 수동으로 실행하는 경우(도우미 스크립트 없이) 먼저 실행 가능하게 만듭니다.

```bash
chmod +x ~/AppImage/Lumi/Lumi_CI.AppImage
```

`lumi-appimage-unpack-zip.sh`은 이미 실행 권한을 자동으로 적용하고 있습니다.

- Lumi가 이미 다른 빌드에서 실행 중이라면 AppImage를 시작하기 전에 Lumi를 닫으세요.