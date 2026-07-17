---
title: "AppImage"
type: docs
url: "hub/technical-guides/AppImage"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 939beb6f4f1657ab77f785d1753385360cca7920b6291dbcd09f4687bfdfc502
---

AppImage는 단일 파일 Linux 애플리케이션 패키지입니다. 파일 하나를 다운로드해 실행 가능으로 표시한 뒤, 시스템 전체 설치 없이 실행할 수 있습니다.

공식 AppImage 사이트: https://appimage.org/

AppImage는 설치나 시스템 수정 없이 실행되는 Lumi의 휴대용 버전을 제공합니다. 종속성 관리, 소스 코드 컴파일, 개발 환경 구성 없이 바로 소프트웨어를 사용하려는 아티스트에게 이상적입니다.

자체 포함 실행 파일로 AppImage는 시스템 어디에나 저장할 수 있습니다. 새 릴리스 테스트, 여러 버전 보관, 컴퓨터 간 이동이 쉬워집니다.

Lumi 개발 과정에서 AppImage는 지속적 통합 출력과 밀접하게 일치하는 휴대용 테스트 빌드로 작동합니다. 로컬 소스 빌드를 개발 작업에 집중시키면서 일관된 환경에서 안정적인 테스트가 가능합니다.

참고: CI는 Lumi의 저장소 내 통합 종속성 소스(BABL/GEGL/GTK3)로 AppImage를 빌드하므로, 종속성 스택은 로컬 `lumi-build-script.sh` 워크플로와 일치합니다.

## 릴리스 vs 개발 AppImage

- **릴리스 AppImage**: 아직 제공되지 않습니다(Lumi는 아직 출시되지 않았습니다).
- **개발 AppImage(CI 아티팩트)**: 테스트를 위해 진행 중인 개발 커밋에서 자동 생성됩니다.

이 가이드는 주로 **개발 AppImage** 워크플로를 다룹니다.

현재 아티팩트 페이지:

https://gitlab.gnome.org/pixelmixer/lumi-dev/-/artifacts

## CI AppImage 다운로드 기본

CI는 아티팩트 zip 파일(예: `lumi-appimage*.zip`)을 생성합니다.

기본 수동 절차:

1. 최신 CI 아티팩트 zip 다운로드
2. 압축 해제
3. 포함된 `Lumi*.AppImage` 파일 실행

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
  - `~/Downloads`에서 최신 `lumi-appimage*.zip` 찾기
  - AppImage를 `~/AppImage/Lumi/Lumi_CI.AppImage`에 배치
  - 데스크톱 리소스를 `~/.local/share/applications/lumi.desktop`에 배치

- `lumi-appimage-launch.sh`
  - 터미널에서 AppImage 실행
  - 런타임 출력 활성화(`APPIMAGE_DEBUG=1`)

## 일반 참고 사항

- AppImage를 수동으로(도우미 스크립트 없이) 실행하는 경우, 먼저 실행 가능하게 만드세요:

```bash
chmod +x ~/AppImage/Lumi/Lumi_CI.AppImage
```

`lumi-appimage-unpack-zip.sh`는 실행 권한을 자동으로 적용합니다.

- Lumi가 다른 빌드에서 이미 실행 중이면 AppImage를 시작하기 전에 종료하세요.
