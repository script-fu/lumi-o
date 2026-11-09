---
title: "Linux 시스템 개요"
type: docs
url: "hub/install-linux/A-Linux-System-Overview"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: a38745c31470f2662ed8b220af2cc2065e981b8c1435fce2efae41886a2a7996
---

Linux는 강력하고 다재다능한 운영 체제이며, 방대한 개발자 커뮤니티를 갖고 있습니다. Linux 시스템의 핵심에는 원활한 사용자 경험을 위해 함께 동작하는 여러 주요 구성 요소가 있습니다. 이 개요에서는 커널, 배포판, 패키지 관리자, 디스플레이 관리자, 데스크톱 환경, 디스플레이 서버(X11 또는 Wayland) 등 Linux 시스템의 필수 요소를 설명합니다.

Lumi는 Debian과 Cinnamon(X11)에서 개발하고 테스트합니다. 그 환경이 여전히 개발 환경입니다. Wayland의 GNOME은 그와 나란히, 애플리케이션의 전반적인 모습과 OpenGL 렌더링을 확인했습니다.

**주요 Linux 배포판의 일반적인 기본값**

| **배포판** | **패키지 관리자** | **디스플레이 관리자** | **데스크톱 환경** | **디스플레이 서버** |
|--------------------|----------------------|----------------------|-------------------------|--------------------|
| Debian             | APT                  | GDM                  | GNOME                   | Wayland            |
| Ubuntu             | APT                  | GDM                  | GNOME                   | Wayland            |
| Debian             | APT                  | GDM                  | Cinnamon                | X11                |
| Fedora             | DNF                  | GDM                  | GNOME                   | Wayland            |
| Arch Linux         | Pacman               | 사용자 선택           | 사용자 선택              | 사용자 선택         |

### 주요 용어

#### 커널

하드웨어(보통 Linux)와 직접 상호작용하는 운영 체제의 핵심입니다.

#### 배포판

커널과 사용자 공간 도구, 라이브러리, 소프트웨어를 묶은 Linux 배포판입니다. 예: Debian, Arch Linux, Fedora.

#### 패키지 관리자

저장소에서 소프트웨어를 설치, 업데이트, 제거하는 도구입니다. 예: Debian 계열의 APT, Fedora의 DNF, Arch Linux의 Pacman.

#### 디스플레이 관리자

그래픽 로그인 화면과 세션 시작을 관리합니다. 예: GDM(GNOME Display Manager), LightDM, SDDM(Simple Desktop Display Manager).

#### 데스크톱 환경

그래픽 사용자 인터페이스(GUI)를 제공하고 전체적인 모양과 사용자 경험을 관리합니다. 예: GNOME, Cinnamon, KDE Plasma.

#### 디스플레이 서버

디스플레이 출력과 입력 이벤트를 관리합니다. 예: X11(X Window System)과 Wayland. X11은 전통적인 디스플레이 서버이고, Wayland는 더 새롭고 안전한 대안입니다.
