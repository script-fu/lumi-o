---
title: "Debian 설치"
type: docs
url: "hub/install-linux/Installing-Debian"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 1e79ae25c72fd6b2a9d31e1efe3019289f4b44d9230990f6874c0332de6c5f19
---

이 문서는 Lumi-o 개발용 운영 체제로 Debian Stable을 설치하는 과정을 설명합니다. 비슷한 환경을 구성하는 분에게도 도움이 될 수 있습니다.

Debian Stable을 선택한 이유는 Lumi가 예측 가능한 장기 플랫폼 위에서 안정적으로 빌드되도록 하기 위해서입니다. GIMP 개발은 Debian Testing을 대상으로 하므로, Debian Stable은 이와 밀접하게 맞는 기본 시스템입니다.

Lumi-o는 Debian과 Cinnamon(X11)에서 가장 잘 동작하며, 해당 환경에서 개발 및 테스트됩니다. Cinnamon은 Windows와 유사한 익숙한 데스크톱 워크플로를 제공하고, X11은 Lumi 개발에 가장 안정적인 환경입니다.

Windows에서 넘어온다면, 가장 큰 개념적 차이는 대부분의 소프트웨어 설치와 구성이 다운로드형 설치 프로그램이 아니라 패키지 관리자와 간단한 터미널 명령으로 이루어진다는 점입니다.

## 이 가이드는 누구를 위한 것인가

이 가이드는 Lumi 개발에 사용하는 Debian Stable 구성을 기록한 것입니다. 일반적인 Linux 설치 튜토리얼이 아닙니다.

다음과 같은 경우에 특히 유용합니다.

- Windows에서 넘어와 예측 가능한 Linux 환경을 원하는 아티스트
- 소스에서 Lumi를 빌드하는 개발자
- 직접 시스템 구성을 설계하기보다 검증된 작업 환경을 재현하려는 사용자

디스크 파티셔닝과 기본적인 명령줄 사용에 대한 지식이 있다고 가정합니다.

## 데이터 백업

Debian을 설치하기 전에 홈 디렉터리 전체를 외장 드라이브에 백업하세요. 보존하려는 추가 데이터 폴더도 포함합니다.

참고: Linux에서 `~`는 홈 디렉터리를 나타냅니다.

Git 저장소를 사용 중이라면, 중요한 변경 사항을 원격에 푸시해 두면 설치 후 쉽게 복원할 수 있습니다. 이 단계는 이미 Git을 사용하는 경우에만 해당됩니다.

## 파티션 만들기

기본 드라이브에 Debian용 공간을 확보합니다. GParted를 포함해 이 작업을 돕는 가이드와 도구가 많습니다. 환경에 따라 다음과 같이 할 수 있습니다.

- 듀얼 부팅을 위해 기존 Windows 파티션 축소
- 기존 Linux 파티션 재사용
- 새 Linux 파티션과 swap 파티션 준비

파티션 작업은 시스템마다 크게 다르므로, 확실하지 않다면 변경 전에 하드웨어별 가이드를 참고하세요.

## Debian 설치 USB 만들기

대상 파티션과 swap 공간이 이미 준비되어 있다고 가정합니다.

1. 공식 웹사이트에서 Debian ISO를 다운로드합니다: https://www.debian.org/
2. Windows에서는 BalenaEtcher로 ISO를 USB 드라이브에 씁니다.
3. Linux에서는 `dd` 같은 명령줄 도구로 부팅 가능 USB를 만듭니다.

## Debian 설치

1. USB 드라이브를 연결합니다.
2. 재시작하고 부팅 중 부트 메뉴 키(보통 `F2`, `F12`, `Esc`, `Del`)를 누릅니다.
3. USB 장치를 선택합니다.
4. 비그래픽 설치 프로그램을 선택합니다.
5. root 비밀번호는 비워 둡니다. 설치 프로그램이 사용자 계정에 sudo 권한을 부여합니다.
6. 수동으로 파티션을 설정합니다.

   - 파일 시스템: ext4(저널링)
   - Swap: 기존 swap 파티션
   - 마운트 지점: `/`
   - 레이블: `linux`
   - 호스트 이름: `user@hostname` 형식으로 표시되는 시스템 이름
   - 사용자 계정: 성명
   - 사용자 이름: 터미널 로그인 이름

7. 이 단계에서 데스크톱 환경을 선택할 수 있습니다. Lumi 권장 구성으로 **Cinnamon**을 선택하세요.
8. 설치를 완료하고 Debian Stable로 재부팅합니다.

## 시스템 설정

### 디스플레이 스케일링

Debian Stable은 현재, 특히 4K 디스플레이에서 소수 스케일링을 일관되게 처리하지 못합니다. 해상도를 낮추기보다 UI 요소를 직접 조정하세요.

권장 설정:

- 소수 디스플레이 스케일링 피하기
- 메뉴 → 글꼴 선택 → 글꼴 설정 → 텍스트 배율: `2.5`
- 데스크톱 글꼴: `14`
- 패널 → 사용자 지정 → 패널 높이: `60`
- 패널 모양 → 오른쪽 영역 기호 아이콘 크기: `48px`
- 마우스 및 터치패드 → 포인터 크기 조정
- 바탕화면(우클릭) → 사용자 지정 → 아이콘 크기 확대

Firefox 조정:

- 주소 표시줄 → `about:config`
- `layout.css.devPixelsPerPx`를 `1`로 설정

### 터미널

터미널 환경 설정:

1. 메뉴 → 터미널 → 편집 → 환경설정
2. 텍스트 → 초기 크기: `140 columns`, `40 rows`
3. 텍스트 → 사용자 지정 글꼴: `Monospace 10`
4. 색 → 내장 구성표 → Solarized Dark

### 도구 크기 조절용 Alt 키

Lumi에서 `Alt` + 오른쪽 클릭 드래그로 브러시 크기가 조절되지 않으면, 데스크톱이 Alt를 창 관리에 사용하고 있는 것입니다.

1. 시스템 메뉴에서 **창**을 검색하세요.
2. 창 → 동작 → 창을 이동할 때 사용할 특수 키 → **사용 안함**

이 변경 후 Lumi에서 도구 크기 조절에 `Alt` + 오른쪽 클릭 드래그가 작동해야 합니다.

## 데이터 복원

필요에 따라 백업 파일을 홈 디렉터리로 복원합니다. 예:

- `Backup/Home/Artwork` → `~/Artwork`
- `Backup/Home/code` → `~/code`
- `Backup/Home/Desktop` → `~/Desktop`
- `Backup/Home/.ssh` → `~/.ssh`
- `Backup/Home/.config/lumi` → `~/.config/lumi`

참고: `.`으로 시작하는 폴더는 Linux의 숨김 설정 디렉터리입니다.

## 선택 사항: Git 설정

Lumi를 빌드하거나 저장소를 복원할 계획이 있을 때만 필요합니다.

### Git 설치

```bash
sudo apt install git
```

사용자 정보를 설정합니다.

```bash
git config --global --edit
```

#### GitLab 액세스

GitLab 또는 GitHub 저장소 액세스를 복원합니다.

1. SSH 키 파일 권한 변경: `chmod 600 ~/.ssh/id_rsa`
2. SSH 에이전트에 키 추가: `ssh-add ~/.ssh/id_rsa`
3. 연결 테스트: `ssh -T git@ssh.gitlab.gnome.org` 또는 `ssh -T git@github.com`

각 저장소에서 원격을 가져온 뒤 로컬 브랜치를 맞춥니다.

```bash
git reset --hard remote-name/branch-name
git clean -df
```

`git status`로 저장소가 깨끗한지 확인하세요.

이제 데이터와 저장소가 복원된 새 OS가 준비되었습니다. 이 구성은 Lumi 개발에 사용하는 검증된 작업 환경을 반영하며, 필요에 따라 개별 워크플로에 맞게 조정할 수 있습니다.

## OS 설정 후 Lumi 빌드

Lumi 빌드 스크립트 위치:

`~/code/lumi-dev/build/lumi/scripts`.

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# Install dependencies once
sudo bash lumi-install-packages.sh

# First full setup build
bash lumi-build-script.sh --scope setup --dir lumi-dev

# Regular rebuild after code changes
bash lumi-build-script.sh --scope build --dir lumi-dev

# Quick compile path
bash lumi-build-script.sh --scope compile --dir lumi-dev

# Launch Lumi
bash lumi-launch-active.sh lumi-dev
```
