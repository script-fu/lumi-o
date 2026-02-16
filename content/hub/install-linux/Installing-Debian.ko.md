---
title: "데비안 설치"
type: docs
url: "hub/install-linux/Installing-Debian"
---
이 문서에서는 Debian Stable을 Lumi·o 개발 운영 체제로 설치하는 데 사용되는 프로세스를 간략하게 설명합니다. 비슷한 환경을 설정하는 다른 사람들에게 유용할 수 있습니다.

Lumi는 예측 가능한 장기 플랫폼에서 안정적으로 구축하는 것을 목표로 하기 때문에 Debian Stable을 선택했습니다. GIMP 개발은 Debian Testing을 목표로 하여 Debian Stable을 긴밀하게 정렬된 기본 시스템으로 만듭니다.

Windows에서 오는 경우 주요 개념 변경은 대부분의 소프트웨어 설치 및 구성이 다운로드 가능한 설치 프로그램이 아닌 패키지 관리자 및 간단한 터미널 명령을 통해 수행된다는 것입니다.

## 이 가이드는 누구를 위한 것인가요?

이 가이드는 Lumi 개발에 사용되는 Debian Stable 설정을 문서화합니다. 일반적인 Linux 설치 튜토리얼이 아닙니다.

다음과 같은 경우에 가장 유용합니다.

- Windows에서 예측 가능한 Linux 설정을 원하는 아티스트
- 소스에서 Lumi를 빌드하는 개발자
- 자신만의 시스템 구성을 설계하는 것보다 알려진 작업 환경을 재현하는 것을 선호하는 사용자

디스크 파티셔닝 및 간단한 명령줄 사용법에 대한 기본 지식이 있다고 가정합니다.

## 데이터 백업

데비안을 설치하기 전에 외부 드라이브에 홈 디렉터리의 전체 백업을 만드세요. 보존하려는 추가 데이터 폴더를 포함합니다.

참고: Linux에서는 `~`이 홈 디렉터리를 나타냅니다.

Git 리포지토리를 사용하는 경우 중요한 변경 사항을 원본에 푸시하여 설치 후 쉽게 복원할 수 있습니다. 이 단계는 이미 Git을 사용하고 있는 경우에만 관련됩니다.

## 파티션 생성

기본 드라이브에 Debian용 공간을 만드세요. 이 단계에는 GParted를 포함하여 많은 가이드와 도구가 있습니다. 설정에 따라 다음을 수행할 수 있습니다.

- 듀얼 부팅을 위해 기존 Windows 파티션 축소
- 기존 Linux 파티션 재사용
- 새로운 Linux 준비 및 파티션 교체

확실하지 않은 경우에는 변경하기 전에 하드웨어별 가이드를 참조하세요. 파티셔닝 단계는 시스템마다 크게 다르기 때문입니다.


## 데비안 설치 USB 만들기

대상 파티션과 스왑 공간이 이미 존재한다고 가정합니다.

1. 공식 웹사이트에서 Debian ISO를 다운로드하세요: https://www.debian.org/
2. Windows에서는 BalenaEtcher를 사용하여 ISO를 USB 드라이브에 씁니다.
3. Linux에서는 `dd`과 같은 명령줄 도구를 사용하여 부팅 가능한 USB를 만듭니다.

## 데비안 설치

1. USB 드라이브를 삽입합니다.
2. 다시 시작하고 시작 중에 부팅 메뉴 키(일반적으로 `F2`, `F12`, `Esc` 또는 `Del`)를 누릅니다.
3. USB 장치를 선택합니다.
4. 그래픽이 아닌 설치 프로그램을 선택합니다.
5. 설치 프로그램이 사용자 계정에 sudo 액세스 권한을 부여하도록 메시지가 표시되면 루트 비밀번호를 비워 둡니다.
6. 수동으로 파티션 나누기:

   - 파일 시스템: ext4(저널링)
   - 스왑: 기존 스왑 파티션
   - 마운트 지점: `/`
   - 라벨 : `linux`
   - 호스트 이름: `user@hostname`으로 표시되는 시스템 이름
   - 사용자 계정: 성명
   - 사용자 이름 : 단말기 로그인 이름

7. 데스크탑 환경으로 **Cinnamon**을 선택합니다.
8. 설치를 완료하고 Debian Stable로 재부팅합니다.

## 시스템 설정

### 디스플레이 크기 조정

Debian Stable은 현재 특히 4K 디스플레이에서 부분 스케일링을 일관되지 않게 처리합니다. 디스플레이 해상도를 줄이는 대신 인터페이스 요소를 직접 조정하세요.

권장 조정:- 분수 디스플레이 스케일링을 피하십시오.
- 메뉴 → 글꼴 선택 → 글꼴 설정 → 텍스트 배율: `2.5`
- 데스크탑 글꼴: `14`
- 패널 → 사용자 정의 → 패널 높이: `60`
- 패널 모양 → 오른쪽 영역 기호 아이콘 크기: `48px`
- 마우스 및 터치패드 → 포인터 크기 조정
- 바탕화면(우클릭) → 사용자 정의 → 아이콘 크기 확대

Firefox 조정:

- 주소 표시줄 → `about:config`
- `layout.css.devPixelsPerPx`을 `1`으로 설정합니다.

### 터미널

터미널 기본 설정 구성:

1. 메뉴 → 터미널 → 편집 → 환경설정
2. 텍스트 → 초기 크기: `140 columns`, `40 rows`
3. 텍스트 → 맞춤 글꼴: `Monospace 10`
4. 색상 → 내장 구성표 → Solarized Dark

## 데이터 복원

필요에 따라 백업된 파일을 홈 디렉터리에 복원합니다. 예를 들면 다음과 같습니다.

- `Backup/Home/Artwork` → `~/Artwork`
- `Backup/Home/code` → `~/code`
- `Backup/Home/Desktop` → `~/Desktop`
- `Backup/Home/.ssh` → `~/.ssh`
- `Backup/Home/.config/lumi` → `~/.config/lumi`

참고: `.`으로 시작하는 폴더는 Linux의 숨겨진 구성 디렉터리입니다.

## 선택사항: Git 설정

Lumi를 빌드하거나 리포지토리를 복원하려는 경우에만 필요합니다.

### Git 설치

```bash
sudo apt install git
```

귀하의 신원을 구성하십시오:

```bash
git config --global --edit
```

#### GitLab 액세스

GitLab 또는 GitHub에 대한 저장소 액세스를 복원합니다.

1. SSH 키 파일에 대한 권한을 변경합니다: `chmod 600 ~/.ssh/id_rsa`
2. 새 Git 설치에 사용자를 추가합니다: `ssh-add ~/.ssh/id_rsa`
3. 연결 테스트: `ssh -T git@ssh.gitlab.gnome.org` 또는 `ssh -T git@github.com`

각 저장소에 대해 원본을 가져오고 일치하도록 로컬 분기를 재설정합니다.

```bash
git reset --hard remote-name/branch-name
git clean -df
```

`git status`을 실행하여 저장소가 깨끗한지 확인하세요.

이제 모든 데이터와 리포지토리가 복원된 새로운 OS가 생겼습니다. 이 설정은 Lumi 개발에 사용되는 알려진 작업 환경을 반영하며 필요에 따라 개별 작업 흐름에 맞게 조정할 수 있습니다.

## OS 설치 후 Lumi 빌드

Lumi 빌드 스크립트는 다음 위치에 있습니다.

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