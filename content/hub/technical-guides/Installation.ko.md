---
title: "설치"
type: docs
---
아래의 초기 복제 단계에는 Git이 필요합니다. Git이 아직 설치되지 않은 경우 먼저 설치하거나(Debian/Ubuntu: `sudo apt install git`) 다음을 따르세요: [Using Git on Linux](/hub/technical-guides/Using-Git-on-Linux/)

## 1) 루미 복제(최초 설정)

Lumi용 디렉터리를 만들고 Git을 사용하여 소스 코드를 복제합니다.

```bash
sudo apt install git

mkdir -p ~/code
cd ~/code

# Clone via SSH (matches the Git guide above)
git clone git@ssh.gitlab.gnome.org:pixelmixer/lumi-dev.git lumi-dev

# Or clone via HTTPS (no SSH key setup)
# git clone https://gitlab.gnome.org/pixelmixer/lumi-dev.git lumi-dev
```

## 2) 종속성 설치(최초 설정)

```bash
cd ~/code/lumi-dev/build/lumi/scripts
sudo bash lumi-install-packages.sh
```

## 3) Lumi 빌드(최초 설정)

첫 번째 전체 설정 빌드(처음 또는 주요 변경 후):

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope setup --dir lumi-dev
```

## 4) 루미 실행

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-launch-active.sh lumi-dev
```

## 선택사항: 다시 빌드/컴파일

코드 변경 후 일반 재구축:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope build --dir lumi-dev
```

빠른 컴파일 전용 경로:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope compile --dir lumi-dev
```

단일 통합 구성 요소를 구축합니다(`babl`을 `gegl` 또는 `gtk3`으로 대체):

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope build --dir lumi-dev --component babl
```

## 선택사항: 빌드 유형

필요한 경우 `--type`을 사용하세요.

- `debug` – 디버깅 작업 흐름
- `debugoptimized` – 개발을 위한 균형 잡힌 기본값
- `release` – 가장 빠른 런타임

예:

```bash
bash lumi-build-script.sh --scope build --dir lumi-dev --type release
```