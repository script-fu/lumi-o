---
title: "설치"
type: docs
url: "hub/technical-guides/Installation"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: ff429321515ea8c3b77a6f1f0cfd2486c8042e168032b9b0bec97b497930e25e
---

아래 초기 복제 단계에는 Git이 필요합니다. Git이 아직 설치되지 않았다면 먼저 설치하세요(Debian/Ubuntu: `sudo apt install git`). 또는 다음 가이드를 따르세요: [Linux에서 Git 사용](/hub/technical-guides/Using-Git-on-Linux/)

## 1) Lumi 복제(최초 설정)

Lumi용 디렉터리를 만들고 Git으로 소스 코드를 복제합니다.

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

처음이거나 주요 변경 후의 첫 전체 설정 빌드:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope setup --dir lumi-dev
```

## 4) Lumi 실행

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-launch-active.sh lumi-dev
```

## 선택 사항: 다시 빌드 / 컴파일

코드 변경 후 일반 재빌드:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope build --dir lumi-dev
```

컴파일만 빠르게 수행:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope compile --dir lumi-dev
```

단일 통합 구성 요소 빌드(`babl`을 `gegl` 또는 `gtk3`으로 바꿈):

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope build --dir lumi-dev --component babl
```

## 선택 사항: 빌드 유형

필요할 때 `--type`을 사용하세요:

- `debug` – 디버깅 워크플로
- `debugoptimized` – 개발용 균형 잡힌 기본값
- `release` – 가장 빠른 실행 속도

예:

```bash
bash lumi-build-script.sh --scope build --dir lumi-dev --type release
```
