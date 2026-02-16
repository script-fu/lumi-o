---
title: "설치"
type: docs
url: "hub/technical-guides/folder/Installation"
---
이 가이드에서는 다음에서 현재 Lumi 빌드 스크립트를 사용합니다.

`~/code/lumi-dev/build/lumi/scripts`

## 1) 종속성 설치(최초 설정)

```bash
cd ~/code/lumi-dev/build/lumi/scripts
sudo bash lumi-install-packages.sh
```

## 2) 루미 빌드

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# First full setup build (BABL + lumi-gtk3 + GEGL + Lumi)
bash lumi-build-script.sh --scope setup --dir lumi-dev

# Normal rebuild after code changes
bash lumi-build-script.sh --scope build --dir lumi-dev

# Quick compile-only path
bash lumi-build-script.sh --scope compile --dir lumi-dev
```

## 3) 루미 실행

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-launch-active.sh lumi-dev
```

## 빌드 유형

필요한 경우 `--type`을 사용하세요.

- `debug` – 디버깅 작업 흐름
- `debugoptimized` – 개발을 위한 균형 잡힌 기본값
- `release` – 가장 빠른 런타임

예:

```bash
bash lumi-build-script.sh --scope build --dir lumi-dev --type release
```