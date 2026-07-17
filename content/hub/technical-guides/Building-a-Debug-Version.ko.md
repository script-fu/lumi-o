---
title: "디버그 버전 빌드"
type: docs
url: "hub/technical-guides/Building-a-Debug-Version"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: fecc781e73b4f30881c5150c958ae9b2df4164acd4cf86926186acb8e2021d5f
---

이 가이드는 `build/lumi/scripts`의 스크립트를 사용한 Lumi **로컬 디버그 워크플로**를 설명합니다.

이 워크플로는 다음을 위해 설계되었습니다:

- 로컬 빌드 아티팩트 사용(기호 다운로드 불필요)
- 디버그 기호가 실제로 포함되어 있는지 확인
- 기본적으로 오프라인 기호 모드로 GDB 시작

## 사전 요구 사항

- Debian 기반 Linux(프로젝트 기준: Debian 13)
- Lumi 소스 트리가 이미 복제됨

## 일회성 GDB 설정(선택 사항이지만 권장)

GDB 도구를 설치합니다:

```bash
sudo apt update
sudo apt install gdb gdbserver
```

선택적 로컬 로깅 설정:

```bash
mkdir -p ~/code/gdb_logs
cat > ~/.gdbinit <<'EOF'
set logging file ~/code/gdb_logs/gdb_log.txt
set logging enabled on
set logging overwrite on
EOF
```

참고: Lumi의 로컬 디버그 스크립트는 기호 해석을 로컬에서 재현 가능하게 유지하기 위해 기본적으로 `debuginfod`를 비활성화합니다.

## 빠른 시작

스크립트 디렉터리에서:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
```

### 디버그 빌드 + 실행(기본값)

일반적인 디버깅 세션에 사용합니다.

```bash
bash lumi-debug-local.sh lumi-dev build
```

이 명령은 다음을 수행합니다:

1. Lumi를 디버그 모드로 빌드
2. 디버그 기호 검증
3. GDB 아래에서 Lumi 실행

### 디버그 빌드만(나중 TTY/원격 세션용)

지금 빌드하고 나중에 실행/디버그하려면 이 방법을 사용하세요.

```bash
bash lumi-build-debug.sh lumi-dev build
```

## Linux에서 TTY 사용

TTY(텍스트 콘솔)는 하드 프리즈를 디버깅하는 가장 안정적인 방법인 경우가 많습니다.

- `Ctrl + Alt + F1`부터 `Ctrl + Alt + F6`까지 TTY로 전환
- 텍스트 프롬프트에서 로그인
- `Ctrl + Alt + F7`(일부 시스템에서는 `F2`)로 그래픽 세션으로 돌아가기

중요한 이유: 데스크톱 세션이 멈춰도 TTY는 종종 응답하므로 GDB를 연결하고 백트레이스를 캡처해 유용한 충돌 데이터를 확보할 수 있습니다.

## 선택 사항: 원격 / TTY 디버깅

하드 프리즈나 디스플레이 잠금에는 `gdbserver`를 사용하세요:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash gdbserver.sh
```

TTY(프리즈 상황에 권장) 또는 다른 터미널에서:

```bash
gdb /home/mark/code/lumi-dev/bin/lumi-0.1
(gdb) target remote localhost:9999
(gdb) continue
```

로컬 GDB 실행(비 TTY 경로):

```bash
bash lumi-debug-launch.sh --repo lumi-dev
```

## 성능 참고

디버그 빌드는 의도적으로 더 느립니다. 디버깅이 끝나면 더 빠른 빌드로 돌아가세요:

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# Full release reset of all major components
bash lumi-debug-reset-release.sh lumi-dev

# Optional faster local-only variant
bash lumi-build-script.sh --scope build --dir lumi-dev --type debugoptimized
```
