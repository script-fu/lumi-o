---
title: "디버그 버전 빌드"
type: docs
url: "hub/technical-guides/folder/Building-a-Debug-Version"
---
이 가이드에서는 `build/lumi/scripts`의 스크립트를 사용하여 Lumi의 **로컬 디버그 워크플로**를 설명합니다.

워크플로는 다음을 위해 설계되었습니다.

- 로컬 빌드 아티팩트 사용(기호 다운로드 필요 없음)
- 디버그 기호가 실제로 존재하는지 확인합니다.
- 기본적으로 오프라인 기호 모드로 GDB를 시작합니다.

## 전제 조건

- Debian 기반 Linux(프로젝트 기준: Debian 13)
- 이미 복제된 Lumi 소스 트리

## 일회성 GDB 설정(선택사항이지만 권장됨)

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

참고: Lumi의 로컬 디버그 스크립트는 기호 해상도를 로컬에서 재현 가능하게 유지하기 위해 기본적으로 `debuginfod`을 비활성화합니다.

## 빠른 시작

스크립트 디렉토리에서:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
```

### 디버그 빌드 + 실행(기본값)

일반적인 디버깅 세션에 사용하세요.

```bash
bash lumi-debug-local.sh lumi-dev build
```

이 명령은 다음과 같습니다.

1. 디버그 모드에서 Lumi를 빌드합니다.
2. 디버그 기호를 확인합니다.
3. GDB에서 Lumi를 시작합니다.

### 디버그 빌드 전용(나중 TTY/원격 세션용)

지금 빌드하고 나중에 실행/디버그하려는 경우 이 기능을 사용하세요.

```bash
bash lumi-build-debug.sh lumi-dev build
```

## Linux에서 TTY 사용하기

TTY(텍스트 콘솔)는 강제 정지를 디버깅하는 가장 안정적인 방법인 경우가 많습니다.

- `Ctrl + Alt + F1`을 통해 `Ctrl + Alt + F6`을 사용하여 TTY로 전환합니다.
- 텍스트 프롬프트에서 로그인
- `Ctrl + Alt + F7`(또는 일부 시스템에서는 `F2`)을 사용하여 그래픽 세션으로 돌아갑니다.

이것이 중요한 이유: 데스크톱 세션이 중단된 경우에도 TTY가 계속 응답하는 경우가 많으므로 GDB를 연결하고 역추적을 캡처하고 유용한 충돌 데이터를 복구할 수 있습니다.

## 선택 사항: 원격/TTY 디버깅

강제 정지 또는 디스플레이 잠금의 경우 `gdbserver`을 사용하세요.

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash gdbserver.sh
```

그런 다음 TTY(정지 시나리오에 권장) 또는 다른 터미널에서:

```bash
gdb /home/mark/code/lumi-dev/bin/lumi-0.1
(gdb) target remote localhost:9999
(gdb) continue
```

로컬 GDB 실행의 경우(TTY가 아닌 경로):

```bash
bash lumi-debug-launch.sh --repo lumi-dev
```

## 성능 노트

디버그 빌드는 설계상 속도가 더 느립니다. 디버깅이 완료되면 더 빠른 빌드로 다시 전환하세요.

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# Full release reset of all major components
bash lumi-debug-reset-release.sh lumi-dev

# Optional faster local-only variant
bash lumi-build-script.sh --scope build --dir lumi-dev --type debugoptimized
```