---
title: "빠른 시작"
type: docs
---
Lumi는 아직 출시되지 않았으며 개발 버전으로 제공됩니다.

이미 Linux를 사용 중이고 Lumi를 빠르게 실행하려면 GitLab 아티팩트의 최신 **개발 AppImage**를 사용하세요.

https://gitlab.gnome.org/pixelmixer/lumi-dev/-/artifacts

1. 최신 개발 AppImage 아티팩트 zip을 다운로드합니다.
2. 압축을 푼다.
3. `Lumi*.AppImage` 파일을 더블클릭하여 실행하세요.

AppImage는 이미 실행 가능해야 합니다. 그렇지 않은 경우 파일 권한에서 **프로그램으로 파일 실행 허용**을 활성화하거나 아래 터미널 방법을 사용하십시오.

```bash
chmod +x Lumi*.AppImage
./Lumi*.AppImage
```

## Linux에서 Wacom 설정

Lumi의 디지털 페인팅의 경우 일반적으로 간단한 **선형 압력 설정**이 가장 좋습니다.

- 태블릿 드라이버 압력 곡선을 선형으로 유지하십시오.
- 압력/입력 곡선을 Lumi 선형으로 유지합니다.
- 브러시 역학이 비선형일 수 있으므로 브러시 자체로 느낌을 형성합니다.

다음을 사용하여 Linux 드라이버 곡선을 확인하고 재설정할 수 있습니다.

```bash
xsetwacom --get "Wacom Intuos Pro L Pen stylus" PressureCurve
xsetwacom --set "Wacom Intuos Pro L Pen stylus" PressureCurve 0 0 100 100
```

실용적인 팁:

- Lumi는 현재 X11 결함을 방지하기 위해 문제가 있는 Wacom 패드/터치링 입력을 차단합니다. 대신 타블렛 버튼을 **상대** 브러시 크기 확대/축소에 매핑하세요.
- `Alt`을 사용한 브러시 크기 드래그가 작동하지 않는 경우 데스크탑이 `Alt`을 사용하여 창을 이동하고 있을 수 있습니다. 해당 창 관리자 바로가기를 `Super`으로 변경하거나 비활성화하세요.

소스 코드에서 작업하려면 [Technical Guides](/hub/technical-guides/) 및 [Installation](/hub/technical-guides/Installation/)으로 이동하세요.