---
title: "Linux용 오픈 소스 디지털 페인팅"
toc: false
type: docs
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 4f453bf4100140e9a2779d4b9806a0ef8f27e68af8a9b6f680d01a6a5d2f2341
---
[![gui-01](/images/screens/gui-00.jpg)](/images/screens/gui-01.jpg)

Lumi-o는 레이어 기반 래스터 페인팅을 위해 설계된, 의도적으로 가볍고 안정적인 소프트웨어입니다. Wacom 태블릿 지원은 전문 일러스트 작업에 필요한 부드럽고 반응성 높은 입력을 제공합니다.

Lumi의 팔레트 기반 색상 시스템은 실제 안료 프로필을 스펙트럼 방식으로 혼합합니다. 시각화된 색상 관계를 바탕으로 팔레트를 구성하고, 명도를 조절하며, 색상에 이름을 붙여 정리할 수 있습니다. 팔레트 맵과 팔레트 믹서는 그림이나 프로젝트 전반에서 색상 선택의 일관성을 유지하는 데 도움이 됩니다.

GIMP의 핵심 아키텍처 위에 구축된 Lumi-o는 아티스트가 설계하고 다듬었습니다. 도구는 접근하기 쉬우면서도 고급 워크플로에 필요한 사용자 정의를 제공합니다. 빠른 불러오기와 저장, 완전히 통합된 자동 저장 시스템이 작업 흐름을 방해하지 않고 작업물을 보호합니다.

브러시 스트로크는 캘리그래피를 포함해 결과 요철이 있는 표면 질감을 받을 수 있습니다. 새 스트로크나 스트로크 중간의 방향 전환은 그 결을 어긋나게 하므로, 왔다 갔다 하며 넣는 음영은 방향을 되돌릴 때마다 새 자리에서 쌓입니다. 강모 브러시는 세밀하게 형태를 잡을 수 있고, 강모는 스트로크를 따라 움직이며 새로 납니다. OpenGL은 이미지 창에서 레이어 스택을 그리고, 도구 프리셋마다 스트로크를 칠할 수 있습니다. 큰 에어브러시와 큰 텍스처 스탬프는 훨씬 빨라지고, 캔버스 이동도 부드럽게 유지됩니다.

지연 로딩을 통해 Lumi는 훨씬 적은 RAM으로 크고 복잡한 프로젝트를 열 수 있습니다. 그룹이 접혀 있을 때 Lumi는 저장된 합성 이미지를 불러오고, 레이어, 마스크, 중첩된 그룹은 그룹이 다시 펼쳐질 때까지 가벼운 자리 표시자로 남습니다.

Lumi-o는 X11 위의 Cinnamon과 함께 Debian 13에서 개발됩니다. Wayland 위의 GNOME에서는 애플리케이션의 전반적인 모습과 OpenGL 렌더링을 확인했습니다. Windows에서 넘어온다면 Debian 설치가 첫 번째 단계입니다. [리눅스를 설치하는 방법](/hub/install-linux/)을 참조하세요.

{{< cards >}}
  {{< card link="hub/about/" title="소개" icon="information-circle" >}}
  {{< card link="hub/quick-start/" title="빠른 시작" icon="play" >}}
  {{< card link="hub/features/" title="기능" icon="book-open" >}}
{{< /cards >}}
