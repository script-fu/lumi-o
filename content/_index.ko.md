---
title: "Linux용 오픈 소스 디지털 페인팅"
toc: false
type: docs
---
[![gui-01](/images/screens/gui-00.jpg)](/images/screens/gui-01.jpg)

Lumi-o는 레이어드 래스터 페인팅을 위해 설계되었으며 의도적으로 가늘고 안정적입니다. Wacom 태블릿 지원은 전문 일러스트레이션에 필요한 부드럽고 반응성이 뛰어난 입력을 제공하기 위해 광범위한 테스트를 거쳤습니다.

김프의 핵심 아키텍처를 기반으로 구축된 Lumi-o는 데이터 무결성이 최우선인 전용 페인팅 환경을 선호하여 범용 복잡성을 제거합니다. 고속 자동 저장 및 롤링 증분 저장 시스템은 흐름을 방해하지 않고 작업을 보호합니다.

지연 로딩은 고정된 메모리 한도를 제거하여 RAM 제한에 부딪치지 않고 크고 복잡한 프로젝트와 대규모 캔버스 크기를 관리할 수 있게 해줍니다.

Lumi-o는 Cinnamon 데스크탑을 사용하는 Debian 13에서 개발되었습니다. Windows에서 오는 경우 Debian을 설치하는 것이 첫 번째 단계입니다: [Linux 설치 방법](/hub/install-linux/).

{{< cards >}}
  {{< card link="hub/about/" title="에 대한" icon="information-circle" >}}
  {{< card link="hub/quick-start/" title="빠른 시작" icon="play" >}}
  {{< card link="hub/features/" title="특징" icon="book-open" >}}
{{< /cards >}}