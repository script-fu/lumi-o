---
title: "Cursor"
type: docs
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: d3a7764cd3f00336fdcbb4d09a47fc3cabf4c99767cd005ccdc2fafdfa119117
url: "hub/scripting/tools/visual-studio-code"
---

## Cursor란

[Cursor](https://cursor.com/)는 같은 창에서 AI 도움을 받는 코드 편집기입니다. Visual Studio Code를 바탕으로 하므로 구문 강조, 확장, 작업 공간, Git이 그대로 이어집니다. Lumi 스크립트에는 여기서 Cursor를 씁니다. 그 AI 도움이 훨씬 경제적이기 때문입니다.

- **구문 강조**: 강조는 코드에 색을 입힙니다. 오류가 더 잘 보이고, 구조가 읽히며, 복잡한 코드도 따라가기 쉽습니다.
- **확장**: Cursor는 Visual Studio Code 확장을 실행합니다. 언어, 테마, 도구 가운데 맞는 확장이 있습니다.
- **버전 관리**: Git이 들어 있습니다. [GitHub](https://github.com/)나 [GitLab](https://gitlab.com/) 같은 서비스로 변경을 추적하고 분기를 다루기 쉽습니다.
- **편집기 안의 AI**: 이미 열어 둔 파일 옆에서 검색, 리팩터, 작은 수정을 부탁할 수 있습니다. 그 도움의 비용은 평범한 스크립트 작업에 쓸 만큼 낮습니다.

### 처음 쓸 때 주요 기능

- **작업 공간**: Cursor는 여러 프로젝트를 "작업 공간"으로 모아, 관련된 파일과 폴더를 함께 둡니다. 이동과 프로젝트 관리가 쉬워집니다.

- **프로필**: 프로필로 프로젝트나 작업마다 다른 구성을 둘 수 있습니다. 웹 개발용과 Scheme용을 나누고, 각각 자신의 확장, 테마, 설정을 갖게 합니다.

### Cursor 시작하기

Cursor를 쓰려면:

- **다운로드하고 설치**: [Cursor 웹사이트](https://cursor.com/)에서 Linux 버전을 받습니다.

- **화면 살펴보기**: Cursor를 열면 여러 패널이 보입니다. 왼쪽 **활동 표시줄**에서 파일, 검색, Git, 확장으로 이동합니다.

- **파일이나 폴더 열기**: 프로젝트 폴더를 넣어 작업 공간을 마련하고, 현재 구성을 작업 공간 파일로 저장합니다.

작업 공간은 저장된 폴더 보기에 그 프로젝트의 편집기 설정을 더한 것입니다.

### 확장

- **확장 설치**: 활동 표시줄의 **확장** 아이콘을 눌러 Scheme에 유용한 도구를 찾아 설치합니다. 중심은 Allen Huang의 **"vscode-scheme"**입니다. 구문 강조와, Lumi 플러그인을 쓰는 언어인 Scheme의 기본 지원을 제공합니다.

언어 파일을 고쳐 Scheme 키워드나 Lumi 스크립트 관련 말을 더하면 확장을 자신에게 맞게 조정할 수 있습니다.

자신만의 보조 라이브러리를 돌보고 있다면, 함수 이름을 강조에 넣으면 이동과 리팩터가 빨라집니다.

### 마무리

검색, 리팩터, 빠른 수정이 쉽고 AI가 손 가까운 편집기를 쓰십시오. Cursor와 Scheme 확장이 여기서 쓰는 기본입니다.
