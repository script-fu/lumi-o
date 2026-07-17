---
title: "일괄 처리"
type: docs
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 141b5ee23e77ecfc8ef5e8112706cfaebc1a2a528518218296dffda5b9dee6d1
---
한 번에 많은 파일을 처리하기 위한 실용적인 엔드투엔드 예제입니다.

## 소스 코드

- [소스 보기](https://gitlab.gnome.org/pixelmixer/lumi-dev/-/blob/main/plug-ins/lumi/batch-process/batch-process.scm)

## Lumi 메뉴

- **파일 → 일괄처리**

## 다루는 내용

- 소스/대상 디렉터리에 대한 `SF-DIRNAME` 매개변수
- 폴백을 사용하여 GUI 경로 유효성 검사(`validate-path-and-dir`)
- 재귀적 디렉터리 검색 및 반복
- 장기 실행 작업에 대한 진행 상황 보고
