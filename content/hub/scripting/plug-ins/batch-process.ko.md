---
title: "일괄 처리"
type: docs
---
한 번에 많은 파일을 처리하기 위한 실용적인 엔드투엔드 예제입니다.

## 사는 곳

- [View the source](https://gitlab.gnome.org/pixelmixer/lumi-dev/-/blob/main/plug-ins/lumi/batch-process/batch-process.scm)

## 루미에 나타나는 위치

- **파일 → 일괄처리**

## 그것이 보여주는 것

- 소스/대상 디렉터리에 대한 `SF-DIRNAME` 매개변수
- 폴백을 사용하여 GUI 경로 유효성 검사(`validate-path-and-dir`)
- 재귀적 디렉터리 검색 및 반복
- 장기 실행 작업에 대한 진행 상황 보고