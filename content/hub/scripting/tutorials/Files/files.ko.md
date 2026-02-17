---
title: "파일"
type: docs
weight: 7
---
파일 및 디렉터리 작업은 구성표 개발에 필수적입니다. 출력을 저장하든, 리소스를 로드하든, 프로젝트 구조를 구성하든 파일 작업을 이해하면 스크립트가 더욱 강력하고 사용자 친화적이게 됩니다.

이 페이지에서는 경로 읽기, 디렉터리 생성, GUI 매개변수를 통한 폴더 입력 수집 등 일반적인 파일 및 디렉터리 작업을 다룹니다.

## 사용자의 홈 디렉토리

Lumi는 Linux 전용이므로 사용자의 홈 디렉터리는 `HOME` 환경 변수에서 나옵니다.

사용자의 홈 디렉토리를 문자열로 얻으려면:

```scheme
(getenv "HOME")
```

예제 출력:

```scheme
"/home/username"
```

## 디렉터리 구분 기호

플랫폼별 경로 구분 기호인 전역 변수 `DIR-SEPARATOR`도 있습니다. Lumi(Linux)에서는 항상 `/`입니다.

```scheme
> DIR-SEPARATOR
"/"
```

## 디렉토리 위치 얻기

플러그인에 대한 구성표 대화 상자에서 사용자에게 디렉토리 위치를 요청할 수 있습니다.

```scheme
(scheme-register
  "scheme-batch-process"
  "Batch Process"
  "Iteratively open the source files, then process, export and close"
  "Mark Sweeney"
  "Under GNU GENERAL PUBLIC LICENSE Version 3"
  "2025"
  ""
  SF-DIRNAME "Loca_tion of Source"       ""
  SF-DIRNAME "Location of _Destination"  ""
  SF-TOGGLE  "S_how Loaded Images"       0
  SF-TOGGLE  "Only Process Open I_mages" 0)
```

`SF-DIRNAME`은 디렉토리에 대한 브라우저를 제공합니다.

```scheme
(define (batch-process-file-system src-dir src-dir-fallback extension dst-dir dst-dir-fallback show-images process-fn export-fn)
  (let* ((validated-src-dir (validate-path-and-dir src-dir src-dir-fallback "Source"))
         (validated-dst-dir (validate-path-and-dir dst-dir dst-dir-fallback "Destination"))
         (files (discover-files validated-src-dir extension)))
    ;; ...
    ))
```

여기서는 두 디렉터리 입력(소스 및 대상)의 유효성을 검사하고 GUI 경로가 비어 있거나 유효하지 않은 경우 기본값으로 돌아갑니다.

[/hub/scripting/plug-ins/batch-process/](/hub/scripting/plug-ins/batch-process/)

구현 세부 사항에 관심이 있는 경우 `validate-path-and-dir`에 대한 플러그인 소스를 검색하세요.

## 디렉토리 만들기

Scheme은 디렉터리를 생성하기 위한 ```dir-make``` 명령을 제공합니다. 이 명령은 "/"로 구분된 경로를 사용하고 권한에 대한 선택적 매개변수가 있는 단일 디렉터리를 생성합니다. 우리는 플랫폼별 경로를 제공하지 않습니다.

일반적으로 실제 경로를 위해 여러 디렉터리를 만들어야 합니다. 여기서는 ```dir-make```에 대한 래퍼를 사용하여 도움을 받을 수 있습니다.

```scheme
;; Purpose: A wrapper for (dir-make) that creates a given path from a platform
;;          supplied path. Always emits Linux style separators for dir-make.
(define (make-dir-path path)
  (let* ((path-parts (strbreakup path DIR-SEPARATOR))
         (current-path (car path-parts))) ; Root dir
    ;; Create the rest of the directories step-by-step
    (for-each
     (lambda (part)
       (set! current-path (string-append current-path "/" part)) ; build the path
       (if (file-exists? current-path)
         (debug-message "Directory exists: " current-path)
         (if (dir-make current-path)
           (debug-message "Made directory: " current-path)
           (warning-message "Failed to make directory: " current-path))))
     (cdr path-parts))))
```

참고: 이 함수는 또한 내장된 ```file-exists?```을 사용하여 불필요한 호출을 건너뜁니다. 표시된 파일이나 디렉터리가 존재하면 #t를 반환하고, 존재하지 않거나 요청하는 사용자가 액세스할 수 없으면 #f를 반환합니다.

## 경로 구성

또한 Scheme에서 경로를 분해하고 다시 구축해야 합니다.

경로를 여러 부분으로 분할하려면 ```strbreakup```을 사용합니다.

### Linux 경로 예

```scheme
> (strbreakup (getenv "HOME") DIR-SEPARATOR)
("" "home" "username")

> (strbreakup "/this/path/" DIR-SEPARATOR)
("" "this" "path" "")
```

> 참고: 선행 및 후행 슬래시는 결과 목록에서 빈 문자열 요소가 됩니다.

경로를 다시 작성하려면 ```string-append```을 사용하십시오.

### Linux 경로 구축

```scheme
> (string-append (getenv "HOME") DIR-SEPARATOR "myfolder" DIR-SEPARATOR "myfile.xcf")
"/home/username/myfolder/myfile.xcf"
```
````