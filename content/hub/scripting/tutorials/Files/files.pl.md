---
title: "Pliki"
type: docs
weight: 7
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: a68dc9328daa1e5b96aee6bf0949a8454b7826df85bdae254502ad9a24864992
url: "hub/scripting/tutorials/files"
---
Praca z plikami i katalogami jest niezbędna do Scheme. Niezależnie od tego, czy zapisujesz dane wyjściowe, ładujesz zasoby, czy organizujesz strukturę projektu, zrozumienie operacji na plikach sprawi, że Twoje skrypty będą bardziej niezawodne i przyjazne dla użytkownika.

Na tej stronie opisano typowe zadania związane z plikami i katalogami: odczytywanie ścieżek, tworzenie katalogów i zbieranie danych wejściowych z folderów za pomocą parametrów GUI.

## Katalog domowy użytkownika

Lumi działa tylko na Linuksie, więc katalog domowy użytkownika pochodzi ze zmiennej środowiskowej `HOME`.

Aby uzyskać katalog domowy użytkownika jako ciąg znaków:

```scheme
(getenv "HOME")
```

Przykładowe wyjście:

```scheme
"/home/username"
```

## Separator katalogów

Istnieje również zmienna globalna `DIR-SEPARATOR`, która jest separatorem ścieżki specyficznym dla platformy. W Lumi (Linux) jest to zawsze `/`.

```scheme
> DIR-SEPARATOR
"/"
```

## Uzyskiwanie lokalizacji z katalogu

Możemy zapytać użytkownika o lokalizację katalogu w oknie dialogowym Scheme dla wtyczki.

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

`SF-DIRNAME` udostępnia przeglądarkę do katalogu.

```scheme
(define (batch-process-file-system src-dir src-dir-fallback extension dst-dir dst-dir-fallback show-images process-fn export-fn)
  (let* ((validated-src-dir (validate-path-and-dir src-dir src-dir-fallback "Source"))
         (validated-dst-dir (validate-path-and-dir dst-dir dst-dir-fallback "Destination"))
         (files (discover-files validated-src-dir extension)))
    ;; …
    ))
```

Tutaj sprawdzamy dwa wejścia katalogu (źródłowy i docelowy) i przywracamy wartości domyślne, jeśli ścieżki GUI są puste/nieprawidłowe.

[/hub/scripting/plug-ins/batch-process/](/hub/scripting/plug-ins/batch-process/)

Jeśli interesują Cię szczegóły implementacji, poszukaj w źródle wtyczki `validate-path-and-dir`.

## Tworzenie katalogu

Scheme udostępnia polecenie `dir-make` umożliwiające utworzenie katalogu. To polecenie przyjmuje ścieżkę oddzieloną znakiem „/” i tworzy pojedynczy katalog z opcjonalnym parametrem dla uprawnień. Nie nadajemy mu ścieżek specyficznych dla platformy.

Zwykle musimy utworzyć wiele katalogów dla praktycznej ścieżki. Możemy użyć opakowania dla `dir-make`, aby nam w tym pomóc.

```scheme
;; Cel: Opakowanie dla (dir-make), które tworzy podaną ścieżkę z poziomu platformy
;;          podanej ścieżki. Zawsze emituje separatory w stylu Linux dla dir-make.
(define (make-dir-path path)
  (let* ((path-parts (strbreakup path DIR-SEPARATOR))
         (current-path (car path-parts))) ; Katalog główny
    ;; Utwórz pozostałe katalogi krok po kroku
    (for-each
     (lambda (part)
       (set! current-path (string-append current-path "/" part)) ; Buduje ścieżkę
       (if (file-exists? current-path)
         (debug-message "Directory exists: " current-path)
         (if (dir-make current-path)
           (debug-message "Made directory: " current-path)
           (warning-message "Failed to make directory: " current-path))))
     (cdr path-parts))))
```

Uwaga: Ta funkcja wykorzystuje również wbudowane `file-exists?` do pomijania niepotrzebnych połączeń. Zwraca `#t`, jeśli wskazany plik lub katalog istnieje, i `#f`, jeśli nie istnieje lub nie jest dostępny dla żądającego użytkownika.

## Konstruowanie ścieżki

Musimy także rozbić i odbudować ścieżki w Scheme.

Aby podzielić ścieżkę na części, użyj `strbreakup`:

### Przykłady ścieżek do Linuksa

```scheme
> (strbreakup (getenv "HOME") DIR-SEPARATOR)
("" "home" "username")

> (strbreakup "/this/path/" DIR-SEPARATOR)
("" "this" "path" "")
```

> Uwaga: Ukośniki początkowe i końcowe stają się pustymi elementami ciągu na wynikowej liście.

Aby odbudować ścieżkę, użyj `string-append`:

### Tworzenie ścieżki do Linuksa

```scheme
> (string-append (getenv "HOME") DIR-SEPARATOR "myfolder" DIR-SEPARATOR "myfile.xcf")
"/home/username/myfolder/myfile.xcf"
```
```