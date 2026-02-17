---
title: "Wtyczka filtrująca"
type: docs
weight: 2
---
W samouczku [First Step](../../first-step/) użyliśmy wtyczki _procedure_. Tego typu wtyczki działają bez potrzeby wprowadzania obrazu lub możliwości rysowania jako danych wejściowych. Zwykle używamy wtyczki do zmiany obrazu i jego rysunków. Wtyczki takie jak te nazywane są wtyczkami _filter_.

### Co to jest rysunkowy?

**Możliwość rysowania** w Lumi odnosi się do elementu obrazu, na którym można rysować, takiego jak warstwa lub kanał. Wtyczki filtrujące zazwyczaj działają na tych elementach.

### Przykład prostej wtyczki filtrującej

```scheme
# !/usr/bin/env lumi-scheme-interpreter-0.1

(define (scheme-simple-filter-plug-in image drawables)
  ;; Use a let statement to define a message variable and core code
  (let ((message "hello, world"))
    ;; Display the message in Lumi's error console
    (lumi-message message)
    ;; Invert the colors of the first selected drawable
    (lumi-drawable-invert (vector-ref drawables 0) 1)))

;; Register the plug-in
(scheme-register-filter
  "scheme-simple-filter-plug-in"           ;; Main procedure name
  "Simple Filter Plug-in Demo"             ;; The name as it appears in the Lumi menu
  "Tests a basic Scheme filter plug-in"    ;; Tool-tip description
  "Author Name"                            ;; Give yourself some credit
  "License"                                ;; License
  "Date written"                           ;; Date written
  "*"                                      ;; Indicates this plug-in requires an image
  SF-ONE-OR-MORE-DRAWABLE)                 ;; Requires one or more selected drawables

;; Specify the menu location for the plug-in
(scheme-menu-register
  "scheme-simple-filter-plug-in"
  "<Image>/Plug-in")
```

Skopiuj tekst i zapisz go jako `simple-filter-plug-in.scm` w folderze o nazwie `simple-filter-plug-in` w jednym z folderów wtyczek Lumi. Folder wtyczek Lumi to _dowolny_ folder wymieniony w:
 **Lumi > Edytuj > Preferencje > Foldery > Wtyczki**

W systemie Linux kliknij prawym przyciskiem myszy plik `simple-filter-plug-in.scm`, przejdź do **Właściwości > Uprawnienia** i zaznacz **Zezwalaj na wykonywanie pliku jako programu**. Gdy plik znajdzie się we właściwym miejscu, będzie wykonywalny i wolny od błędów składniowych, po ponownym uruchomieniu Lumi pojawi się on w górnym pasku nagłówka menu, wewnątrz menu o nazwie **Wtyczka**.

### Uruchamianie wtyczki

1. Otwórz obraz (ta wtyczka filtrująca wymaga do działania obrazu).
2. Otwórz **Windows > Dokowalne okna dialogowe > Konsola błędów**, aby zobaczyć komunikat.
3. Z menu **Wtyczka** wybierz opcję **Prosta wersja demonstracyjna wtyczki filtrującej**.
4. Kolory jednej z wybranych warstw zostaną odwrócone, a w konsoli błędów zostanie wyświetlony komunikat.

### Edytowanie wtyczki

Możesz dostosować wtyczkę, edytując jej plik `.scm`. Na przykład, aby zmienić wyświetlany komunikat:

1. Otwórz plik i znajdź linię definiującą `message`.
2. Zastąp `"hello, world"` swoim niestandardowym tekstem.
3. Zapisz plik.

W Lumi w wersji 3 wtyczki nie wymagają odświeżania, aby zapisane zmiany zaczęły obowiązywać. Aby zobaczyć zaktualizowany komunikat, wystarczy ponownie uruchomić wtyczkę.

### Badanie wtyczki

#### Linia Shebang

Pierwsza linijka gwarantuje, że skrypt będzie działać jako wtyczka w Lumi 3:

```scheme
# !/usr/bin/env lumi-scheme-interpreter-0.1
```

#### Definicja procedury

Procedura przyjmuje dwa argumenty: aktywny obraz i wybrane rysunki.

```scheme
(define (scheme-simple-filter-plug-in image drawables)
```

#### Podstawowa logika

Instrukcja `let` definiuje zmienną i wykonuje operacje na rysunku.

```scheme
(let ((message "hello, world"))
  (lumi-message message) ;; Displays a message in Lumi's error console
  (lumi-drawable-invert (vector-ref drawables 0) 1)) ;; Inverts the colors of the first selected drawable
```

### Rejestracja wtyczki

Wtyczka jest zarejestrowana w Lumi jako wtyczka filtrująca:

```scheme
(scheme-register-filter
  "scheme-simple-filter-plug-in"           ;; Register the main procedure
  "Simple Filter Plug-in Demo"             ;; The name as it appears in the Lumi menu
  "Tests a basic Scheme filter plug-in"    ;; Tool-tip description
  "Author Name"                            ;; Author's name
  "License"                                ;; License type
  "Date written"                           ;; Date written
  "*"                                      ;; Indicates the plug-in requires an image
  SF-ONE-OR-MORE-DRAWABLE)                 ;; Requires one or more selected drawables
```

#### Rejestracja menu
Ta linia określa lokalizację menu wtyczki:

```scheme
(scheme-menu-register
  "scheme-simple-filter-plug-in"
  "<Image>/Plug-in")
```

### Rozwiązywanie problemów

Jeśli wtyczka nie zostanie wyświetlona, sprawdź jej lokalizację, nazwę i właściwości pliku wykonywalnego.

Lokalizacja musi znajdować się na ścieżce wyszukiwania wtyczki.
Nazwa pliku musi być zgodna z nazwą folderu zawierającego.
Plik musi być ustawiony jako wykonywalny.


**Konsola błędów** to cenne narzędzie do rozwiązywania problemów z niestandardowymi wtyczkami. Jeśli Twoja wtyczka nie zachowuje się zgodnie z oczekiwaniami, sprawdź tutaj komunikaty o błędach lub dzienniki. Okno **Terminalu** może również udostępniać informacje dotyczące debugowania i raportować problemy z ładowaniem.