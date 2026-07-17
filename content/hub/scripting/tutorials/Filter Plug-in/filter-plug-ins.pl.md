---
title: "Wtyczka filtrująca"
type: docs
weight: 2
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: db9cfb794dad80ce918a3eca7b47d02b23dbbba960a26765c04d95d459d8ec6b
url: "hub/scripting/tutorials/Filter Plug-in/filter-plug-ins"
---
W samouczku [Pierwszy krok](../../first-step/) użyliśmy wtyczki _procedure_. Tego typu wtyczki działają bez potrzeby wprowadzania obrazu lub możliwości rysowania jako danych wejściowych. Zwykle używamy wtyczki do zmiany obrazu i jego rysunków. Wtyczki takie jak te nazywane są wtyczkami _filter_.

### Co to jest drawable?

**Drawable** w Lumi oznacza element obrazu, na którym można rysować, taki jak warstwa lub kanał. Wtyczki filtrujące zazwyczaj działają na tych elementach.

### Przykład prostej wtyczki filtrującej

```scheme
#!/usr/bin/env lumi-scheme-interpreter-0.1

(define (scheme-simple-filter-plug-in image drawables)
  ;; Użyj instrukcji let, aby zdefiniować zmienną wiadomości i kod główny
  (let ((message "hello, world"))
    ;; Wyświetl wiadomość w konsoli komunikatów Lumi
    (lumi-message message)
    ;; Odwróć kolory pierwszego wybranego drawable
    (lumi-drawable-invert (vector-ref drawables 0) 1)))

;; Rejestruje wtyczkę
(scheme-register-filter
  "scheme-simple-filter-plug-in"           ;; Nazwa procedury głównej
  "Simple Filter Plug-in Demo"             ;; Nazwa wyświetlana w menu Lumi
  "Tests a basic Scheme filter plug-in"    ;; Opis podpowiedzi
  "Author Name"                            ;; Podziękuj sobie za pracę
  "License"                                ;; Licencja
  "Date written"                           ;; Data napisania
  "*"                                      ;; Wskazuje, że ta wtyczka wymaga obrazu
  SF-ONE-OR-MORE-DRAWABLE)                 ;; Wymaga co najmniej jednego wybranego drawable

;; Określ lokalizację menu dla wtyczki
(scheme-menu-register
  "scheme-simple-filter-plug-in"
  "<Image>/Plug-in")
```

Skopiuj tekst i zapisz go jako `simple-filter-plug-in.scm` w folderze o nazwie `simple-filter-plug-in` w jednym z folderów wtyczek Lumi. Folder wtyczek Lumi to _dowolny_ folder wymieniony w:
 **Lumi > Edytuj > Preferencje > Foldery > Wtyczki**

W systemie Linux kliknij prawym przyciskiem myszy plik `simple-filter-plug-in.scm`, przejdź do **Właściwości > Uprawnienia** i zaznacz **Zezwalaj na wykonywanie pliku jako programu**. Gdy plik znajdzie się we właściwym miejscu, będzie wykonywalny i wolny od błędów składniowych, po ponownym uruchomieniu Lumi pojawi się on w górnym pasku nagłówka menu, wewnątrz menu o nazwie **Wtyczka**.

### Uruchamianie wtyczki

1. Otwórz obraz (ta wtyczka filtrująca wymaga do działania obrazu).
2. Otwórz **Narzędzia → Debugowanie → Konsola wiadomości**, aby zobaczyć komunikat.
3. Z menu **Wtyczka** wybierz opcję **Simple Filter Plug-in Demo**.
4. Kolory jednej z wybranych warstw zostaną odwrócone, a w konsoli komunikatów zostanie wyświetlony komunikat.

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
#!/usr/bin/env lumi-scheme-interpreter-0.1

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
  (lumi-message message) ;; Wyświetla wiadomość w konsoli komunikatów Lumi
  (lumi-drawable-invert (vector-ref drawables 0) 1)) ;; Odwraca kolory pierwszego wybranego drawable
```

### Rejestracja wtyczki

Wtyczka jest zarejestrowana w Lumi jako wtyczka filtrująca:

```scheme
(scheme-register-filter
  "scheme-simple-filter-plug-in"           ;; Rejestruje procedurę główną
  "Simple Filter Plug-in Demo"             ;; Nazwa wyświetlana w menu Lumi
  "Tests a basic Scheme filter plug-in"    ;; Opis podpowiedzi
  "Author Name"                            ;; Imię i nazwisko autora
  "License"                                ;; Typ licencji
  "Date written"                           ;; Data napisania
  "*"                                      ;; Wskazuje, że wtyczka wymaga obrazu
  SF-ONE-OR-MORE-DRAWABLE)                 ;; Wymaga co najmniej jednego wybranego drawable
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


**Konsola wiadomości** to cenne narzędzie do rozwiązywania problemów z niestandardowymi wtyczkami. Jeśli Twoja wtyczka nie zachowuje się zgodnie z oczekiwaniami, sprawdź tutaj komunikaty o błędach lub dzienniki. Okno **Terminalu** może również udostępniać informacje dotyczące debugowania i raportować problemy z ładowaniem.