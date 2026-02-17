---
title: "Witaj świecie!"
type: docs
weight: 1
---
W tym samouczku omówiono minimalną strukturę wtyczki Scheme. Niektóre linie mają charakter szablonowy: są wymagane, aby Lumi załadował plik, nawet jeśli jeszcze ich w pełni nie rozumiesz.

```bash
# !/usr/bin/env lumi-scheme-interpreter-0.1
```

Na wysokim poziomie będziesz:

1. Zdefiniuj funkcję
2. Zarejestruj go tak, aby pojawił się w bazie danych procedur
3. (Opcjonalnie) Dodaj pozycję menu
4. Zainstaluj plik w folderze wtyczek

### Zdefiniuj funkcję

Funkcja, znana również jako _procedura_, to fragment kodu o nazwie i przeznaczeniu. Pobiera dane wejściowe i generuje dane wyjściowe.

**Wejście** > **_Funkcja_** > **Wyjście**

### Zarejestruj funkcję

Rejestracja polega na umieszczeniu nazwy funkcji na liście, aby Lumi o niej wiedziała.

```scheme
(scheme-register-procedure "scheme-hello-world"...
```

### Link do menu

Dzięki temu Lumi znajdzie swoją funkcję w systemie menu.

```scheme
(scheme-menu-register "scheme-hello-world" "<Image>/Funky")
```

Spowoduje to wyświetlenie menu „Funky” na pasku menu głównego. Zmień ścieżkę, aby umieścić wtyczkę w innym miejscu. Ścieżka `<Image>/Funky` oznacza, że ​​wtyczka pojawi się w kategorii menu **Obraz**. Możesz zmienić `<Image>` na `<Tools>`, `<Filters>` itd., w zależności od tego, gdzie chcesz, aby wtyczka się pojawiała.

### Komentarze

W Scheme, podstawowym języku Scheme, komentarze zazwyczaj umieszcza się poprzedzając pomocny wiersz tekstu znakiem `;;`. To, jak będziesz używać komentarzy, będzie zależeć od Twojej biegłości jako programisty — jeśli kodujesz okazjonalnie, więcej komentarzy będzie pomocne. Jeśli kodujesz przez cały czas, kod jest równie łatwy do odczytania, jak komentarz. Ponadto podczas programowania funkcjonalnego kod staje się wystarczająco opisowy, aby można go było czytać jak skrypt.

### Składnia

Kod ma zazwyczaj niewiele zasad dotyczących umieszczania elementów w wierszu, dzięki czemu możemy łatwo odczytać wiersz. Na przykład zdanie może zawierać spację po przecinku lub kropce. Pomaga to w czytelności.

Kod może organizować rzeczy w podobny sposób, co na pierwszy rzut oka może wyglądać dziwnie:

```scheme
(define (function-name input-a
                       input-b
                       input-c))
```

## Przykładowy kod

Oto pełny przykład. Większość procedur Lumi ma przedrostek `lumi-`. Na przykład `lumi-message` drukuje ciąg znaków do skonfigurowanej procedury obsługi wiadomości.

```scheme
# !/usr/bin/env lumi-scheme-interpreter-0.1

(define (scheme-hello-world)

  ;; Set the message handler to output the message to a GUI dialog box
  (lumi-message-set-handler 0)
  (lumi-message "Hello world!\n")

  ;; Set the message handler to output the message to the Error Console
  (lumi-message-set-handler 2)
  (lumi-message "Hello world!\n")

  ;; Send the message to the terminal, the OS window that launched Lumi
  (display "Hello world!\n"))


(scheme-register-procedure "scheme-hello-world"
  "Hello world!"
  "A Scheme procedure plug-in"
  "Mark Sweeney"
  "Under GNU GENERAL PUBLIC LICENSE Version 3"
  "2024")

(scheme-menu-register
  "scheme-hello-world"
  "<Image>/Funky")
```

### Zainstaluj wtyczkę

1. Przejdź do **Lumi -> Edycja -> Preferencje -> Foldery -> Wtyczki**.
2. Dodaj swój folder wtyczek [repo](/hub/scripting/tools/git) do listy.
3. Utwórz folder dla wtyczki i zapisz powyższy przykładowy kod jako `hello-world.scm`:
  - `your-plug-ins-repo/hello-world/hello-world.scm`
4. Kliknij prawym przyciskiem myszy plik `hello-world.scm`.
5. Przejdź do **Właściwości -> Uprawnienia -> Zezwalaj na wykonywanie pliku jako programu**.
6. Uruchom ponownie Lumi.

### Wypróbuj wtyczkę

Wtyczka powinna teraz pojawić się w menu „Funky” w głównym oknie Lumi. Kliknij go, a powinien wyświetlić się komunikat „Hello world!” wiadomość. Spróbuj zmodyfikować kod, na przykład zmienić tekst wiadomości, i zapisz plik. Po ponownym uruchomieniu wtyczki wprowadzone zmiany zostaną odzwierciedlone bez ponownego uruchamiania Lumi.

Spróbuj poeksperymentować, zmieniając ścieżkę menu. Na przykład `"<Image>/File"` umieści go w menu Plik, a `"<Image>/File/Funky"` utworzy nową sekcję w menu Plik. Jest to świetny sposób na dostosowanie miejsca wyświetlania wtyczki i uporządkowanie narzędzi.