---
title: "Git"
type: docs
---
Użyj Git, aby śledzić zmiany we wtyczkach, wycofywać błędy i udostępniać kod pomiędzy maszynami.

## Po co organizować swój kod?

Gdy masz więcej niż jeden skrypt, spójna struktura folderów oszczędza czas i ułatwia kontrolę wersji.

## Konfigurowanie struktury folderów kodu

Jednym z najprostszych sposobów porządkowania projektów jest utworzenie dedykowanego **folderu z kodem** na komputerze lokalnym. Wewnątrz tego folderu możesz tworzyć podfoldery dla każdego projektu lub repozytorium. Oto zalecana struktura folderów:

```plaintext
/home/your-username/code/
  ├── project1/
  ├── project2/
  └── project3/
```

Każdy podfolder (np. `project1`) reprezentuje **repozytorium**, w którym będziesz przechowywać pliki i kod tego projektu.

## Co to jest repozytorium?

**Repozytorium** (lub **repo**) to zasadniczo folder z zawartością śledzoną przez Git. Kiedy tworzysz repozytorium lokalnie, inicjujesz Git w tym folderze, umożliwiając zapisanie wszelkich zmian w klonie online.

### Lokalne i zdalne repozytoria

- **Repozytorium lokalne**: Jest to repozytorium przechowywane na Twoim komputerze, w jednym z folderów Twojego projektu.
- **Remote Repo**: Wersja repozytorium przechowywana online (na przykład w GitLab lub GitHub).

## Korzystanie z Git i GitHub

Po utworzeniu struktury folderów możesz zainicjować Git i połączyć swoje lokalne projekty z GitHub. Aby rozpocząć, wykonaj następujące kroki:

### Podstawowe kroki korzystania z Git i GitHub

1. **Zainstaluj Gita**
2. **Utwórz konto GitHub**
3. **Utwórz puste repozytorium w GitHubie**
4. **Zainicjuj Git w swoim lokalnym projekcie**
5. **Połącz swoje lokalne repozytorium z GitHub**
6. **Przestaw swoje pliki**
7. **Zatwierdź zmiany**
8. **Prześlij zmiany do GitHub**
9. **Wyświetl swoje repozytorium online**

### 1. Zainstaluj Gita

Jeśli jeszcze nie zainstalowałeś Gita, możesz to zrobić w systemie Linux, używając:

```sh
sudo apt install git
```

### 2. Utwórz konto GitHub

Jeśli nie masz jeszcze konta, odwiedź [GitHub](https://github.com/), aby się zarejestrować. Po zarejestrowaniu możesz tworzyć repozytoria w GitHub, aby przechowywać swój kod online.

### 3. Utwórz puste repozytorium w GitHubie

1. **Zaloguj się do GitHub**: Przejdź do [GitHub](https://github.com/) i zaloguj się na swoje konto.
2. **Utwórz nowe repozytorium**:
   - Kliknij ikonę **+** w prawym górnym rogu i wybierz **Nowe repozytorium**.
   - Wprowadź nazwę repozytorium (np. `your-repository`).
   - W razie potrzeby dodaj opis.
   - Wybierz widoczność **Publiczną** lub **Prywatną**.
   - **Nie** inicjuj repozytorium za pomocą pliku README, `.gitignore` lub licencji (aby uniknąć konfliktów).
   - Kliknij **Utwórz repozytorium**.

### 4. Zainicjuj Git w swoim lokalnym projekcie

Aby rozpocząć śledzenie folderu projektu za pomocą Git, otwórz terminal, przejdź do folderu projektu i uruchom:

```sh
cd code/your/project/folder
git init
```

To polecenie inicjuje puste repozytorium Git w folderze projektu.

### 5. Połącz swoje lokalne repozytorium z GitHubem

Następnie połącz swoje lokalne repozytorium z GitHub. Po utworzeniu pustego repozytorium na GitHubie dodaj je jako zdalne do swojego lokalnego projektu:

```sh
cd code/your/project/folder
git remote add origin https://github.com/your-username/your-repository.git
```

Zastąp `your-username` i `your-repository` rzeczywistą nazwą użytkownika GitHub i nazwą repozytorium. To polecenie łączy Twój lokalny projekt ze zdalnym repozytorium w GitHub.

### 6. Przygotuj swoje pliki

Zanim będziesz mógł zapisać zmiany w Git, musisz powiedzieć Gitowi, które pliki zmieniłeś i chcesz zapisać. Nazywa się to „stagingiem” plików. Użyj następującego polecenia, aby przygotować wszystkie zmodyfikowane lub nowe pliki:

```sh
git add .
```To mówi Gitowi, aby śledził zmiany wprowadzone we wszystkich plikach w projekcie. Możesz także przygotować określone pliki, zastępując `.` nazwą pliku.

### 7. Zatwierdź zmiany

Po przygotowaniu następnym krokiem jest zapisanie (lub „zatwierdzenie”) zmian w lokalnym repozytorium Git. Podczas zatwierdzania powinieneś zawsze dołączyć komunikat opisujący wprowadzone zmiany. Na przykład:

```sh
git commit -m "Add new feature"
```

Flaga `-m` umożliwia napisanie wiadomości podsumowującej wprowadzone zmiany. Ta wiadomość pomoże Tobie i innym zrozumieć, co zostało zmodyfikowane w tym zatwierdzeniu.

### 8. Prześlij zmiany do GitHuba

Po lokalnym zatwierdzeniu zmian możesz teraz „wypchnąć” je do GitHuba, aby zaktualizować zdalne repozytorium. Uruchom następujące polecenie, aby przesłać zmiany:

```sh
git push -u origin main
```

Gałąź `main` jest domyślną gałęzią w GitHub, w której przechowywany jest kod, a to polecenie przesyła lokalne zmiany do zdalnego repozytorium, udostępniając je online.

### 9. Wyświetl swój kod w GitHubie

Po przesłaniu kodu do GitHub możesz wyświetlić swoje repozytorium w interfejsie internetowym GitHub. Powinieneś zobaczyć pliki z lokalnego repozytorium wraz z historią zatwierdzeń pokazującą wprowadzone zmiany.

## Wniosek

Organizując swój kod w dedykowane foldery i używając GitHuba do zarządzania repozytoriami i tworzenia ich kopii zapasowych, zapewnisz dobrą strukturę swoich projektów i będziesz łatwo dostępny. Kiedy już będziesz miał działającą wersję swojego kodu, wypchnij ją do GitHuba. Następnie można łatwo śledzić wszelkie zmiany za pomocą interfejsu internetowego GitHub lub programu Visual Studio Code, który wyróżnia zmodyfikowane linie. Takie podejście pozwala na dalsze udoskonalanie i rozszerzanie kodu bez utraty śledzenia postępu i zmian.

Git i platformy takie jak GitHub i GitLab to potężne narzędzia i chociaż mogą być skomplikowane, w Internecie dostępnych jest wiele zasobów, które pomogą Ci lepiej je zrozumieć. Jednym z najcenniejszych zasobów, jakie znalazłem, są pomocnicy AI, tacy jak ChatGPT. Możesz opisać, co musisz osiągnąć, a te narzędzia cierpliwie poprowadzą Cię przez proces krok po kroku.

## Słowniczek

Oto kilka typowych terminów, które możesz spotkać podczas pracy z Git i GitHub:- **Zatwierdź**: Migawka zmian w repozytorium. Każde zatwierdzenie zawiera komunikat opisujący, co zostało zmienione i tworzy zapis historyczny, do którego można się później odwołać lub do którego można wrócić.
- **Repozytorium (Repo)**: Zbiór plików i ich historia śledzona przez Git. Repozytoria mogą istnieć lokalnie na Twoim komputerze lub zdalnie na platformach takich jak GitHub. Każdy projekt jest zazwyczaj przechowywany we własnym repozytorium.
- **Zdalne**: Zdalne repozytorium to wersja Twojego projektu hostowana na platformie takiej jak GitHub. Lokalna wersja Twojego projektu na Twoim komputerze jest połączona z tym pilotem, dzięki czemu możesz przesyłać (wypychać) i pobierać (ściągać) zmiany.
- **Staging**: Proces przygotowania plików do zatwierdzenia. Kiedy przygotowujesz plik, mówisz Gitowi, że chcesz dołączyć go do następnego zatwierdzenia. Staging pozwala wybrać, które zmiany mają zostać uwzględnione w zatwierdzeniu.
- **Push**: czynność polegająca na wysłaniu zatwierdzonych zmian z lokalnego repozytorium do zdalnego repozytorium (np. GitHub), aby inne osoby mogły uzyskać dostęp do zaktualizowanej wersji Twojego kodu.
- **Wyciąganie**: Akt pobierania zmian ze zdalnego repozytorium w celu aktualizacji kopii lokalnej. Pobierasz zmiany, gdy chcesz zsynchronizować swoje lokalne repozytorium z najnowszą wersją z pilota.
- **Origin**: Domyślna nazwa zdalnego repozytorium przy pierwszym połączeniu lokalnego repozytorium ze zdalnym. Zwykle odnosi się do głównego adresu URL Twojego projektu w GitHub.