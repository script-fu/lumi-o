---
title: "Korzystanie z Gita w systemie Linux"
type: docs
url: "hub/technical-guides/folder/Using-Git-on-Linux"
---
Witamy w tym przewodniku dla początkujących dotyczącym korzystania z Git w systemie Linux! Ten przewodnik ma na celu pomóc Ci rozpocząć pracę z Git i GitLab oraz zapewnić podstawową wiedzę na temat korzystania z tych narzędzi.

## Przegląd Gita

Kod używany do tworzenia aplikacji jest przechowywany w zbiorze folderów i plików w systemie. Git to aplikacja, która pozwala nam tworzyć kopie zapasowe, udostępniać i kopiować tę kolekcję. Git jest znany jako system kontroli wersji, który pozwala śledzić zmiany w kodzie i współpracować z innymi. Jest to potężne narzędzie szeroko stosowane w społeczności open source. GitLab to platforma internetowa, która umożliwia hostowanie repozytoriów Git i zarządzanie nimi online, ułatwiając współpracę z innymi i śledzenie zmian w kodzie.

## Co to jest repozytorium?

_repo_, skrót od repozytorium, to lokalny folder zarządzany przez Git z kopią online. Repozytorium Git Lab to zbiór plików i folderów tworzących projekt. Może mieć _oddziały_, które są niezależnymi kopiami tego samego projektu. Oddział to osobna wersja Twojego projektu, która pozwala na wprowadzanie zmian bez wpływu na wersję główną. Jest to przydatne do testowania nowych funkcji lub naprawiania błędów bez zakłócania głównego projektu. Istnieje repozytorium lokalne, przechowywane na dysku twardym, oraz repozytorium zdalne, przechowywane online za pomocą Git i GitLab.

## Używanie Gita

Będziesz musiał zainstalować Git w swoim systemie. W systemach opartych na Debianie możesz użyć polecenia apt, aby zainstalować pakiety oprogramowania. W tym przypadku używamy go do instalacji Gita, czyli pakietu udostępniającego system kontroli wersji Git. Polecenie sudo daje instalatorowi pozwolenie na instalację w systemie.

```bash
 sudo apt install git
```

## Uzyskaj dostęp do GitLaba

Zanim będziesz mógł używać [GitLab](https://gitlab.com/users/sign_up), musisz utworzyć konto, odwiedzając witrynę GitLab i dopełniając proces rejestracji.

GitLab wymaga _SSH_ do bezpiecznej i uwierzytelnionej komunikacji pomiędzy klientem (na przykład Tobą) a serwerem GitLab podczas wykonywania operacji Git, takich jak _klonowanie_, _wypychanie_ i _pobieranie_ repozytoriów. Klonowanie polega na utworzeniu lokalnej kopii repozytorium, pobranie powoduje przeniesienie wszelkich zmian dokonanych w repozytorium do kopii lokalnej, a wypchnięcie polega na wysłaniu zmian i zawartości do repozytorium serwera. SSH (Secure Shell) to protokół sieciowy, który umożliwia bezpieczny dostęp zdalny i wykorzystuje _pary kluczy_ do uwierzytelniania i ustanawiania bezpiecznych połączeń. Aby wygenerować parę kluczy SSH, możesz użyć polecenia ssh-keygen w swoim terminalu.

```bash
 ssh-keygen
```

Podaj nazwę pliku lub użyj domyślnej, naciskając klawisz Enter i opcjonalnie hasło. W twoim katalogu domowym, w ukrytym folderze o nazwie .ssh, znajdują się teraz dwa pliki id_rsa, jeśli wybrałeś nazwy domyślne. Plik .pub jest kluczem publicznym i możesz zobaczyć jego zawartość za pomocą edytora tekstu.

Zaloguj się na swoje konto GitLab i przejdź do ustawień użytkownika. Kliknij „Klucze SSH” w menu nawigacyjnym po lewej stronie. Skopiuj i wklej swój klucz publiczny w polu Klucz i nadaj kluczowi odpowiedni tytuł, np. PC@Home. Kliknij przycisk „Dodaj klucz”, aby zapisać klucz. Twój klucz publiczny SSH został teraz dodany do Twojego konta GitLab i możesz go używać do uwierzytelniania w repozytoriach GitLab. Sprawdź, czy Twoje klucze i połączenie działają, używając polecenia ssh -T, aby zobaczyć wiadomość powitalną od GitLab.

```bash
 $ ssh -T git@ssh.gitlab.gnome.org
 Welcome to GitLab, @username!
```

## Podstawowe polecenia GitTeraz, gdy masz już zainstalowany Git i skonfigurowałeś klucz SSH w GitLabie, przejdźmy przez kilka podstawowych poleceń Git do zarządzania repozytoriami. Polecenia te pomogą Ci pracować z istniejącymi projektami, aktualizować je i bezpiecznie wprowadzać zmiany.

### 1. **Klonowanie repozytorium**

Klonowanie to proces tworzenia lokalnej kopii zdalnego repozytorium. Jest to przydatne, gdy chcesz pracować nad projektem, który już istnieje w GitLabie. Aby sklonować repozytorium, użyj polecenia `git clone` i adresu URL repozytorium:

```sh
git clone https://gitlab.com/username/repository.git
```

Zastąp `https://gitlab.com/username/repository.git` adresem URL repozytorium, które chcesz sklonować. To polecenie utworzy lokalną kopię repozytorium w nowym katalogu.

### 2. **Sprawdzanie statusu repozytorium**

Aby sprawdzić, czy w Twoim lokalnym repozytorium zaszły jakieś zmiany lub zobaczyć jego aktualny stan, użyj:

```sh
git status
```

To polecenie pokaże Ci, które pliki zostały zmodyfikowane, dodane lub usunięte w lokalnej kopii repozytorium.

### 3. **Zdalne repozytoria**

Zdalne repozytoria to wersje Twojego projektu hostowane online, na przykład w GitLab. Służą jako centralne miejsce, w którym przechowywany jest Twój kod i inni mogą uzyskać do niego dostęp. Domyślne zdalne repozytorium, które Git tworzy podczas klonowania projektu, nazywa się `origin`. Możesz dodawać, usuwać lub wyświetlać listę zdalnych repozytoriów za pomocą następujących poleceń:

- **Lista pilotów:**

  Aby zobaczyć, które zdalne repozytoria są połączone z Twoim lokalnym projektem, użyj:

  ```sh
  git remote -v
  ```

  To polecenie wyświetla listę wszystkich pilotów i ich adresów URL. Zwykle na liście będzie widoczny `origin`.

- **Dodawanie pilota:**

  Jeśli chcesz dodać nowe zdalne repozytorium, możesz to zrobić za pomocą:

  ```sh
  git remote add <name> <url>
  ```

  Zastąp `<name>` nazwą pilota i `<url>` adresem URL repozytorium.

- **Usuwanie pilota:**

  Aby usunąć zdalne repozytorium, użyj:

  ```sh
  git remote remove <name>
  ```

  Zastąp `<name>` nazwą pilota, który chcesz usunąć.

### 4. **Pobieranie zmian ze zdalnego repozytorium**

Jeśli chcesz zobaczyć, jakie zmiany zostały wprowadzone w zdalnym repozytorium bez stosowania ich do kopii lokalnej, użyj:

```sh
git fetch origin
```

To polecenie pobiera najnowsze zmiany ze zdalnego repozytorium, ale nie łączy ich z oddziałem lokalnym. Jest to sposób na sprawdzenie dostępności aktualizacji przed podjęciem decyzji o ich włączeniu.

### 5. **Resetowanie lokalnego repozytorium**

Jeśli chcesz zresetować repozytorium lokalne, aby dokładnie odpowiadało repozytorium zdalnemu, możesz zastosować „twardy” reset. **Ostrzeżenie:** spowoduje to zastąpienie wszelkich wprowadzonych zmian lokalnych.

```sh
git reset --hard origin/branch-name
```

Zastąp `branch-name` nazwą gałęzi, którą chcesz zresetować. To polecenie odrzuci wszelkie lokalne zmiany i sprawi, że twoje lokalne repozytorium będzie identyczne z repozytorium zdalnym.

### 6. **Wyświetlanie historii zatwierdzeń**

Aby zobaczyć listę zmian wprowadzonych w repozytorium na przestrzeni czasu, użyj:

```sh
git log
```

To polecenie wyświetla historię zatwierdzeń, w tym autora, datę i komunikat dla każdej zmiany. Jest to przydatne do zrozumienia, jakie zmiany zostały wprowadzone i kiedy.

### Podsumowanie

Te podstawowe polecenia Git pomogą Ci w pracy z repozytoriami, zapewnią aktualność lokalnych kopii i zapewnią bezpieczne zarządzanie zdalnymi repozytoriami. Klonowanie repozytoriów, sprawdzanie statusu kopii lokalnej i zarządzanie zdalnymi repozytoriami to kluczowe umiejętności w zarządzaniu projektami przy użyciu Git.