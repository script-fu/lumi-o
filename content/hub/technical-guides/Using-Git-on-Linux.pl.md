---
title: "Korzystanie z Gita w systemie Linux"
type: docs
url: "hub/technical-guides/Using-Git-on-Linux"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 7054a9ff9efeb93b4f494197e09ed1fe34d5d6bde7bc305480693c3982d375ae
---

Witamy w przewodniku dla początkujących dotyczącym korzystania z Git w systemie Linux! Ten przewodnik pomoże Ci rozpocząć pracę z Git i GitLab oraz zapewni podstawową wiedzę na temat korzystania z tych narzędzi.

## Przegląd Gita

Kod używany do tworzenia aplikacji jest przechowywany w zbiorze folderów i plików w Twoim systemie. Git to aplikacja, która pozwala tworzyć kopie zapasowe, udostępniać i kopiować tę kolekcję. Git to system kontroli wersji, który umożliwia śledzenie zmian w kodzie i współpracę z innymi. To potężne narzędzie szeroko stosowane w społeczności open source. GitLab to platforma internetowa, która pozwala hostować repozytoria Git i zarządzać nimi online, ułatwiając współpracę z innymi i śledzenie zmian w kodzie.

## Co to jest repozytorium?

_repo_, skrót od repozytorium, to lokalny folder zarządzany przez Git z kopią online. Repozytorium GitLab to zbiór plików i folderów tworzących projekt. Może mieć _branches_ — niezależne kopie tego samego projektu. Branch to osobna wersja projektu, która pozwala wprowadzać zmiany bez wpływu na wersję główną. Jest to przydatne do testowania nowych funkcji lub naprawiania błędów bez zakłócania głównego projektu. Istnieje repozytorium lokalne na dysku twardym oraz repozytorium zdalne (remote repo) przechowywane online za pomocą Git i GitLab.

## Korzystanie z Gita

Musisz zainstalować Git w swoim systemie. W systemach opartych na Debianie możesz użyć polecenia apt, aby zainstalować pakiety oprogramowania. W tym przypadku używamy go do instalacji Gita — pakietu udostępniającego system kontroli wersji Git. Polecenie sudo daje instalatorowi uprawnienia do instalacji w systemie.

```bash
 sudo apt install git
```

## Dostęp do GitLaba

Zanim będziesz mógł korzystać z [GitLab](https://gitlab.com/users/sign_up), musisz utworzyć konto, odwiedzając witrynę GitLab i kończąc proces rejestracji.

GitLab wymaga _SSH_ do bezpiecznej, uwierzytelnionej komunikacji między klientem (np. Tobą) a serwerem GitLab podczas operacji Git, takich jak _clone_, _push_ i _fetch_ repozytoriów. _Clone_ tworzy lokalną kopię repozytorium, _fetch_ pobiera zmiany z serwera do kopii lokalnej, a _push_ wysyła zmiany do repozytorium zdalnego. SSH (Secure Shell) to protokół sieciowy umożliwiający bezpieczny dostęp zdalny; wykorzystuje _pary kluczy_ do uwierzytelniania i nawiązywania bezpiecznych połączeń. Aby wygenerować parę kluczy SSH, użyj polecenia `ssh-keygen` w terminalu.

```bash
 ssh-keygen
```

Podaj nazwę pliku lub naciśnij Enter, aby użyć domyślnej, i opcjonalnie hasło. W katalogu domowym, w ukrytym folderze o nazwie .ssh, znajdują się teraz dwa pliki id_rsa, jeśli wybrałeś nazwy domyślne. Plik .pub to klucz publiczny; jego zawartość możesz zobaczyć w edytorze tekstu.

Zaloguj się na konto GitLab i przejdź do ustawień użytkownika. Kliknij „SSH Keys” w menu nawigacyjnym po lewej stronie. Skopiuj i wklej klucz publiczny w polu Key i nadaj mu odpowiedni tytuł, np. PC@Home. Kliknij „Add Key”, aby zapisać klucz. Twój publiczny klucz SSH został dodany do konta GitLab i możesz go używać do uwierzytelniania w repozytoriach GitLab. Sprawdź, czy klucze i połączenie działają, używając polecenia ssh -T — powinieneś zobaczyć wiadomość powitalną od GitLab.

```bash
 $ ssh -T git@ssh.gitlab.gnome.org
 Welcome to GitLab, @username!
```

## Podstawowe polecenia Git

Teraz, gdy masz zainstalowany Git i skonfigurowany klucz SSH w GitLabie, przejdźmy przez kilka podstawowych poleceń Git do zarządzania repozytoriami. Te polecenia pomogą Ci pracować z istniejącymi projektami, utrzymywać je na bieżąco i bezpiecznie wprowadzać zmiany.

### 1. **Klonowanie repozytorium**

Klonowanie to proces tworzenia lokalnej kopii repozytorium zdalnego. Jest to przydatne, gdy chcesz pracować nad projektem, który już istnieje w GitLabie. Aby sklonować repozytorium, użyj polecenia `git clone`, a następnie podaj URL repozytorium:

```sh
git clone https://gitlab.com/username/repository.git
```

Zastąp `https://gitlab.com/username/repository.git` adresem URL repozytorium, które chcesz sklonować. To polecenie utworzy lokalną kopię repozytorium w nowym katalogu.

### 2. **Sprawdzanie statusu repozytorium**

Aby sprawdzić, czy w lokalnym repozytorium zaszły zmiany, lub zobaczyć jego bieżący stan, użyj:

```sh
git status
```

To polecenie pokaże, które pliki zostały zmodyfikowane, dodane lub usunięte w lokalnej kopii repozytorium.

### 3. **Repozytoria zdalne**

Repozytoria zdalne to wersje projektu hostowane online, na przykład w GitLab. Służą jako centralne miejsce przechowywania kodu, do którego inni mogą uzyskać dostęp. Domyślne repozytorium zdalne, które Git tworzy podczas klonowania projektu, nazywa się `origin`. Możesz dodawać, usuwać i wyświetlać listę repozytoriów zdalnych za pomocą następujących poleceń:

- **Wyświetlanie repozytoriów zdalnych:**

  Aby zobaczyć, które repozytoria zdalne są połączone z lokalnym projektem, użyj:

  ```sh
  git remote -v
  ```

  To polecenie wyświetla listę wszystkich repozytoriów zdalnych i ich adresów URL. Zwykle na liście widoczny jest `origin`.

- **Dodawanie repozytorium zdalnego:**

  Aby dodać nowe repozytorium zdalne, użyj:

  ```sh
  git remote add <name> <url>
  ```

  Zastąp `<name>` nazwą repozytorium zdalnego i `<url>` adresem URL repozytorium.

- **Usuwanie repozytorium zdalnego:**

  Aby usunąć repozytorium zdalne, użyj:

  ```sh
  git remote remove <name>
  ```

  Zastąp `<name>` nazwą repozytorium zdalnego, które chcesz usunąć.

### 4. **Pobieranie zmian z repozytorium zdalnego**

Jeśli chcesz zobaczyć, jakie zmiany wprowadzono w repozytorium zdalnym, bez stosowania ich w kopii lokalnej, użyj:

```sh
git fetch origin
```

To polecenie pobiera najnowsze zmiany z repozytorium zdalnego, ale nie scala ich z lokalnym branchem. To sposób na sprawdzenie aktualizacji przed podjęciem decyzji o ich włączeniu.

### 5. **Resetowanie lokalnego repozytorium**

Jeśli chcesz zresetować lokalne repozytorium tak, aby dokładnie odpowiadało repozytorium zdalnemu, możesz użyć „twardego” resetu. **Ostrzeżenie:** spowoduje to nadpisanie wszelkich wprowadzonych zmian lokalnych.

```sh
git reset --hard origin/branch-name
```

Zastąp `branch-name` nazwą brancha, który chcesz zresetować. To polecenie odrzuci wszelkie lokalne zmiany i sprawi, że lokalne repozytorium będzie identyczne z repozytorium zdalnym.

### 6. **Wyświetlanie historii commitów**

Aby zobaczyć listę zmian wprowadzonych w repozytorium na przestrzeni czasu, użyj:

```sh
git log
```

To polecenie wyświetla historię commitów, w tym autora, datę i komunikat dla każdej zmiany. Jest to przydatne, aby zrozumieć, jakie zmiany wprowadzono i kiedy.

### Podsumowanie

Te podstawowe polecenia Git pomogą Ci pracować z repozytoriami, utrzymywać lokalne kopie na bieżąco i bezpiecznie zarządzać repozytoriami zdalnymi. Klonowanie repozytoriów, sprawdzanie statusu kopii lokalnej i zarządzanie repozytoriami zdalnymi to kluczowe umiejętności przy zarządzaniu projektami w Git.
