---
title: "Instalacja Debiana"
type: docs
url: "hub/install-linux/Installing-Debian"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 1e79ae25c72fd6b2a9d31e1efe3019289f4b44d9230990f6874c0332de6c5f19
---

Ten dokument opisuje proces instalacji Debian Stable jako systemu operacyjnego do rozwoju Lumi-o. Może być przydatny dla osób konfigurujących podobne środowisko.

Wybrano Debian Stable, ponieważ Lumi ma budować się niezawodnie na przewidywalnej, długoterminowej platformie. Rozwój GIMP ukierunkowany jest na Debian Testing, dzięki czemu Debian Stable jest ściśle powiązanym systemem bazowym.

Lumi działa najlepiej na Debianie z Cinnamon (X11) i jest rozwijany oraz testowany w tym środowisku. Cinnamon zapewnia znajomy, przypominający Windows przepływ pracy na pulpicie, a X11 oferuje najbardziej stabilne środowisko do rozwoju Lumi.

Jeśli przechodzisz z Windows, główna zmiana koncepcyjna polega na tym, że większość instalacji i konfiguracji oprogramowania odbywa się przez menedżery pakietów i proste polecenia terminala, a nie przez instalatory do pobrania.

## Dla kogo jest ten przewodnik

Ten przewodnik dokumentuje działającą konfigurację Debian Stable używaną do rozwoju Lumi. To nie jest ogólny samouczek instalacji Linuxa.

Jest najbardziej przydatny dla:

- artystów przechodzących z Windows, którzy chcą przewidywalnej konfiguracji Linuxa
- programistów budujących Lumi ze źródeł
- użytkowników, którzy wolą odtworzyć znane środowisko pracy niż projektować własną konfigurację systemu

Zakłada się podstawową znajomość partycjonowania dysku i prostego korzystania z wiersza poleceń.

## Utwórz kopię zapasową danych

Przed instalacją Debiana utwórz pełną kopię zapasową katalogu domowego na dysku zewnętrznym. Dołącz wszelkie dodatkowe foldery z danymi, które chcesz zachować.

Uwaga: w systemie Linux `~` oznacza katalog domowy.

Jeśli korzystasz z repozytoriów Git, wypchnij ważne zmiany do remote'ów, aby można je było łatwo przywrócić po instalacji. Ten krok ma znaczenie tylko wtedy, gdy już używasz Git.

## Utwórz partycję

Przygotuj miejsce na dysku głównym dla Debiana. Istnieje wiele przewodników i narzędzi ułatwiających ten krok, w tym GParted. W zależności od konfiguracji możesz:

- zmniejszyć istniejącą partycję Windows w celu dual boot
- ponownie wykorzystać istniejącą partycję Linux
- utworzyć nowe partycje Linux i swap

Jeśli nie masz pewności, przed wprowadzeniem zmian zapoznaj się z przewodnikami dotyczącymi konkretnego sprzętu, ponieważ etapy partycjonowania znacznie różnią się między systemami.


## Utwórz instalacyjny nośnik USB Debiana

Zakładając, że docelowa partycja i przestrzeń swap już istnieją:

1. Pobierz obraz ISO Debiana z oficjalnej strony: https://www.debian.org/
2. W systemie Windows użyj BalenaEtcher, aby zapisać obraz ISO na dysku USB.
3. W systemie Linux użyj narzędzia wiersza poleceń, takiego jak `dd`, aby utworzyć rozruchowy nośnik USB.

## Zainstaluj Debiana

1. Włóż dysk USB.
2. Uruchom ponownie komputer i naciśnij klawisz menu rozruchu (zwykle `F2`, `F12`, `Esc` lub `Del`) podczas startu.
3. Wybierz urządzenie USB.
4. Wybierz instalator niegraficzny.
5. Po wyświetleniu monitu pozostaw hasło roota puste, aby instalator przyznał dostęp sudo do Twojego konta użytkownika.
6. Partycjonuj ręcznie:

   - System plików: ext4 (journaling)
   - Swap: istniejąca partycja swap
   - Punkt montowania: `/`
   - Etykieta: `linux`
   - Nazwa hosta: nazwa systemu wyświetlana jako `user@hostname`
   - Konto użytkownika: Twoje imię i nazwisko
   - Nazwa użytkownika: nazwa logowania w terminalu

7. Na tym etapie instalator Debiana umożliwia wybór środowiska graficznego; wybierz **Cinnamon** dla konfiguracji zalecanej przez Lumi.
8. Zakończ instalację i uruchom ponownie system Debian Stable.

## Konfiguracja systemu

### Skalowanie wyświetlacza

Debian Stable obecnie obsługuje skalowanie ułamkowe niespójnie, szczególnie na wyświetlaczach 4K. Zamiast obniżać rozdzielczość ekranu, dostosuj bezpośrednio elementy interfejsu.

Zalecane ustawienia:

- Unikaj ułamkowego skalowania wyświetlacza.
- Menu → Font Selection → Font Settings → Text Scaling Factor: `2.5`
- Desktop Font: `14`
- Panel → Customize → Panel Height: `60`
- Panel Appearance → Right Zone Symbolic Icon Size: `48px`
- Mysz i touchpad → Dostosowanie rozmiaru wskaźnika
- Desktop (right-click) → Customize → Larger icon size

Dostosowanie Firefoxa:

- Address bar → `about:config`
- Ustaw `layout.css.devPixelsPerPx` na `1`

### Terminal

Skonfiguruj preferencje terminala:

1. Menu → Terminal → Edit → Preferences
2. Text → Initial size: `140 columns`, `40 rows`
3. Text → Custom font: `Monospace 10`
4. Colours → Built-in schemes → Solarized Dark

### Klawisz Alt do zmiany rozmiaru narzędzi

Jeśli `Alt` + przeciąganie prawym przyciskiem myszy nie zmienia rozmiaru pędzli w Lumi, pulpit używa Alt do zarządzania oknami.

1. Wyszukaj **Okna** w menu systemowym.
2. Okna → Zachowanie → Specjalny przycisk do przenoszenia i zmieniania rozmiaru okien → **Nieaktywne**

Po tej zmianie `Alt` + przeciąganie prawym przyciskiem myszy powinno działać w Lumi do zmiany rozmiaru narzędzi.

## Przywróć dane

W razie potrzeby przywróć pliki z kopii zapasowej do katalogu domowego, na przykład:

- `Backup/Home/Artwork` → `~/Artwork`
- `Backup/Home/code` → `~/code`
- `Backup/Home/Desktop` → `~/Desktop`
- `Backup/Home/.ssh` → `~/.ssh`
- `Backup/Home/.config/lumi` → `~/.config/lumi`

Uwaga: foldery zaczynające się od `.` to ukryte katalogi konfiguracyjne w systemie Linux.

## Opcjonalnie: konfiguracja Git

Wymagane tylko wtedy, gdy planujesz zbudować Lumi lub przywrócić repozytoria.

### Zainstaluj Git

```bash
sudo apt install git
```

Skonfiguruj swoją tożsamość:

```bash
git config --global --edit
```

#### Dostęp do GitLab

Przywróć dostęp do repozytoriów w GitLab lub GitHub:

1. Zmień uprawnienia pliku klucza SSH: `chmod 600 ~/.ssh/id_rsa`
2. Dodaj klucz do agenta SSH: `ssh-add ~/.ssh/id_rsa`
3. Przetestuj połączenie: `ssh -T git@ssh.gitlab.gnome.org` lub `ssh -T git@github.com`

Dla każdego repozytorium pobierz zmiany z remote'ów i zresetuj lokalną gałąź, aby była zgodna:

```bash
git reset --hard remote-name/branch-name
git clean -df
```

Uruchom `git status`, aby potwierdzić, że repozytoria są czyste.

Masz teraz nowy system operacyjny z przywróconymi danymi i repozytoriami. Ta konfiguracja odzwierciedla znane środowisko pracy używane do rozwoju Lumi i w razie potrzeby można ją dostosować do indywidualnych przepływów pracy.

## Zbuduj Lumi po konfiguracji systemu

Skrypty budowania Lumi znajdują się w:

`~/code/lumi-dev/build/lumi/scripts`.

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# Install dependencies once
sudo bash lumi-install-packages.sh

# First full setup build
bash lumi-build-script.sh --scope setup --dir lumi-dev

# Regular rebuild after code changes
bash lumi-build-script.sh --scope build --dir lumi-dev

# Quick compile path
bash lumi-build-script.sh --scope compile --dir lumi-dev

# Launch Lumi
bash lumi-launch-active.sh lumi-dev
```

