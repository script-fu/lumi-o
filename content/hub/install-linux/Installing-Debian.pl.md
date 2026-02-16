---
title: "Instalowanie Debiana"
type: docs
url: "hub/install-linux/Installing-Debian"
---
Ten dokument opisuje proces instalacji Debian Stable jako rozwojowego systemu operacyjnego Lumi·o. Może być przydatne dla innych osób tworzących podobne środowisko.

Wybrano Debian Stable, ponieważ celem Lumi jest niezawodne budowanie na przewidywalnej, długoterminowej platformie. Rozwój GIMP jest ukierunkowany na testowanie Debiana, dzięki czemu Debian Stable jest ściśle dostosowanym systemem podstawowym.

Jeśli korzystasz z systemu Windows, główna zmiana koncepcyjna polega na tym, że większość instalacji i konfiguracji oprogramowania odbywa się za pomocą menedżerów pakietów i prostych poleceń terminala, a nie instalatorów do pobrania.

## Dla kogo jest przeznaczony ten przewodnik

Ten przewodnik dokumentuje działającą stabilną konfigurację Debiana używaną do programowania Lumi. To nie jest ogólny samouczek dotyczący instalacji Linuksa.

Jest najbardziej przydatny do:

- artyści odchodzący od systemu Windows, którzy chcą przewidywalnej konfiguracji systemu Linux
- programiści budujący Lumi ze źródeł
- użytkownicy, którzy wolą odtworzyć znane środowisko pracy niż projektować własną konfigurację systemu

Zakłada się podstawową znajomość partycjonowania dysku i prostego korzystania z wiersza poleceń.

## Utwórz kopię zapasową swoich danych

Przed instalacją Debiana utwórz pełną kopię zapasową katalogu domowego na dysku zewnętrznym. Dołącz wszelkie dodatkowe foldery danych, które chcesz zachować.

Uwaga: W systemie Linux `~` reprezentuje katalog domowy.

Jeśli korzystasz z repozytoriów Git, wypchnij wszelkie ważne zmiany do ich źródeł, aby można je było łatwo przywrócić po instalacji. Ten krok ma znaczenie tylko wtedy, gdy już korzystasz z Git.

## Utwórz partycję

Utwórz miejsce na dysku głównym dla Debiana. Istnieje wiele przewodników i narzędzi ułatwiających ten krok, w tym GParted. W zależności od konfiguracji możesz:

- zmniejsz istniejącą partycję Windows w celu podwójnego rozruchu
- ponownie użyj istniejącej partycji Linux
- przygotuj nowy Linux i zamień partycje

Jeśli nie masz pewności, przed wprowadzeniem zmian zapoznaj się z przewodnikami dotyczącymi konkretnego sprzętu, ponieważ etapy partycjonowania znacznie różnią się w zależności od systemu.


## Utwórz USB instalacyjny Debiana

Zakładając, że docelowa partycja i przestrzeń wymiany już istnieją:

1. Pobierz ISO Debiana z oficjalnej strony: https://www.debian.org/
2. W systemie Windows użyj narzędzia BalenaEtcher, aby zapisać obraz ISO na dysku USB.
3. W systemie Linux użyj narzędzia wiersza poleceń, takiego jak `dd`, aby utworzyć rozruchowy nośnik USB.

## Zainstaluj Debiana

1. Włóż dysk USB.
2. Uruchom ponownie i naciśnij klawisz menu startowego (zwykle `F2`, `F12`, `Esc` lub `Del`) podczas uruchamiania.
3. Wybierz urządzenie USB.
4. Wybierz instalator niegraficzny.
5. Po wyświetleniu monitu pozostaw hasło roota puste, aby instalator przyznał sudo dostęp do Twojego konta użytkownika.
6. Partycjonowanie ręczne:

   - System plików: ext4 (księgowanie)
   - Zamień: istniejąca partycja wymiany
   - Punkt montażu: `/`
   - Etykieta: `linux`
   - Nazwa hosta: nazwa systemu wyświetlana jako `user@hostname`
   - Konto użytkownika: Twoje imię i nazwisko
   - Nazwa użytkownika: nazwa logowania terminala

7. Wybierz **Cynamon** jako środowisko pulpitu.
8. Zakończ instalację i uruchom ponownie system Debian Stable.

## Konfiguracja systemu

### Skalowanie wyświetlacza

Debian Stable obecnie obsługuje skalowanie ułamkowe niekonsekwentnie, szczególnie na wyświetlaczach 4K. Zamiast zmniejszać rozdzielczość wyświetlacza, dostosuj bezpośrednio elementy interfejsu.

Zalecane korekty:- Unikaj ułamkowego skalowania wyświetlania.
- Menu → Wybór czcionki → Ustawienia czcionki → Współczynnik skalowania tekstu: `2.5`
- Czcionka na pulpicie: `14`
- Panel → Dostosuj → Wysokość panelu: `60`
- Wygląd panelu → Symboliczna ikona prawej strefy Rozmiar: `48px`
- Mysz i touchpad → Regulacja rozmiaru wskaźnika
- Pulpit (kliknij prawym przyciskiem myszy) → Dostosuj → Większy rozmiar ikony

Regulacja Firefoksa:

- Pasek adresu → `about:config`
- Ustaw `layout.css.devPixelsPerPx` na `1`

### Terminal

Skonfiguruj preferencje terminala:

1. Menu → Terminal → Edytuj → Preferencje
2. Tekst → Rozmiar początkowy: `140 columns`, `40 rows`
3. Tekst → Niestandardowa czcionka: `Monospace 10`
4. Kolory → Wbudowane schematy → Solarized Dark

## Przywróć dane

W razie potrzeby przywróć pliki z kopii zapasowej do katalogu domowego, na przykład:

- `Backup/Home/Artwork` → `~/Artwork`
- `Backup/Home/code` → `~/code`
- `Backup/Home/Desktop` → `~/Desktop`
- `Backup/Home/.ssh` → `~/.ssh`
- `Backup/Home/.config/lumi` → `~/.config/lumi`

Uwaga: Foldery zaczynające się od `.` to ukryte katalogi konfiguracyjne w systemie Linux.

## Opcjonalnie: konfiguracja Git

Wymagane tylko jeśli planujesz zbudować Lumi lub przywrócić repozytoria.

### Zainstaluj Gita

```bash
sudo apt install git
```

Skonfiguruj swoją tożsamość:

```bash
git config --global --edit
```

#### Dostęp do GitLaba

Przywróć dostęp do repozytorium w GitLab lub GitHub:

1. Zmień uprawnienia do pliku klucza SSH: `chmod 600 ~/.ssh/id_rsa`
2. Dodaj użytkownika do nowej instalacji Git: `ssh-add ~/.ssh/id_rsa`
3. Przetestuj połączenie: `ssh -T git@ssh.gitlab.gnome.org` lub `ssh -T git@github.com`

Dla każdego repozytorium pobierz źródła i zresetuj oddział lokalny, aby pasował:

```bash
git reset --hard remote-name/branch-name
git clean -df
```

Uruchom `git status`, aby potwierdzić, że repozytoria są czyste.

Mamy teraz nowy system operacyjny z przywróconymi wszystkimi danymi i repozytoriami. Ta konfiguracja odzwierciedla znane środowisko pracy używane do programowania Lumi i w razie potrzeby można ją dostosować do indywidualnych przepływów pracy.

## Zbuduj Lumi po skonfigurowaniu systemu operacyjnego

Skrypty kompilacji Lumi znajdują się w:

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