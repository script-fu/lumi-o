---
title: "Kopia zapasowa systemu za pomocą Clonezilli"
type: docs
url: "hub/install-linux/System-Backup-Clonezilla"
---
Często tworzy się kopie zapasowe ważnych plików, aby powrócić do wcześniejszych wersji lub zastąpić uszkodzone dane. Jednakże innym istotnym typem kopii zapasowej jest **klon dysku**, czyli pełna kopia zapasowa stanu systemu.

Gdy system jest już skonfigurowany i działa prawidłowo, utworzenie pełnej kopii zapasowej ma kluczowe znaczenie dla przywrócenia środowiska w przypadku wystąpienia awarii. Ta kopia zapasowa uzupełnia regularne zapisywanie danych roboczych.

[Clonezilla](https://clonezilla.org/) to bezpłatne oprogramowanie do tworzenia i klonowania dysków o otwartym kodzie źródłowym. Umożliwia użytkownikom tworzenie i przywracanie pełnych kopii zapasowych dysku twardego komputera, co czyni go popularnym narzędziem zarówno wśród informatyków, jak i użytkowników domowych.

Zawsze lepiej mieć kopię zapasową i jej nie potrzebować, niż potrzebować kopii zapasowej i jej nie mieć.


## Kluczowe cechy Clonezilli

- **Obraz dysku**: Clonezilla tworzy dokładną kopię dysku twardego, w tym system operacyjny, aplikacje i dane.
- **Kopia zapasowa i przywracanie**: Umożliwia utworzenie obrazu kopii zapasowej dysku twardego i przywrócenie go w przypadku awarii lub migracji na nowy dysk.
- **Bezpłatne i otwarte oprogramowanie**: Clonezilla jest całkowicie darmowa, a kod źródłowy można modyfikować i dostosowywać.


## Używanie Clonezilli do tworzenia kopii zapasowych

### Kroki przygotowawcze

Będziesz potrzebował dysku USB dla Clonezilli i zewnętrznego dysku twardego, który jest większy niż dysk wewnętrzny, który chcesz sklonować.

Te kroki upraszczają proces w oparciu o [official guide](https://clonezilla.org//fine-print-live-doc.php?path=./clonezilla-live/doc/01_Save_disk_image/00-boot-clonezilla-live-cd.doc#00-boot-clonezilla-live-cd.doc). Dobrym pomysłem jest przejrzenie pełnego przewodnika, który zawiera zrzuty ekranu dla większej przejrzystości.

1. **Utwórz Clonezilla Live USB lub CD/DVD**: Postępuj zgodnie ze szczegółowymi instrukcjami na Clonezilla [website](https://clonezilla.org/liveusb.php), aby utworzyć bootowalną płytę USB lub CD/DVD.

2. **Podłącz zewnętrzny dysk kopii zapasowej**: Podłącz dysk zewnętrzny i upewnij się, że jest rozpoznawany przez system. To będzie miejsce docelowe Twojej kopii zapasowej.

3. **Sprawdź układ partycji**: Użyj polecenia `lsblk` w terminalu, aby sprawdzić układ partycji głównego dysku twardego. Zanotuj nazwę głównego urządzenia.

4. **Uruchom z dysku USB Clonezilla Live**: Uruchom ponownie komputer i uruchom komputer z utworzonego nośnika Clonezilla. Może być konieczne uzyskanie dostępu do ustawień BIOS/UEFI (zwykle poprzez naciśnięcie klawisza F2, F12, ESC lub DEL podczas uruchamiania) i dostosowanie kolejności rozruchu, aby nadać priorytet napędowi USB.



### Kopia zapasowa w Clonezilli

1. **Wybierz tryb kopii zapasowej**: Po uruchomieniu Clonezilli wybierz tryb „urządzenie-urządzenie”. Ten tryb umożliwia bezpośrednie klonowanie dysku wewnętrznego na urządzenie zewnętrzne.

2. **Wybierz urządzenie źródłowe**: Wybierz główny dysk wewnętrzny.

3. **Wybierz urządzenie docelowe**: Wybierz zewnętrzny dysk kopii zapasowych jako urządzenie docelowe. Przy wyborze urządzenia należy zachować ostrożność, aby uniknąć nadpisania ważnych danych. Upewnij się, że dysk docelowy jest równy lub większy niż dysk źródłowy.

4. **Rozpocznij proces tworzenia kopii zapasowej**: Clonezilla rozpocznie proces tworzenia kopii zapasowej. W zależności od rozmiaru partycji i szybkości dysków może to zająć od kilku minut do kilku godzin.

5. **Oznacz swoją kopię zapasową**: Po zakończeniu tworzenia kopii zapasowej oznacz dysk USB i zewnętrzny dysk twardy datą oraz nazwą systemu, którego kopię zapasową utworzono. Przechowuj je w bezpiecznym miejscu.

---

### Przywracanie z kopii zapasowej

Jeśli chcesz przywrócić system Debian z kopii zapasowej, wykonaj następujące kroki:

1. **Uruchom z Clonezilla Media**: Włóż Clonezilla USB i uruchom z niego komputer, wykonując te same czynności, co podczas procesu tworzenia kopii zapasowej.2. **Wybierz tryb przywracania**: Wybierz ponownie tryb „urządzenie-urządzenie”, ale tym razem przywrócisz z obrazu kopii zapasowej. Spowoduje to skopiowanie wszystkich danych z dysku zewnętrznego z powrotem na dysk wewnętrzny.

3. **Wybierz urządzenie źródłowe**: Wybierz dysk zewnętrzny, na którym przechowywana jest kopia zapasowa.

4. **Wybierz urządzenie docelowe**: Wybierz dysk wewnętrzny, na którym chcesz przywrócić kopię zapasową.

5. **Rozpocznij proces przywracania**: Clonezilla rozpocznie proces przywracania. Podobnie jak w przypadku tworzenia kopii zapasowej, wymagany czas będzie zależał od rozmiaru dysku i szybkości sprzętu.

---

## Uwagi końcowe

Kopie zapasowe dysków za pomocą Clonezilla zapewniają zachowanie całego systemu — systemu operacyjnego, ustawień i aplikacji. Przy minimalnym wysiłku można zabezpieczyć system przed katastrofalną awarią i zminimalizować przestoje w przypadku awarii.

Pamiętaj, **kopie zapasowe są niezbędne**. Regularnie aktualizuj swoje kopie zapasowe i okresowo je testuj, aby mieć pewność, że w razie potrzeby będziesz mógł przywrócić system.

Po uruchomieniu możesz podłączyć zewnętrzny dysk kopii zapasowej i sprawdzić strukturę jego partycji za pomocą narzędzia Disks w systemie Linux. Dysk zapasowy powinien odzwierciedlać strukturę dysku wewnętrznego, z tymi samymi partycjami i pewną ilością niewykorzystanego miejsca, jeśli dysk zewnętrzny jest większy.