---
title: "Kopia zapasowa systemu z Clonezilla"
type: docs
url: "hub/install-linux/System-Backup-Clonezilla"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: b3453289d7da56bb4fc9039616edb73e537acd6a722f0eb8a000e4a398016863
---

Często tworzy się kopie zapasowe ważnych plików, aby wrócić do wcześniejszych wersji lub zastąpić uszkodzone dane. Innym ważnym typem kopii zapasowej jest jednak **klon dysku** — pełna kopia stanu systemu.

Gdy system jest już skonfigurowany i działa dobrze, pełna kopia zapasowa jest kluczowa do przywrócenia środowiska po awarii. Uzupełnia ona regularne zapisywanie danych roboczych.

[Clonezilla](https://clonezilla.org/) to darmowe oprogramowanie open source do tworzenia obrazów dysków i klonowania. Umożliwia tworzenie i przywracanie pełnych kopii zapasowych dysku twardego komputera — popularne narzędzie zarówno dla specjalistów IT, jak i użytkowników domowych.

Lepiej mieć kopię zapasową i jej nie potrzebować, niż potrzebować kopii zapasowej i jej nie mieć.


## Kluczowe funkcje Clonezilla

- **Obraz dysku**: Clonezilla tworzy dokładną kopię dysku twardego, w tym system operacyjny, aplikacje i dane.
- **Kopia zapasowa i przywracanie**: pozwala utworzyć obraz kopii zapasowej dysku i przywrócić go po awarii lub migracji na nowy dysk.
- **Darmowe i open source**: Clonezilla jest całkowicie darmowa; kod źródłowy można modyfikować i dostosowywać.


## Tworzenie kopii zapasowej w Clonezilla

### Kroki przygotowawcze

Potrzebujesz dysku USB dla Clonezilla oraz zewnętrznego dysku twardego większego niż dysk wewnętrzny, który chcesz sklonować.

Poniższe kroki upraszczają proces na podstawie [oficjalnego przewodnika](https://clonezilla.org//fine-print-live-doc.php?path=./clonezilla-live/doc/01_Save_disk_image/00-boot-clonezilla-live-cd.doc#00-boot-clonezilla-live-cd.doc). Warto też przejrzeć pełny przewodnik ze zrzutami ekranu.

1. **Utwórz Clonezilla Live USB lub CD/DVD**: postępuj zgodnie ze szczegółowymi instrukcjami na [stronie internetowej Clonezilla](https://clonezilla.org/liveusb.php), aby utworzyć rozruchowy nośnik USB lub CD/DVD.

2. **Podłącz zewnętrzny dysk kopii zapasowej**: podłącz dysk zewnętrzny i upewnij się, że system go rozpoznaje. To będzie miejsce docelowe kopii zapasowej.

3. **Sprawdź układ partycji**: użyj polecenia `lsblk` w terminalu, aby sprawdzić układ partycji głównego dysku twardego. Zanotuj nazwę głównego urządzenia.

4. **Uruchom komputer z Clonezilla Live USB**: uruchom ponownie komputer z nośnika Clonezilla, który utworzyłeś. Może być konieczne wejście do ustawień BIOS/UEFI (zwykle klawisze F2, F12, ESC lub DEL podczas startu) i ustawienie priorytetu rozruchu z USB.



### Kopia zapasowa w Clonezilla

1. **Wybierz tryb kopii zapasowej**: po uruchomieniu Clonezilla wybierz tryb „device-device”. Umożliwia on bezpośrednie sklonowanie dysku wewnętrznego na urządzenie zewnętrzne.

2. **Wybierz urządzenie źródłowe**: wybierz główny dysk wewnętrzny.

3. **Wybierz urządzenie docelowe**: wybierz zewnętrzny dysk kopii zapasowej jako cel. Uważaj przy wyborze urządzenia, aby nie nadpisać ważnych danych. Dysk docelowy musi być co najmniej tak duży jak źródłowy.

4. **Rozpocznij tworzenie kopii zapasowej**: Clonezilla rozpocznie proces. W zależności od rozmiaru partycji i szybkości dysków może to potrwać od kilku minut do kilku godzin.

5. **Oznacz kopię zapasową**: po zakończeniu oznacz dysk USB i dysk zewnętrzny datą oraz nazwą systemu, którego kopię utworzyłeś. Przechowuj je w bezpiecznym miejscu.

---

### Przywracanie z kopii zapasowej

Aby przywrócić system Debian z kopii zapasowej, wykonaj następujące kroki:

1. **Uruchom z nośnika Clonezilla**: włóż nośnik USB Clonezilla i uruchom z niego komputer, tak jak podczas tworzenia kopii zapasowej.

2. **Wybierz tryb przywracania**: ponownie wybierz tryb „device-device”, ale tym razem przywracaj z obrazu kopii zapasowej. Wszystkie dane zostaną skopiowane z dysku zewnętrznego z powrotem na dysk wewnętrzny.

3. **Wybierz urządzenie źródłowe**: wybierz dysk zewnętrzny, na którym przechowywana jest kopia zapasowa.

4. **Wybierz urządzenie docelowe**: wybierz dysk wewnętrzny, na który chcesz przywrócić kopię.

5. **Rozpocznij przywracanie**: Clonezilla rozpocznie proces przywracania. Podobnie jak przy tworzeniu kopii, czas zależy od rozmiaru dysku i szybkości sprzętu.

---

## Uwagi końcowe

Kopie zapasowe dysków w Clonezilla zachowują cały system — system operacyjny, ustawienia i aplikacje. Przy niewielkim wysiłku zabezpieczysz system przed poważną awarią i ograniczysz przestoje po awarii.

Pamiętaj: **kopie zapasowe są niezbędne**. Regularnie je aktualizuj i okresowo testuj, aby mieć pewność, że w razie potrzeby przywrócisz system.

Po uruchomieniu możesz podłączyć zewnętrzny dysk kopii zapasowej i sprawdzić strukturę partycji narzędziem Disks w Linuxie. Dysk zapasowy powinien odzwierciedlać strukturę dysku wewnętrznego — te same partycje oraz ewentualnie wolne miejsce, jeśli dysk zewnętrzny jest większy.

