---
title: "Układ wydawniczy"
type: docs
url: "hub/features/publishing-layout"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: dc0367028ed8f6b4e1508c309384967daa43a4148f8d70f00880173a0a1fca7d
---

Ilustracja do druku i publikacji często wymaga czegoś więcej niż rozmiaru płótna. Strony mają linie cięcia, rozkładówki mają szwy środkowe, a ważna treść może musieć trzymać się z dala od obszarów, które zostaną obcięte lub wpadną w grzbiet oprawy. Narzędzia układu wydawniczego Lumi utrzymują te kwestie widoczne podczas malowania, bez spłaszczania ich w grafice.

Granice układu są przechowywane dla każdego obrazu, zapisywane z projektem i można je wyłączyć, gdy nie są potrzebne. Celem jest jasne poczucie struktury strony w pracy nad książką, komiksem czy drukiem, podczas gdy warstwowy obraz pozostaje w pełni edytowalny pod spodem.

## Spad i cięcie

Spad określa, jak daleko grafika wychodzi poza końcową krawędź strony. Lumi pokazuje obszar cięcia jako granicę aktywnej strony wewnątrz płótna, z marginesem spadu jako zacienioną nakładką wokół niego. Ułatwia to malowanie tła i detali brzegowych, które muszą przetrwać cięcie, bez zgadywania, gdzie skończy się gotowa strona.

Pomiary można ustawić w jednostkach odpowiednich do zlecenia — calach, milimetrach lub innej znanej jednostce druku, nie tylko w pikselach.

## Grzbiet i rozkładówki

W rozkładówkach dwustronicowych grzbiet wyznacza strefę chronioną wokół szwu środkowego, gdzie należy unikać ważnej treści. Po włączeniu Lumi pokazuje pasy grzbietu na rozkładówce, dzięki czemu twarze, tekst i punkty centralne pozostają poza obszarem oprawy, a cała rozkładówka nadal jest jednym ciągłym płótnem.

To szczególnie przydatne w komiksach, książkach obrazkowych i pracach drukowanych jako sąsiadujące strony, a nie pojedyncze arkusze.

## Prowadnice kompozycji

Opcjonalne prowadnice brzegowe zaznaczają przycięty obszar strony subtelnymi znacznikami kompozycji. Prowadnice mogą podążać za podziałem stron lub całą rozkładówką i używać trójpodziału, złotych proporcji lub pięciopodziału — zależnie od tego, jak oceniać układ.

Służą jako cicha referencja podczas układania i wykańczania i pomagają ocenić położenie względem strony, która faktycznie trafi do druku, a nie tylko względem pełnego cyfrowego płótna.

## Podgląd układu na płótnie

Nakładki układu steruje menu Widok. Spady, grzbiety i prowadnice można pokazywać osobno lub razem, aby artysta skupił się na tej części struktury publikacji, która w danej chwili ma znaczenie.

Obraz > Włącz układ włącza lub wyłącza granice układu dla bieżącego obrazu. Gdy układ jest wyłączony, nakładki są ukryte, a przełączniki widoku ustępują, lecz ustawienia granic pozostają zapisane w pliku na później.

## Zapisane z projektem

Ustawienia układu podróżują z projektem `.lum`. Późniejsze otwarcie obrazu przywraca spad, grzbiet, wygląd nakładki, wybór prowadnic i informację, czy układ jest włączony dla tego pliku. Konfiguracja świadoma publikacji pozostaje częścią stanu roboczego grafiki, a nie tymczasową preferencją wyświetlania.

Dla artystów przechodzących między szkicowaniem, malowaniem i przygotowaniem do druku wszystko zostaje w jednym miejscu: ten sam warstwowy obraz ze strukturą publikacji, gdy strona jej potrzebuje.
