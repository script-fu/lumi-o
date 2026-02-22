---
title: "Warstwy i edycja nieniszcząca"
type: docs
---
System warstw Lumi umożliwia złożone, nieniszczące przepływy pracy z pełną kontrolą nad mieszaniem, maskowaniem i kompozycją.

## Przegląd

Warstwy są podstawą uporządkowanej ilustracji. Każda warstwa jest niezależna i ma własny tryb mieszania, krycie i opcjonalną maskę warstwy. Grupy mogą zagnieżdżać warstwy hierarchicznie, korzystając z własnych właściwości mieszania i przycinania.

## Dostęp

**Panele** → **Warstwy** lub domyślny panel **Warstwy** po prawej stronie.

## Typy warstw

### Warstwy farby

Standardowe warstwy rastrowe dla malowanej zawartości. Przechowuj dane pikseli jako bufory GEGL z opcjonalną przezroczystością alfa.

### Grupuj warstwy

Hierarchiczne kontenery do organizowania powiązanych warstw. Grupy mogą mieć własny tryb mieszania, krycie i maski przycinające. Projekcje grupowe są komponowane na żądanie.

### Maski warstw

Maski skali szarości dołączone do dowolnej warstwy, kontrolujące krycie na piksel. Malowanie na masce bielą sprawia, że ​​piksele stają się nieprzezroczyste; czarny sprawia, że ​​są przezroczyste; szary zapewnia częściowe krycie.

## Tryby mieszania

Każda warstwa ma tryb mieszania określający sposób łączenia jej z warstwami poniżej:

- **Normalny**: Bezpośrednie mieszanie krycia.
- **Pomnóż**: Przyciemnij poprzez pomnożenie wartości kolorów.
- **Ekran**: Rozjaśnij poprzez odwracanie, mnożenie i ponowne odwracanie.
- **Nakładka**: połączenie mnożenia i ekranowania.
- **Dodaj**: Mieszanie addytywne (sumuje wartości kolorów).
- **Odejmij**: Mieszanie odejmujące.
- **Kolor, odcień, nasycenie, jasność**: Mieszanie komponentów HSL.

## Przycinanie i maskowanie

- **Tryb kompozytowy: Przytnij do tła**: Ustawienie trybu złożonego warstwy na **Przytnij do tła** ogranicza komponowanie do obszarów, w których skumulowane warstwy **Łączące** poniżej mają ustaloną nieprzezroczystość. Warstwa maluje tylko te warstwy, które zawierają treść i nie mogą rozszerzać śladu alfa. Ustawia się to dla poszczególnych warstw w oknie dialogowym Atrybuty warstw (menu **Tryb kompozytowy**). Jeśli efektywnym trybem złożonym warstwy jest inny niż Union, ikona oka w panelu Warstwy zostaje zastąpiona ikoną złożoną, aby wskazać niestandardowe zachowanie podczas komponowania.

  **Przykład: wspólny kształt alfa:** w grupie dolna warstwa zawiera wypełniony okrąg na przezroczystym tle, ustawiony na domyślny tryb złożony **Unia**. Każda warstwa powyżej w tej samej grupie ma ustawioną opcję **Przytnij do tła**. Warstwy te mogą malować tylko tam, gdzie okrąg zapewnia krycie (jeden kształt, wiele warstw). Jest to powszechny wzór do kolorowania, cieniowania i opracowywania szczegółów w ramach określonej sylwetki bez obawy o rozlanie.
- **Maski warstw**: Zastosuj maskę w skali szarości, aby kontrolować widoczność warstwy piksel po pikselu. Malowanie na biało maski ujawnia; czarne kryjówki; szary zapewnia częściowe krycie.
- **Maski Pure-Child**: Maski są przechowywane jako maski podrzędne w stosie do rysowania, co zapobiega utracie danych podczas transformacji.

## Wybieranie warstw (klawisz Alt)

Dotknięcie **Alt** (lewy Alt) podczas przesuwania wskaźnika myszy nad obszarem roboczym powoduje wybranie warstwy z widocznymi pikselami pod kursorem, bez przełączania narzędzi i klikania.

### Jak to działa

- **Naciśnij Alt**: Kursor zmieni się w krzyżyk, wskazując, że tryb wybierania jest aktywny.
- **Release Alt**: Lumi wybiera najwyższą nieprzezroczystą warstwę w pozycji kursora (krycie > 25%) i zaznacza ją. Warstwa zostanie podświetlona w panelu Warstwy, a na pasku stanu pojawi się komunikat **„Wybrana warstwa: 'nazwa warstwy'”**.
- W środkowym punkcie wybranej warstwy na płótnie rysowany jest uchwyt. Uchwyt zmniejsza się i zanika w miarę oddalania się kursora.

### Cykliczne przełączanie warstwKażde kolejne naciśnięcie klawisza Alt w tym samym miejscu powoduje wybranie **następnej warstwy w dół** na stosie w tym momencie. Lumi zapamiętuje ostatnią wybraną warstwę i przechodzi obok niej do warstwy znajdującej się poniżej. Po osiągnięciu dna stosu kolejne dotknięcie powoduje powrót do najwyższej warstwy w tej pozycji. Ułatwia to dostęp do zagnieżdżonych warstw w złożonych scenach poprzez wielokrotne naciśnięcie klawisza Alt.

### Zasady anulowania

Wybór zostaje anulowany (nie zostaje aktywowany po zwolnieniu klawisza Alt), jeśli przy przytrzymanym klawiszu Alt wystąpi jedna z poniższych sytuacji:

- Naciśnięto przycisk myszy (lewy lub prawy przycisk myszy).
- Naciśnięto dowolny inny klawisz.

Dzięki temu gesty przeciągania Alt (takie jak regulacja rozmiaru pędzla) i skróty zmodyfikowane za pomocą klawisza Alt będą działać bez przypadkowej zmiany aktywnej warstwy.

### Ograniczenia

- Wybieranie warstw nie jest aktywowane podczas operacji narzędzia **Przekształć**; Alt ma tam inne znaczenie.
- Pobieranie nie następuje, jeśli występuje selekcja pływająca.
- Tylko lewy Alt uruchamia wybieranie; prawy Alt jest traktowany jako standardowy modyfikator.

## Operacje

W panelu Warstwy:

- **Utwórz warstwę**: Kliknij prawym przyciskiem myszy → **Nowa warstwa** lub użyj menu **Warstwa**.
- **Duplikuj**: Kliknij prawym przyciskiem myszy → **Duplikuj** lub **Warstwa** → **Duplikuj**.
- **Usuń**: Kliknij prawym przyciskiem myszy → **Usuń** lub wybierz i naciśnij **Usuń**.
- **Zmień kolejność**: przeciągnij warstwy w górę lub w dół, aby zmienić kolejność układania.
- **Zmień nazwę**: Kliknij dwukrotnie nazwę warstwy.
- **Scal w dół**: Kliknij prawym przyciskiem myszy → **Scal w dół**, aby połączyć z warstwą poniżej.
- **Spłaszczony obraz**: **Obraz** → **Spłaszczony obraz**, aby połączyć wszystkie widoczne warstwy.

## Właściwości warstwy

- **Krycie**: 0–100%, kontroluje ogólną przezroczystość warstwy.
- **Tryb mieszania**: menu rozwijane umożliwiające wybranie sposobu łączenia warstwy z warstwami poniżej.
- **Widoczne/Ukryte**: Ikona oka przełącza widoczność warstwy.

## Blokady warstw

Ikony kłódki są wyświetlane w wierszu nagłówka panelu Warstwy. Każdą blokadę można przełączać niezależnie. Kliknięcie prawym przyciskiem myszy ikony kłódki ustawia ją wyłącznie (blokuje tylko ten typ, odblokowując wszystkie inne na tej samej warstwie).

- **Lock Alpha**: Zapobiega malowaniu na przezroczystych obszarach. Pociągnięcia pędzlem wpływają tylko na piksele, które mają już krycie; w pełni przezroczyste piksele nie są modyfikowane. Przydatny do malowania w obrębie istniejących kształtów bez rozlewania się na zewnątrz.

- **Zablokuj maskę**: Uniemożliwia edycję maski warstwy. Maska pozostaje widoczna i aktywna, ale nie można jej pomalować ani zmodyfikować, gdy ta blokada jest włączona.

- **Zablokuj kolor**: Blokuje malowanie na określonym kolorze: bieżącym kolorze pierwszego planu w momencie zastosowania blokady. Kolejne pociągnięcia na tej warstwie korzystają z zapisanego koloru niezależnie od aktywnego koloru pierwszego planu. Odblokowanie powoduje odrzucenie zapisanego koloru.

- **Zablokuj zawartość** (Zablokuj piksele): Zapobiega wszelkim edycjom pikseli w warstwie. Warstwa nie może być malowana, wypełniana, przekształcana ani w żaden inny sposób modyfikowana. Przydatny do zabezpieczania gotowych warstw.

- **Zablokuj pozycję**: Zapobiega przesuwaniu lub przekształcaniu warstwy. Warstwę można nadal edytować; blokowane są jedynie zmiany położenia (narzędzie Przesuń, narzędzie Przekształć).

- **Zablokuj widoczność**: Zapobiega przełączaniu widoczności warstwy przez ikonę oka. Chroni warstwy, które powinny zawsze pozostać widoczne (lub ukryte) podczas edycji.

Wszystkie blokady są zapisywane w projekcie i pozostają w pamięci pomiędzy sesjami.

## Efekty warstw (fx)

Nieniszczące filtry GEGL zastosowane za pomocą menu **Filtry** są przechowywane jako zatwierdzone efekty na warstwie, a nie natychmiast modyfikują piksele. Gdy warstwa ma co najmniej jeden zatwierdzony efekt, w panelu Warstwy obok tej warstwy pojawia się ikona **fx**.### Dostęp do wyskakującego okienka efektów

Kliknij ikonę **fx** w wierszu warstwy w panelu Warstwy, aby otworzyć wyskakujące okienko **Efekty warstw** dla tej warstwy.

W wyskakującym okienku wyświetlany jest stos filtrów dla warstwy, a każdy zatwierdzony efekt jest wymieniony według nazwy i znajduje się obok niego przełącznik widoczności.

### Sterowanie

- **Przełączanie widoczności oka** (na górze wyskakującego okienka): Włącza i wyłącza wszystkie efekty jednocześnie.
- **Przełączanie widoczności poszczególnych filtrów**: Każdy wiersz filtra ma własną ikonę oka, która umożliwia niezależne włączenie lub wyłączenie tego efektu.
- **Edycja**: Otwiera okno dialogowe ustawień wybranego filtra, umożliwiając nieniszczącą regulację jego parametrów.
- **Podnieś / Opuść**: Przesuwa wybrany filtr w górę lub w dół stosu, zmieniając kolejność stosowania efektów.
- **Scal**: Zapisuje wszystkie aktualnie widoczne efekty na pikselach warstwy, czyniąc zmiany trwałymi. Ikona fx zostanie usunięta, jeśli wszystkie efekty zostaną scalone. Scalanie nie jest dostępne w warstwach grupowych.
- **Usuń**: Całkowicie usuwa wybrany filtr. Jeśli nie pozostaną żadne efekty, okno popover zamknie się automatycznie.

Dwukrotne kliknięcie filtru na liście powoduje również otwarcie okna dialogowego edycji.

Funkcje **Edycja** i **Usuń** są zablokowane, jeśli na warstwie włączona jest opcja Zablokuj piksele. Nie można zmienić kolejności filtrów, gdy jeden z nich jest aktywnie edytowany.

### Dodawanie efektów

Zastosuj filtr z **Filtry** → (dowolna kategoria). Jeśli celem jest aktywna warstwa, a operacja przebiega w sposób nieniszczący, wynik jest zapisywany jako efekt warstwy, a nie zapisywany w danych pikseli. Ikona fx pojawia się na warstwie, gdy występuje co najmniej jeden efekt.

## Okno dialogowe atrybutów warstwy

Kliknij dwukrotnie warstwę w panelu Warstwy, aby otworzyć okno dialogowe Atrybuty warstw.

### Tożsamość

- **Znacznik koloru**: Etykieta koloru służąca do organizacji wizualnej w panelu Warstwy.

### Złożona przestrzeń i tryb

- **Przestrzeń złożona**: Przestrzeń kolorów używana podczas łączenia tej warstwy z warstwami poniżej. Opcje: Auto, Liniowy (RGB), Percepcyjny (RGB).
- **Tryb złożony**: Kontroluje interakcję warstwy alfa z tłem. Opcje obejmują Łączenie (wpływa na wszystkie obszary, ustawienie domyślne w trybie normalnym), Przytnij do tła (wpływa tylko na obszary z istniejącą zawartością, ustawienie domyślne w większości innych trybów mieszania) i Przecięcie.

### Rozmiar i przesunięcia

W przypadku istniejącej warstwy opcja **Rozmiary** pokazuje wymiary warstwy i wymiary maski (jeśli maska jest dołączona) jako etykiety tylko do odczytu.

**Przesunięcia warstw**: Pokrętła X i Y kontrolujące położenie warstwy na płótnie. Zmiany obowiązują natychmiast, a nie po zamknięciu okna dialogowego.

Jeśli warstwa zawiera maskę, poniżej pokazano **Przesunięcia maski** (pokrętła X i Y dla niezależnej pozycji maski).

Podczas tworzenia nowej warstwy pola Szerokość i Wysokość oraz lista rozwijana **Wypełnij** (Pierwszy plan, Tło, Biały, Przezroczysty) zastępują wyświetlany rozmiar tylko do odczytu.

### Atrybuty warstw (trwałe pasożyty)

Dolna część okna dialogowego zawiera przewijaną tabelę nazw/wartości dla trwałych pasożytów (dowolne metadane klucz-wartość dołączone do warstwy). Wartości te są przechowywane w projekcie i są dostępne z poziomu interfejsu skryptowego Scheme.

- Kliknij dowolną komórkę w kolumnie Nazwa lub Wartość, aby edytować ją bezpośrednio.
- **Dodaj**: Dołącza nowy pusty wiersz.
- **Usuń**: Usuwa zaznaczony wiersz i jego pasożyta z warstwy.

Jeśli warstwa nie zawiera trwałych pasożytów, pokazane zostaną trzy puste rzędy startowe.

### Stan zawartościLinia informacyjna tylko do odczytu na dole pokazuje bieżący stan zawartości warstwy (i maski, jeśli jest dostępna): **Przezroczysta**, **Jednolita** lub **Mieszana**. Przedrostek `*` wskazuje, że warstwa zawiera niezapisane zmiany od ostatniego zapisu.

## Wydajność

- **Tryb szybki**: Podczas malowania na pojedynczej warstwie zagnieżdżonej w grupie Lumi tymczasowo przełącza grupy przodków na renderowanie przechodzące na czas trwania pociągnięcia, pomijając pełną rekompozycję projekcji grupowej. Eliminuje to opóźnienie aktualizacji projekcji zagnieżdżonej podczas pisania odręcznego i malowania. Pełne komponowanie zostanie wznowione po zakończeniu obrysu, zmianie aktywnej warstwy lub przed zapisaniem.

  Tryb szybki jest wyłączony, jeśli którykolwiek z poniższych warunków ma zastosowanie do grupy przodków:
  - Grupa posiada widoczne filtry nieniszczące (filtry wymagają bufora projekcyjnego).
  - Tryb mieszania grupy jest inny niż **Normalny** lub **Przekazujący**.
  - Grupa ma bezpośredni element podrzędny korzystający z trybu złożonego **Przytnij do tła** lub **Przecięcie** (wymagają one danych tła z bufora projekcji).

  Tryb szybki nie jest także włączany w przypadku warstw najwyższego poziomu, swobodnych zaznaczeń lub gdy celem jest jednocześnie wiele warstw.

  Strukturyzacja plików w celu uniknięcia tych warunków w grupach malowania przy użyciu trybów mieszania Normalny na warstwach gwarantuje, że tryb szybki pozostanie aktywny przez całą sesję odręczną lub malowania.
- **Leniwe ładowanie**: Duże projekty ładują się szybko; dane warstw są ładowane tylko wtedy, gdy są potrzebne (np. gdy stają się widoczne lub malowane).

## Format pliku

Wszystkie warstwy, maski i właściwości są przechowywane w otwartym formacie Lumi `.lum`. Plik jest katalogiem zawierającym poszczególne bufory warstw i metadane, zapewniającym kompatybilność i długoterminową dostępność.