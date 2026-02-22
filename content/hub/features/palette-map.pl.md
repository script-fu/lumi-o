---
title: "Mapa palet"
type: docs
---
Mapa Palet odpowiada na praktyczne pytanie malarzy: mając zestaw pigmentów, jakie kolory można z nich właściwie mieszać? Zaczynając od pigmentów wejściowych palety, proceduralnie bada każdą kombinację (mieszanki dwóch pigmentów, mieszanki trójskładnikowe, różnice tonalne) i odwzorowuje wyniki na kole kolorów. Wynikiem jest obraz osiągalnej przestrzeni kolorów dla tego konkretnego zestawu pigmentów.

Mapa jest także narzędziem do nawigacji opartej na współrzędnych. Organizuje każdą wygenerowaną mieszankę według odcienia i jasności w okrągłą siatkę, dzięki czemu cała paleta jest czytelna na pierwszy rzut oka, a każdy kolor ma stabilny adres domowy.

## Struktura siatki

Mapa jest podzielona na siatkę 36 × 15:

- **36 sektorów odcieni**: kroki co 10° wokół koła, wyśrodkowane na nazwach głównych odcieni.
- **15 komórek jasności**: 3 komórki na pasmo wartości × 5 pasm (High Key, Upper Mid, Middle, Lower Mid, Deep), biegnących od bieli na zewnątrz do czerni w środku.

Każda komórka jest małym klinem na kole. Mówi się, że wpis umieszczony w komórce ma tę komórkę jako swoje **pochodzenie**: logiczny adres domowy na mapie.

## Kolory w komórkach

Gdy o tę samą komórkę rywalizuje wiele kolorów, w widocznym miejscu wyświetlany jest tylko jeden **zwycięzca**:

1. Zgłoszenia **główne** zawsze wygrywają swoją komórkę, niezależnie od innych osób.
2. Jeśli nie ma żadnego podstawowego, wygenerowana mieszanka (drugorzędna lub trzeciorzędna) z **najwyższą barwą** wygrywa.

Zgłoszenia, które nie wygrają, zajmują drugie miejsce i pozostają dostępne poprzez przełączanie kliknięć (patrz poniżej).

Wpisy niestandardowe (zapisane miksy) są renderowane jako kwadratowe kropki; wygenerowane mieszanki i kolory podstawowe są renderowane jako okrągłe kropki.

## Kliknij Jazda na rowerze

Kliknięcie zajętej komórki powoduje wybranie zwycięzcy jako koloru pierwszego planu. Ponowne kliknięcie tej samej komórki powoduje przejście do następnego mieszkańca (miksy wygenerowane na drugim miejscu, a następnie dowolne wpisy niestandardowe zapisane pod tym adresem siatki). Każde kliknięcie powoduje przejście o jeden krok w stosie.

**Kliknięcie lewym przyciskiem** powoduje wyświetlenie na pierwszym planie. Gdy docelowy kolor jest ustawiony na tło (z przybornika), zamiast tego klika opcję Trasa do tła.

## Shift-Select: Ładowanie punktów końcowych miksera

Przytrzymaj **Shift**, aby przejść do trybu ładowania punktu końcowego:

- **Kliknięcie lewym przyciskiem** przypisuje kliknięty wpis jako **Nadrzędny A (CCW)** w Mikserze palet.
- **Kliknięcie prawym przyciskiem** przypisuje go jako **Nadrzędny B (CW)**.

W tym trybie można wybierać tylko wpisy klasy A (mieszanki podstawowe i niestandardowe o nienaruszonym pochodzeniu). Trzecie wartości są ukryte, a kropki nie należące do klasy A są przyciemnione. Krótka nakładka potwierdza, że ​​tryb jest aktywny.

## Najważniejsze cechy nadrzędne miksera

Gdy Mikser Palet ma aktywne punkty końcowe Rodzica A i Rodzica B, oba są zaznaczone na Mapie **diamentowymi pierścieniami** (kształt rombu z czarną obwódką). Te podświetlenia pozostają widoczne nawet wtedy, gdy inne elementy wyświetlacza są przełączane, więc aktywne elementy nadrzędne mieszania są zawsze możliwe do zidentyfikowania.

## Pochodzenie a pozycja wizualna

Każdy wpis ma dwie pozycje na Mapie:

- **Pochodzenie (komórka źródłowa)**: Adres siatki logicznej, do którego należy wpis, ustalony na czas jego istnienia.
- **Wizualna pozycja punktu**: Miejsce, w którym kolor faktycznie jest renderowany na podstawie jego percepcyjnej barwy i jasności.

Dzięki **Przeniesieniu najlepszego dopasowania** po zapisaniu mieszanki system oblicza optymalną recepturę ostatecznego koloru i ustawia początek tak, aby pasował do wizualnej pozycji koloru. Dzięki temu zapisane kolory są zbliżone do ich wizualnej lokalizacji na kole i sprawia, że ​​mapa jest spójna przestrzennie.

## Przeciąganie zapisanych miksów

Pozycje niestandardowe (zapisane miksy) można zmieniać, przeciągając:1. Kliknij i przytrzymaj wpis Niestandardowy (kropka kwadratowa) i przeciągnij poza próg 5 pikseli.
2. Kursor zmieni się, wskazując tryb przeciągania. Najważniejsze elementy nadrzędne są aktualizowane na bieżąco w miarę poruszania się po mapie, aby pokazać nowe elementy nadrzędne łączące na każdej pozycji kandydata.
3. Przeciągnięta kropka zostanie przyciągnięta do najbliższej prawidłowej pozycji próbki.
4. Zwolnij, aby zatwierdzić. Wpis przyjmuje recepturę komórki docelowej: jej elementy nadrzędne, mieszanka, ton i nasycenie są aktualizowane w celu dopasowania, a jej początek jest aktualizowany w celu dopasowania do nowej pozycji wizualnej.

Ruchy przeciągania można cofnąć poprzez **Edycja → Cofnij**.

## Dwukrotne kliknięcie: przełączanie obszaru roboczego mapy

W **Edytorze palet** dwukrotne kliknięcie dowolnego wpisu palety powoduje włączenie i wyłączenie widoku obszaru roboczego Mapa palet. Jest to szybki sposób na przełączanie pomiędzy przeglądaniem zapisanych kolorów a mieszaniem na Mapie bez korzystania z menu. Nie ma to wpływu na działanie pojedynczego kliknięcia (przywracanie przepisu wpisu do miksera).

## Nakładka płótna

Mapę palet można przywołać bezpośrednio na kanwę obrazu jako nakładkę pełnoekranową, klikając **próbkę pierwszego planu/tła** w przyborniku. Daje to dużą powierzchnię miksowania bez konieczności poświęcania stałego panelu na Mapę.

## Centralna próbka koloru

Okrągła próbka znajduje się pośrodku dziury w kształcie pierścienia i odzwierciedla kolor dowolnej komórki, nad którą znajduje się kursor:

- **Kolor najechania**: gdy kursor zatrzyma się na wpisie mapy, próbka zostanie natychmiast zaktualizowana, aby pokazać kolor tego wpisu.
- **Wybrany kolor jako zastępczy**: gdy nie jest najechana żadna komórka, próbka pokazuje wynik obliczony przez Mikser palet dla aktualnie wybranego wpisu. Jeśli mikser nie został jeszcze rozwiązany, używa podstawowego koloru wyświetlanego wpisu, więc miejsce nigdy nie staje się puste.
- Cienka ciemna ramka przez cały czas wyznacza próbkę.
- Gdy kursor zatrzyma się na chwilę nad środkową próbką, pojawi się biało-czarny pierścień zewnętrzny, sygnalizujący, że obszar jest interaktywny.
- **Kliknięcie środkowej próbki** zamyka nakładkę płótna i powraca do normalnego widoku obrazu (tak samo jak kliknięcie poza zewnętrznym pierścieniem).

## Klawisz Alt: tryb porównania płótna

Kiedy nakładka płótna mapy palet jest otwarta, przytrzymanie **Alt** tymczasowo powoduje odsłonięcie obrazu poniżej:

- Cały interfejs mapy palety staje się niewidoczny (jego krycie spada do zera), odsłaniając płótno.
- Za kursorem podąża 64-pikselowa okrągła próbka wypełniona aktualnie próbkowanym kolorem Palette Mixer, dzięki czemu podczas sprawdzania obrazu pozostajesz świadomy aktywnego miksu.
- Zwolnienie Alt przywraca mapę palety z pełnym kryciem.

Etykieta podpowiedzi „Przytrzymaj klawisz Alt, aby zobaczyć obraz”* jest wyświetlana w widoku obszaru roboczego jako przypomnienie.