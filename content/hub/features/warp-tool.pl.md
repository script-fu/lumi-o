---
title: "Narzędzie wypaczania"
type: docs
---
Narzędzie Wypaczanie przesuwa, ciągnie i przesuwa piksele swobodnie po obszarze roboczym. W Lumi idzie to dalej niż większość implementacji: może wypaczyć całą grupę warstw – niezależnie od tego, ile zagnieżdżonych warstw i masek zawiera – jako pojedynczy, zunifikowany obiekt, bez spłaszczania lub utraty jakiejkolwiek struktury.

## Przegląd

Wybierz warstwę i przeciągnij po niej, aby przesunąć piksele w dowolnym kierunku. Wypaczenie nie powoduje destrukcji podczas pracy: możesz cofać i powtarzać poszczególne pociągnięcia, zmieniać rozmiar pędzla lub zachowanie pomiędzy pociągnięciami i kontynuować udoskonalanie aż do zatwierdzenia. Zatwierdzenie powoduje destrukcyjne zastosowanie skumulowanej mapy przemieszczeń do danych pikseli warstwy.

Po wybraniu **warstwy grupowej** narzędzie działa na grupie jako całości. Widzisz podgląd na żywo całej skomponowanej grupy i wchodzisz z nim w interakcję. Po zatwierdzeniu to samo wypaczenie jest stosowane precyzyjnie i niezależnie do każdej warstwy podrzędnej i maski w grupie, zachowując pełną strukturę warstw.

## Wypaczenie grupowe

Wypaczanie grupy to główna umiejętność, która wyróżnia narzędzie wypaczania Lumi.

### Problem, który rozwiązuje

W większości programów malarskich wypaczanie ilustracji wielowarstwowej wymaga najpierw spłaszczenia grupy (zniszczenie struktury warstw) lub wypaczenia każdej warstwy z osobna i próby dopasowania ich wzrokowo (nudne i nieprecyzyjne). Żadne podejście nie zachowuje oryginalnej struktury do dalszej nieniszczącej edycji.

Lumi wypacza całą grupę jako jeden element, a następnie rozprowadza dokładnie tę samą transformację do każdej warstwy w jej obrębie.

### Jak to działa

Kiedy wybierzesz grupę i rozpoczniesz proces wypaczania, Lumi tworzy **pływającą warstwę podglądu** z złożonej projekcji grupy. Jeśli grupa zawiera maskę, maska ​​jest dodawana do podglądu, dzięki czemu podgląd dokładnie odzwierciedla ostateczny wygląd. Malujesz pociągnięcia osnowy bezpośrednio na tym podglądzie — otrzymujesz dokładnie to, co widzisz.

Po zatwierdzeniu, Lumi:

1. Stosuje przemieszczenie do każdej podstawowej warstwy w grupie (w tym głęboko zagnieżdżonych warstw w podgrupach), rozszerzając płótno każdej warstwy na tyle, aby uchwycić pełny obszar wypaczenia.
2. Stosuje to samo przemieszczenie do każdej maski w grupie w tym samym przejściu.
3. Wznawia automatyczne obliczanie granic grupy, więc rozmiar grupy zmienia się w celu dopasowania do nowo wypaczonych elementów podrzędnych.
4. Przycina każdą wypaczoną warstwę z powrotem do jej rzeczywistej, pomalowanej zawartości, aby zachować niewielkie rozmiary plików.
5. Usuwa warstwę podglądu i ponownie generuje projekcję grupową ze zaktualizowanych elementów podrzędnych.

Wszystko to dzieje się w ramach jednego kroku cofania. Po zatwierdzeniu grupa wygląda dokładnie tak, jak na podglądzie, z nienaruszonymi wszystkimi warstwami i maskami.

### Maski

Opcja **Maski wypaczenia** (domyślnie włączona) powoduje, że maski na każdej warstwie i grupie wewnątrz celu wypaczenia otrzymują identyczną transformację przemieszczenia. Maski warstw poruszają się wraz ze swoimi warstwami: maska, która przycinała kontur postaci, po wypaczeniu nadal przycina ten sam kontur.

Gdy opcja **Maski wypaczenia** jest wyłączona, przenoszona jest tylko zawartość warstwy; maski zachowują swoje pierwotne położenie.

## Opcje narzędzi

### Zachowanie

| Tryb | Efekt |
| :--- | :--- |
| **Przesuń** | Wypycha piksele w kierunku obrysu. Podstawowy tryb dla większości prac związanych z wypaczaniem. |
| **Rośnij** | Rozszerza piksele na zewnątrz od środka pędzla. |
| **Zmniejsz** | Przyciąga piksele do środka, w stronę środka pędzla. |
| **Wiruj zgodnie z ruchem wskazówek zegara** | Obraca piksele zgodnie z ruchem wskazówek zegara wokół środka pędzla. |
| **Wiruj w kierunku przeciwnym do ruchu wskazówek zegara** | Obraca piksele w lewo wokół środka pędzla. |
| **Usuń** | Usuwa przemieszczenie wypaczenia, przywracając piksele do ich oryginalnych pozycji. |
| **Gładkie** | Rozprasza przemieszczenia, zmiękczając nagłe przejścia pomiędzy obszarami wypaczonymi i niewypaczonymi. |

### Sterowanie pędzlem

- **Rozmiar**: Średnica pędzla osnowy w pikselach. Większe pędzle wypierają szersze obszary z bardziej miękkim opadaniem; mniejsze pędzle zapewniają precyzyjną, zlokalizowaną kontrolę.
- **Twardość**: Spadek od środka do krawędzi. Wysoka twardość zapewnia równomierne przemieszczenie na całej powierzchni szczotki; niska twardość skupia efekt w środku.
- **Siła**: Jak daleko przesuwają się piksele na jedno pociągnięcie. Niższa wytrzymałość umożliwia subtelne, stopniowe kształtowanie; wyższa siła powoduje dramatyczny, szybki ruch.

### Czas udaru

- **Uderzenie podczas ruchu** (tylko tryb ruchu): Stosuje zniekształcenie w sposób ciągły podczas ruchu myszy, a nie tylko po impulsie czasowym. Używaj do płynnych pociągnięć przypominających pędzel, gdy chcesz, aby przemieszczenie przesuwało się bezpośrednio za kursorem.
- **Obrysuj okresowo**: Stosuje zniekształcenie w ustalonych odstępach czasu, gdy przycisk myszy jest wciśnięty. Używaj w trybach wzrostu, zmniejszania i wirowania, gdzie zamierzeniem jest ciągłe, okrągłe nakładanie.
- **Stawka**: Częstotliwość okresowego stosowania udaru.

### Jakość

- **Interpolacja**: Metoda próbkowania stosowana podczas zatwierdzania. Liniowy jest szybki i płynny w przypadku większości prac; Cubic i Nohalo zapewniają wyższą wierność drobnych szczegółów.
- **Podgląd wysokiej jakości**: Używa próbnika jakości zatwierdzenia podczas interaktywnego podglądu. Wolniej, ale podgląd dokładnie odpowiada zatwierdzonemu wynikowi.

### Opcje grupy

- **Rozszerz obszar wypaczania** (tylko wypaczanie grupy): Liczba pikseli dodanych jako przezroczysty margines wokół podglądu grupy ze wszystkich stron. Daje to przesuniętej zawartości przestrzeń do wprowadzenia. Do większości prac wystarcza domyślna rozdzielczość 256 pikseli; zmniejsz ją w przypadku dużych obrazów, w których liczy się pamięć, lub zwiększ ją w przypadku bardzo dużych skoków przemieszczenia.
- **Maski wypaczenia**: Określa, czy zastosować to samo wypaczenie do masek warstw i grup. Domyślnie włączone.

## Cofnij i ponów

Każde pociągnięcie jest dyskretnym krokiem cofania w sesji warp. **Ctrl+Z** usuwa ostatni obrys i przywraca mapę przemieszczeń do poprzedniego stanu. **Ctrl+Y** (lub **Ctrl+Shift+Z**) powoduje ponowne zastosowanie. Przed zatwierdzeniem możesz przejrzeć całą historię udaru.

Naciśnięcie **Escape** lub przełączenie narzędzi powoduje odrzucenie wszystkich niezatwierdzonych pociągnięć i przywrócenie warstw do ich pierwotnego stanu. Żadne zmiany nie zostaną zapisane, dopóki nie zostanie to wyraźnie zatwierdzone.

## Zaangażowanie

Kliknij przycisk **Zatwierdź** (lub naciśnij **Enter**), aby destrukcyjnie zastosować nagromadzone wypaczenie. W przypadku wypaczeń grupowych uruchamia to pełną aplikację wielowarstwową opisaną powyżej. Historia cofnięć dla zatwierdzonego wypaczenia jest wówczas pojedynczym wpisem na stosie cofania obrazu, odwracalnym za pomocą standardowego polecenia **Edycja → Cofnij**.