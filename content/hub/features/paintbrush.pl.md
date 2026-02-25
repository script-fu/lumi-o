---
title: "Narzędzie Pędzel"
type: docs
---
Pędzel to podstawowe narzędzie do malowania, zaprojektowane z myślą o responsywnym, inteligentnym malowaniu pędzlem z pełną kontrolą nad naciskiem, prędkością, pochyleniem i dynamiką odstępów.

## Przegląd

Narzędzie Pędzel obsługuje typy pędzli rastrowych, generowanych proceduralnie i animowanych. Pociągnięcia można stabilizować, wygładzać i poddawać obróbce końcowej. Dynamika pędzla reaguje na polecenia rysika, zapewniając precyzyjną kontrolę nad kryciem, rozmiarem, kolorem, kątem i innymi właściwościami podczas pociągnięcia.

## Typy pędzli

### Pędzle rastrowe (.raster)

Obrazy pędzli bitmapowych obsługujące przezroczystość alfa.

### Wygenerowane pędzle (.param)

Proceduralnie renderowane kształty (Koło, Kwadrat, Diament, Trójkąt) z regulowanymi parametrami: twardością, współczynnikiem proporcji, kątem, okrągłością i promieniem narożnika. Wygenerowane pędzle są lekkie i skalowalne.

### Animowane pędzle (.anim)

Sekwencyjne sekwencje klatek, które postępują podczas pociągnięć. Ramki mogą być cyklicznie przełączane przyrostowo (postęp klatek na dotknięcie), wybierane losowo na dotknięcie lub indeksowane według dynamiki (nacisk, prędkość, pochylenie, kąt).

## Malowanie kursora

Kursor dostosowuje się do bieżącego stanu narzędzia, zapewniając jasne, kontekstowe informacje zwrotne:

- **Zarys pędzla**: Kursor śledzi dokładny kształt i rozmiar pędzla, dając podgląd na żywo miejsca, w którym wyląduje farba.
- **Tryb wymazywania**: Gdy wymazywanie jest aktywne, kontur zmienia się w przerywany okrąg, aby wizualnie odróżnić pociągnięcia wymazywane od pociągnięć farby.
- **Prosta granica pędzla**: W przypadku złożonych lub bardzo dużych pędzli, gdzie renderowanie dokładnego konturu jest kosztowne, włącz opcję **Prosta granica pędzla** (w opcjach dodatkowych), aby zamiast tego używać zwykłego okręgu.

## Opcje narzędzi

### Sterowanie najwyższego poziomu

Obecny cały czas, poza jakimkolwiek ekspanderem:
- **Tryb**: Tryb mieszania farb (Normalny, Pomnóż, Ekran itp.)
- **Krycie**: Ogólne krycie obrysu (0–100).

### Właściwości pędzla

W ekspanderze **Właściwości pędzla** (domyślnie rozwiniętym):
- **Rozmiar**: średnica pędzla w pikselach.
- **Współczynnik proporcji**: Zmiażdż lub rozciągnij kształt pędzla (-1,0–1,0). 0 = niezmodyfikowany; wartości ujemne obracają squash o 90°.
- **Kąt**: Obraca stempel pędzla (-180–180°). Niezależne od dynamiki kierunku skoku.
- **Twardość**: Miękkie zanikanie (0,0) do ostrej krawędzi (1,0).
- **Odstępy**: Odległość pomiędzy malowanymi punktami jako procent rozmiaru pędzla. Niższe = gładsze pociągnięcia; wyższy = wzór rozproszony.
- **Odchylenie tekstury**: Odchylenie od reakcji tekstury stempla; 50 jest neutralne. Niższe wartości sprzyjają rozbijaniu tekstury i odtłuszczonej powierzchni poprzez pociągnięcie w stronę szczytu krzywej wartości; wyższe wartości zaciskają się w kierunku pełnych wypełnień, popychając je w stronę barku. Widoczny efekt zależy od zakresu tonalnego tekstury.
- **Drżenie**: Losowe przesuwanie każdej pozycji dotknięcia o maksymalnie określoną liczbę pikseli (0–1024).
- **Gumka**: Mnożnik rozmiaru stosowany, gdy ten pędzel jest używany jako gumka (0,1–10,0). Niewidoczne na samym narzędziu Gumka.

### Dynamika

W ekspanderze **Dynamika**:
- **Dynamika**: Główne włączenie aktywnego ustawienia dynamiki.
- **Dynamics Preset**: Wybiera, które mapowania wejściowe mają być używane.
- **Pomnóż przez ciśnienie**: Przełącznik mnożenia dodatkowego ciśnienia (pokazany, gdy włączona jest dynamika).### Zachowanie po udarze
W ekspanderze **Zachowanie po uderzeniu**:
- **Nakładanie**: Gdy jest włączone, każde dociśnięcie powoduje akumulację krycia, a nie łączenie go w pojedyncze pociągnięcie.
- **Przetwarzanie końcowe**: Stosuje stabilizację, kompresję prędkości i korekcję odtwarzania po zakończeniu uderzenia, poprawiając spójność bez opóźnień.
  - **Próg skrętu**: Próg kąta (0–180°) do korekcji kierunku na ostrych narożnikach. 0 = pomiń korektę kierunku.
  - **Próg podglądu**: Pomija podgląd postprocesowy, gdy prędkość skoku przekracza tę wartość (0 = zawsze podgląd).

#### Kaligrafia

Gdy jest aktywne, stemplowanie dotykowe jest zastępowane ciągłym korytarzem geometrycznym:
- **Dynamiczne krycie**: Moduluje krycie w obrębie pociągnięcia w oparciu o zmiany prędkości i kierunku. Działa najlepiej przy delikatnych, kontrolowanych pociągnięciach; wyniki są mniej przewidywalne w przypadku szybkich bazgrołów. Eksperymentalny.
- **Wzrost prędkości** (0–100%): Maksymalny dozwolony wzrost rozmiaru na próbkę jako procent rozmiaru poprzedniej próbki. Ogranicza szybkość wzrostu dynamiki rozmiaru zależnej od prędkości, zapobiegając nagłym skokom, gdy skok przyspiesza.
- **Velocity Shrink** (0–100%): Maksymalne dozwolone zmniejszenie rozmiaru na próbkę. Ogranicza szybkość zmniejszania się rozmiaru w przypadku zmniejszania się skoku.

#### Stabilizacja i wygładzanie

- **Odległość stabilizacji kierunku** (0–100 pikseli): Minimalny ruch wskaźnika przed rozpoczęciem zachowania zależnego od kierunku, co pozwala uniknąć wczesnych skoków kąta.

#### Wygładzanie

Umożliwia wygładzanie danych wejściowych w czasie rzeczywistym, stosowane do ścieżki obrysu podczas malowania. Rozwija się, aby odsłonić:
  - **Głębokość** (2–256): Liczba poprzednich próbek wejściowych uwzględnionych przy obliczaniu wygładzonej pozycji. Wyższe wartości powodują dłuższe, bardziej zaangażowane opóźnienie.
  - **Pozycja** (0–100): Intensywność wygładzania zastosowanego do pozycji pędzla. Wyższe wartości zaokrąglają ostre zmiany kierunku.
  - **Nacisk** (0–100): Wygładzanie stosowane do sygnału nacisku igły, redukujące skoki ciśnienia i drgania.
  - **Kierunek** (0–100): Wygładzanie stosowane do kierunku pociągnięcia, stabilizujące dynamikę wrażliwą na kąt.

#### Dynamika

Przypisz dane wejściowe rysika lub inne wartości bieżące do parametrów malowania:

- **Nacisk** (rysik): steruje rozmiarem, nieprzezroczystością, szybkością, twardością, kolorem i innymi parametrami w oparciu o nacisk rysika.
- **Prędkość**: Odwzorowuje prędkość pociągnięć na właściwości pędzla.
- **Pochylenie**: Kąty pochylenia rysika w osi X i Y wpływają na kąt i inne parametry.
- **Koło**: Kółko myszy lub kółko rysika.
- **Kierunek**: Kąt kierunku skoku.
- **Zanikanie**: Zanikanie krycia lub rozmiaru po ustalonej liczbie pociągnięć.

Każde dynamiczne wejście można niezależnie przypisać do wielu właściwości. Otwórz **Opcje narzędzia** → **Dynamika**, aby skonfigurować.

### Modulacja skoku

W ekspanderze **Modulacja obrysu** (pokazywanym tylko przy włączonej opcji **Dynamika**):- **Względny kąt początkowy**: Wartość **Kąt początkowy** jest interpretowana w odniesieniu do kierunku pociągnięcia, a nie jako bezwzględny kąt obszaru roboczego.
- **Zanikanie początkowego kąta**: Zanika od **Początkowego kąta** na początku ruchu w stronę dynamicznego kąta w trakcie ruchu. Włączenie tej opcji wymusza włączenie **Względnego kąta początkowego**.
- **Początkowy kąt pędzla** (-180–180°): Kąt pędzla na samym początku pociągnięcia, zanim przejmie dynamika.
- **Początkowe mieszanie kąta** (0,0–1,0): kontroluje szybkość zmiany kąta pędzla od kąta początkowego do kąta dynamicznego. 0 = utrzymuje kąt początkowy; 1 = natychmiast wykorzystuje w pełni dynamiczny kąt.
- **Długość zanikania**: Odległość w jednostkach płótna, na której następuje zanikanie.
- **Powtórz**: Sposób powtarzania zanikania po wyczerpaniu się długości zanikania (Brak, Pętla, Piłokształtny, Trójkąt).


### Główki szczoteczek

Główki szczoteczek umieszczają wiele niezależnych główek szczoteczek na okrągłym **pierścieniu orbitalnym** wyśrodkowanym na ścieżce pociągnięć. Każda głowica maluje pełne muśnięcie w swojej własnej pozycji za każdym razem, gdy posuw jest postępowy, tworząc jednocześnie wiele równoległych lub wachlarzowych pociągnięć.

Promień orbity jest określony przez globalny rozmiar pędzla pomniejszony o rozmiar główki: większe główki znajdują się bliżej środka; mniejsze głowy krążą dalej. Głowy rozmieszczone równomiernie wokół pierścienia. Dzięki dwóm głowicom otrzymujesz po jednej z każdej strony pociągnięcia, tworząc symetryczny rozkład, który zachowuje się jak stalówka kaligraficzna. Suwak **Podążaj za kierunkiem** obraca cały pierścień tak, aby pozostawał prostopadły do ​​pociągnięcia, dzięki czemu stalówka naturalnie śledzi kierunek podczas malowania. Dodawanie kolejnych głowic stopniowo wachluje je wokół pierścienia, aż do pełnego koła natryskiwania przy 16.

Elementy sterujące pojawiają się w ekspanderze **Główki pędzli** w panelu opcji narzędzia.

- **Liczba**: Liczba jednoczesnych główek szczoteczki (1–16).
- **Rozmiar głowy**: Renderowany rozmiar każdej głowy w stosunku do globalnego rozmiaru pędzla (0,1–1,0).
- **Współczynnik kształtu orbity** (0,1–1,0): Kształtuje orbitę formacji od koła do elipsy. 1,0 = orbita kołowa; niższe wartości zgniatają oś pomocniczą.
- **Kąt formowania** (0–360°): Statyczna orientacja pierścienia formowania, stosowana, gdy **Kierunek podążania** jest niższy niż 1,0.
- **Podążaj za kierunkiem** (0,0–1,0): Jak silnie pierścień formujący śledzi kierunek ruchu skoku. Przy 1,0 pierścień jest zawsze prostopadły do ​​kierunku jazdy; przy 0,0 blokuje się na statycznej wartości **Kąta formowania**.
- **Wahania ciśnienia**: Zmienność wielkości na głowicę stosowana jako niezależne odchylenie ciśnienia na krzywych dynamiki.
- **Zmienność krycia**: Zmienność krycia na głowicę, niezależna od zmiany rozmiaru.

#### Rozproszenie

Główne elementy sterujące rozproszeniem w ekspanderze **Główki pędzli**:

- **Kąt rozproszenia** (0–360°, domyślnie 10°): Obraca tylko losowy komponent rozproszenia (nie odstępy wypełnienia). Kąty na głowę/na dotknięcie są odchylone na zewnątrz z kontrolowanym skrzyżowaniem, aby uniknąć sztywnych, lustrzanych pióropuszów. Zamocowany do 360°.
- **Odległość rozproszenia** (0–10000 pikseli): Losowe przesunięcie do przodu z pozycji wypełnienia każdej głowicy. Przewiń ponownie każde dotknięcie.
- **Balans wielkości rozproszenia** (0,0–1,0): kontroluje stromość tłumienia dla głów powyżej progu. Przy wartości 1,0 wszystkie głowy rozpraszają się równomiernie; niższe wartości w coraz większym stopniu tłumią większe głowy, podczas gdy głowy na poziomie progu/poniżej pozostają w pełnej odległości rozproszenia.

### Dodatkowe opcje

W ekspanderze **Opcje dodatkowe** (domyślnie zwiniętym) kontrolki są pogrupowane w sekcje przepełnione, które są rzadziej zmieniane. Dzięki temu główne ekspandery skupiają się na często dostosowywanych elementach sterujących malowaniem.#### Właściwości pędzla (przepełnienie)
- **Zablokuj kąt do przestrzeni ekranu**: Blokuje kąt pędzla do przestrzeni ekranu, dzięki czemu kąt pozostaje równy podczas obracania/odwracania płótna. Brak efektu, gdy dynamika kontroluje kąt.
- **Losowe obrócenie w poziomie**: 50% szans na odbicie lustrzane każdego stempla od lewej do prawej przy każdym dotknięciu.
- **Losowe odwrócenie w pionie**: 50% szans na odwrócenie każdego stempla do góry nogami przy każdym dotknięciu.
- **Losowy obrót**: Losowy obrót każdego stempla o 0°, 90°, 180° lub 270° na jedno dotknięcie.
- **Jitter Jednolity**: Gdy jest włączony, przesunięcia dotknięć z suwaka **Dźwięk** są rysowane z jednolitego rozkładu (każde przesunięcie jest jednakowo prawdopodobne w danym zakresie). Gdy opcja jest wyłączona, rozkład jest gaussowski (przesuwa klaster w stronę środka).
- **Resetuj animację**: W przypadku animowanych pędzli: po włączeniu animacja rozpoczyna się od klatki 0 przy każdym nowym pociągnięciu; gdy jest wyłączony, kontynuuje od miejsca, w którym zakończył się poprzedni skok.

#### Główki szczoteczek (przelew)

Formacja:
- **Sztywność włosia**: Jak sztywno promień orbity dopasowuje się do rozmiaru pędzla skalowanego dynamiką. 0 = orbita rozszerza się i kurczy pod wpływem ciśnienia; 1 = orbita pozostaje niezmienna w rozmiarze podstawowym.
- **Odstępy wypełnienia** (0,0–1,0): Rozkłada główki w szczelinie pomiędzy kolejnymi pozycjami docisku. Stabilna wartość charakteru każdej głowicy określa jej kierunek pochylenia; przy 1,0 głowicy wypełnij pełny odstęp odstępu. Charakter jest stabilny w każdym nasionku.

Rozproszenie:
- **Próg rozmiaru rozproszenia** (0,01–100 pikseli): promień progu dla pełnej odległości rozproszenia. Głowice w tym promieniu lub poniżej wykorzystują pełną odległość rozproszenia; większe główki są stopniowo przyciągane bliżej skoku.

Randomizacja:
- **Ziarno postaci** (0–255): Naprawiono ziarno znaku na głowę (rozmiar, pozycja wypełnienia). To samo ziarno odtwarza tę samą formację przy każdym pociągnięciu. Zmniejszono czułość, gdy włączona jest opcja **Losowa postać głowy**.
- **Losuj postać głowy**: Ponownie rysuje wartości postaci na głowę (rozmiar, położenie rozproszenia) przy każdym stemplu, dzięki czemu formacja jest całkowicie chaotyczna wzdłuż pociągnięcia. Zastępuje **Ziarno postaci**.
- **Losowe klatki animacji**: W przypadku pędzli animowanych: każda głowa niezależnie przesuwa klatkę animacji.

#### Zachowanie po uderzeniu (przepełnienie)

- **Przywróć ostatnio używane kolory**: Przywraca kolory pierwszego planu i tła z poprzedniej sesji podczas uruchamiania, zamiast domyślnych kolorów czarno-białych.
- **Prosta granica pędzla**: Używa zwykłego koła dla konturu kursora pędzla zamiast renderowania pełnego kształtu pędzla. Przydatne w przypadku złożonych lub dużych pędzli, w przypadku których narysowanie dokładnej granicy jest kosztowne.