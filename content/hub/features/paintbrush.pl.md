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

### Opcje pędzla

W ekspanderze **Opcje pędzla** (domyślnie rozwiniętym):
- **Rozmiar**: średnica pędzla w pikselach.
- **Stosunek**: Zmiażdż lub rozciągnij kształt pędzla (-1,0–1,0). 0 = niezmodyfikowany; wartości ujemne obracają squash o 90°.
- **Kąt**: Obraca stempel pędzla (-180–180°). Niezależne od dynamiki kierunku skoku.
- **Odstępy**: Odległość pomiędzy malowanymi punktami jako procent rozmiaru pędzla. Niższe = gładsze pociągnięcia; wyższy = wzór rozproszony.
- **Twardość**: Miękkie zanikanie (0,0) do ostrej krawędzi (1,0).
- **Siła**: Siła nakładania pędzla (0,0–1,0). Ukryty dla narzędzia Ołówek.
- **Drżenie**: Losowe przesuwanie każdej pozycji dotknięcia o maksymalnie określoną liczbę pikseli (0–1024).
- **Gumka**: Mnożnik rozmiaru stosowany, gdy ten pędzel jest używany jako gumka (0,1–10,0). Niewidoczne na samym narzędziu Gumka.

### Skutki udaru

W ekspanderze **Efekty obrysu**:
- **Przetwarzanie końcowe**: Stosuje stabilizację, kompresję prędkości i korekcję odtwarzania po zakończeniu uderzenia, poprawiając spójność bez opóźnień.
  - **Próg skrętu**: Próg kąta (0–180°) do korekcji kierunku na ostrych narożnikach. 0 = pomiń korektę kierunku.
  - **Preview Velocity**: Pomija podgląd postprocesowy, gdy prędkość skoku przekracza tę wartość (0 = zawsze podgląd).
- **Nakładanie**: Gdy jest włączone, każde dociśnięcie powoduje akumulację krycia, a nie łączenie go w pojedyncze pociągnięcie.#### Kaligrafia
Gdy jest aktywne, stemplowanie dotykowe jest zastępowane ciągłym korytarzem geometrycznym:
- **Szerokość** i **Wysokość**: Wymiary korytarza kaligraficznego.
- **Kąt**: Orientacja końcówki (w stopniach).
- **Dynamiczne krycie**: Moduluje krycie w obrębie pociągnięcia w oparciu o zmiany prędkości i kierunku. Działa najlepiej przy delikatnych, kontrolowanych pociągnięciach; wyniki są mniej przewidywalne w przypadku szybkich bazgrołów. Eksperymentalny.
- **Wzrost prędkości** (0–100%): Maksymalny dozwolony wzrost rozmiaru na próbkę jako procent rozmiaru poprzedniej próbki. Ogranicza szybkość wzrostu dynamiki rozmiaru zależnej od prędkości, zapobiegając nagłym skokom, gdy skok przyspiesza.
- **Velocity Shrink** (0–100%): Maksymalne dozwolone zmniejszenie rozmiaru na próbkę. Ogranicza szybkość zmniejszania się rozmiaru w przypadku zmniejszania się skoku.

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

#### Blaknięcie i kolor

W ekspanderze **Zanikanie i kolor** (zagnieżdżonym w Efektach obrysu; widocznym tylko wtedy, gdy włączony jest **System dynamiki**):

- **Względny kąt początkowy**: Wartość **Kąt początkowy** jest interpretowana w odniesieniu do kierunku pociągnięcia, a nie jako bezwzględny kąt obszaru roboczego.
- **Zanikanie początkowego kąta**: Zanika od **Początkowego kąta** na początku ruchu w stronę dynamicznego kąta w trakcie ruchu. Włączenie tej opcji wymusza włączenie **Względnego kąta początkowego**.
- **Kąt początkowy** (-180–180°): Kąt pędzla na samym początku pociągnięcia, zanim przejmie dynamika.
- **Współczynnik mieszania kąta** (0,0–1,0): kontroluje szybkość zmiany kąta pędzla od kąta początkowego do kąta dynamicznego. 0 = utrzymuje kąt początkowy; 1 = natychmiast wykorzystuje w pełni dynamiczny kąt.
- **Stabilizacja kierunku** (0–100 pikseli): opóźnia dynamikę związaną z kierunkiem, wymagając od wskaźnika przebycia określonej liczby pikseli przed aktualizacją kierunku obrysu. Aktywne tylko wtedy, gdy **Post Process** jest wyłączony (Post Process zapewnia własną stabilizację). 0 = wyłączone (natychmiastowy kierunek, może skakać na początku skoku).
- **Długość zanikania**: Odległość w jednostkach płótna, na której następuje zanikanie.
- **Powtórz**: Sposób powtarzania zanikania po wyczerpaniu się długości zanikania (Brak, Pętla, Piłokształtny, Trójkąt).


### Główki szczoteczekGłówki szczoteczek umieszczają wiele niezależnych główek szczoteczek na okrągłym **pierścieniu orbitalnym** wyśrodkowanym na ścieżce pociągnięć. Każda głowica maluje pełne muśnięcie w swojej własnej pozycji za każdym razem, gdy posuw jest postępowy, tworząc jednocześnie wiele równoległych lub wachlarzowych pociągnięć.

Promień orbity jest określony przez globalny rozmiar pędzla pomniejszony o rozmiar główki: większe główki znajdują się bliżej środka; mniejsze głowy krążą dalej. Głowy rozmieszczone równomiernie wokół pierścienia. Dzięki dwóm głowicom otrzymujesz po jednej z każdej strony pociągnięcia, tworząc symetryczny rozkład, który zachowuje się jak stalówka kaligraficzna. Suwak **Podąża za kierunkiem** obraca cały pierścień tak, aby pozostawał prostopadły do ​​pociągnięcia, dzięki czemu stalówka naturalnie śledzi kierunek podczas malowania. Dodawanie kolejnych głowic stopniowo wachluje je wokół pierścienia, aż do pełnego koła natryskiwania przy 16.

Elementy sterujące pojawiają się w ekspanderze **Główki pędzli** w panelu opcji narzędzia.

- **Liczba**: Liczba jednoczesnych główek szczoteczki (1–16).
- **Rozmiar**: Renderowany rozmiar każdej główki w stosunku do globalnego rozmiaru pędzla (0,1–1,0).
- **Kąt** (0–360°): Statyczna orientacja pierścienia formującego, stosowana, gdy **Podąża za kierunkiem** jest poniżej 1,0.
- **Wahania ciśnienia**: Zmienność wielkości na głowicę stosowana jako niezależne odchylenie ciśnienia na krzywych dynamiki.
- **Zmienność krycia**: Zmienność krycia na głowicę, niezależna od zmiany rozmiaru.
- **Sztywność**: Jak sztywno promień orbity dopasowuje się do rozmiaru pędzla skalowanego dynamiką. 0 = orbita śledzi wielkość dynamiki; 1 = orbita pozostaje niezmienna w rozmiarze podstawowym.
- **Podąża za kierunkiem** (0,0–1,0): Jak mocno pierścień formujący śledzi kierunek ruchu skoku. Przy 1,0 pierścień jest zawsze prostopadły do ​​kierunku jazdy; przy 0,0 blokuje się statyczna wartość **Kąta**.
- **Rozstawienie postaci** (0–255): Naprawiono rozstawienie postaci przypadającej na głowę (rozmiar, pozycja rozproszenia, zasięg). To samo ziarno odtwarza tę samą formację przy każdym pociągnięciu. Zniesiono czułość, gdy włączona jest **Losowa postać głowy**.

#### Interpolacja

Przesuwa głowice wzdłuż i wokół ścieżki pociągnięcia przy każdym pociągnięciu, tworząc efekty rozmazywania i rozpylania.

- **Przekroczenie** (0–5): Rozrzuca głowy do przodu wzdłuż kierunku jazdy. Przy 1,0 głowice rozprzestrzeniają się do jednego pełnego odstępu między dotknięciami do przodu; wartości powyżej 1,0 umożliwiają większy zasięg przy dużym odchyleniu rzadkości.
- **Niedoszacowanie** (0–5): To samo, co Przekroczenie, ale z tyłu za bieżącym dotknięciem. W połączeniu z Overshoot tworzy to wiodącą smugę lub ogon komety. Tłumiony przy pierwszym dotknięciu, aby uniknąć artefaktów wstecznych.
- **Kąt natrysku** (0–90°): Każda głowica jest kierowana na zewnątrz od kierunku skoku o losowy kąt na głowicę aż do tej wartości. Zaciskany pod kątem 90°, dzięki czemu żadna głowa nie jest skierowana do tyłu. Domyślnie: 10°.
- **Spray Seed** (0–255): Stałe ziarno dla kątów natrysku na głowicę, niezależne od Character Seed. Brak czułości, gdy włączony jest **Losowy wzór natryskiwania**.

#### Randomizacja

- **Losowa postać głowy**: Ponownie rysuje wartości postaci na głowę (rozmiar, położenie rozproszenia, zasięg) przy każdym dotknięciu, dzięki czemu formacja jest całkowicie chaotyczna wzdłuż pociągnięcia. Zastępuje **Ziarno postaci**.
- **Losowy wzór natrysku**: Po każdym pociągnięciu zmienia kąt natrysku, dzięki czemu wentylator przesuwa się w sposób ciągły wzdłuż skoku („żywy strumień”). Zastępuje **Rozpylanie nasion**.
- **Losowe klatki animacji**: W przypadku pędzli animowanych: każda głowa niezależnie przesuwa klatkę animacji.

### Dodatkowe opcje

W ekspanderze **Opcje dodatkowe** (domyślnie zwiniętym):- **Zablokuj do widoku**: Zachowuje wygląd pędzla w stosunku do widoku płótna: kiedy obracasz płótno, pędzel obraca się wraz z nim.
- **Prosta granica pędzla**: Używa zwykłego koła dla konturu kursora pędzla zamiast renderowania pełnego kształtu pędzla. Przydatne w przypadku złożonych lub dużych pędzli, w przypadku których narysowanie dokładnej granicy jest kosztowne.
- **Jitter Jednolity**: Gdy jest włączony, przesunięcia dotknięć z suwaka **Dźwięk** są rysowane z jednolitego rozkładu (każde przesunięcie jest jednakowo prawdopodobne w danym zakresie). Gdy opcja jest wyłączona, rozkład jest gaussowski (przesuwa klaster w stronę środka).
- **Przywróć ostatnio używane kolory**: Przywraca kolory pierwszego planu i tła z poprzedniej sesji podczas uruchamiania, zamiast domyślnych kolorów czarno-białych.
- **Losowo w poziomie**: 50% szans na odbicie każdego stempla od lewej do prawej na dotknięcie.
- **Losowy pion**: 50% szans na odwrócenie każdego stempla do góry nogami przy każdym dotknięciu.
- **Losowy obrót**: Losowy obrót każdego stempla o 0°, 90°, 180° lub 270° na jedno dotknięcie.
- **Resetuj animację**: W przypadku animowanych pędzli: po włączeniu animacja rozpoczyna się od klatki 0 przy każdym nowym pociągnięciu; gdy jest wyłączony, kontynuuje od miejsca, w którym zakończył się poprzedni skok.