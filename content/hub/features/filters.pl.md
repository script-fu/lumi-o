---
title: "Filtry"
type: docs
url: "hub/features/filters"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 312088430d35761f6df789821c1629c829e6eb1d2f8b4be58c5843c893c3c7ed
---

Menu Filtry Lumi skupia w jednym miejscu korekty korekcyjne, stylizowane efekty soczewek, generatory tekstur proceduralnych, zabiegi inspirowane drukiem i narzędzia analityczne. Kolejność menu jest raczej praktyczna niż akademicka: narzędzia do rozmycia i ulepszania znajdują się obok siebie, efekty zniekształcania i oświetlenia są pogrupowane według wyglądu, a generatory tekstur i wzorów są trzymane razem, gdy celem jest zbudowanie materiału źródłowego, a nie modyfikowanie istniejącego obrazu.

Okna dialogowe filtrów działają według tego samego ogólnego schematu działania. Ustawienia wstępne, podgląd, widok podzielony oraz elementy sterujące kryciem lub mieszaniem umożliwiają szybkie dostrojenie efektu, a na warstwach wynik może pozostać jako edytowalny, nieniszczący filtr, zamiast być natychmiast łączony. Lumi przechowuje również najnowszą historię użycia filtrów, więc powtarzanie ostatniego efektu lub ponowne otwieranie ostatniego okna dialogowego jest częścią normalnego rytmu malowania, a nie osobnym zadaniem.

## Rozmycie

### Rozmycie gaussowskie

Rozmycie gaussowskie to standardowy filtr zmiękczający Lumi: czyste, równomierne rozmycie z oddzielną kontrolą rozmiaru w poziomie i pionie, obsługą krawędzi i opcjami jądra. Jest to uniwersalny wybór w przypadku miękkiej ostrości, zmiękczonych masek, atmosferycznej głębi i wszelkich prac, w których samo rozmycie powinno pozostać neutralne.

### Pixelizuj

Pixelize redukuje szczegóły do celowych struktur blokowych zamiast miękkiego rozmycia. Ponieważ okno dialogowe wyświetla szerokość i wysokość bloku, przesunięcia, kształt pikseli i zachowanie wypełnienia, działa zarówno jako efekt zgrubnej cenzury, jak i kontrolowana mozaika lub obróbka graficzna w niskiej rozdzielczości.

### Selektywne rozmycie gaussowskie

Selektywne rozmycie gaussowskie łagodzi obszary, starając się zachować silniejsze krawędzie. Jest to przydatne, gdy obraz wymaga spokojniejszej tekstury lub mniejszych drgań bez utraty większych granic kształtu, które nadal wymagają wyraźnego odczytu.

### Rozmycie obiektywu

Rozmycie soczewkowe to jeden z filtrów rozmycia Lumi skupiających się bardziej na ilustracjach. Jego elementy sterujące opierają się na kształcie przysłony wielokątnej, krzywiźnie ostrza, rozciągnięciu anamorficznym, wzmocnieniu świateł i konfigurowalnym obszarze ostrości, dzięki czemu zachowuje się mniej jak ogólny zmiękczacz, a bardziej jak stylizowane narzędzie głębi ostrości z ukształtowanym efektem bokeh.

### Tilt-shift

Funkcja Tilt-Shift utrzymuje kontrolowany zakres ostrości ostry, jednocześnie stopniowo rozmywając obraz powyżej i poniżej. Kąt pasma okna dialogowego, jego wtapianie, odchylenie perspektywy, kształt tęczówki i wzmocnienie miniatury sprawiają, że dobrze pasuje ono do scen o miniaturowym wyglądzie, widoków architektonicznych i wszelkich kompozycji, w których ostrość powinna być odczytywana jako zaprojektowany pasek, a nie okrągły wskaźnik głębi.

### Rozmycie w ruchu kołowym

Rozmycie ruchu okrężnego rozmazuje szczegóły wokół punktu centralnego, zamieniając krawędzie w obrotowe smugi. Jest to naturalny wybór w przypadku wirujących obiektów, energii przypominającej turbinę lub ilustracji wymagających wrażenia ruchu orbitalnego.

### Liniowe rozmycie ruchu

Liniowe rozmycie ruchu rozciąga szczegóły w jednym kierunku, symulując podróż, ruch aparatu lub szybki gest w całym kadrze. Jest to szczególnie przydatne, gdy ruch musi być kierunkowy i graficzny, a nie rozproszony.

### Zoom Motion Blur

Zoom Motion Blur emituje szczegóły na zewnątrz od środka, wywołując wrażenie pędu w stronę widza lub od niego. Działa dobrze w przypadku momentów uderzenia, linii prędkości i kompozycji, które wymagają energii zoomu aparatu bez konieczności ponownego malowania całego obrazu.

## Poprawa

### High Pass

High Pass izoluje delikatny lokalny kontrast, a nie szeroką zmianę tonalną. Mając do zarządzania jedynie skalę i kontrast, jest to proste narzędzie do wydobywania szczegółów krawędzi, tworzenia wyraźnych nakładek lub przygotowywania przejść wyostrzających, które powinny bardziej podkreślać strukturę niż kolor.

### Redukcja hałasu

Redukcja szumów to działanie odwrotne: tłumi niepożądane drobne różnice, dzięki czemu większe formy są czytelniejsze. Jest to przydatne, gdy zeskanowany materiał, skompresowane tekstury lub przepracowane fragmenty wymagają uproszczenia przed dalszym malowaniem lub filtrowaniem.

### Wyostrz

Sharpen wykorzystuje model maski wyostrzającej z promieniem, wielkością i progiem kontrolującymi siłę lokalnego kontrastu. W praktyce sprawia to, że nadaje się do przywracania przejrzystości po rozmyciu, zmianie rozmiaru eksportu lub subtelnych przejściach wykończeniowych, gdy szczegóły muszą zostać wydobyte bez zamieniania każdego piksela w szum.

## Kolor

### Tonal Grading

Gradacja tonalna odwzorowuje kolory według zakresu tonalnego, a nie poprzez zmianę kształtu kontrastu lub rysowanie krzywej. Jasność każdego piksela wybiera płynne połączenie trzech kolorów użytkownika dla cienia, półtonów i podświetleń; dzięki czemu obraz zachowuje strukturę od jasnej do ciemnej podczas zmiany palety. Siła dla poszczególnych regionów, odchylenie równowagi w stylu Lightrooma (po lewej stronie faworyzuje stopień cienia, po prawej stopień rozjaśnienia) i miękkość przejścia kontrolują, jak daleko sięga każdy kolor i jak delikatnie nakładają się stopnie. Jest skierowany do ilustracji, komiksów, grafik koncepcyjnych i fotografii, gdy celem jest spójna ocena lub wygląd.

## Zniekształcanie

### Aberracja chromatyczna

Aberracja chromatyczna oddziela kanały kolorów na zewnątrz od wybranego środka, z kontrolą kierunku promieniowego lub stycznego, odchylenia między parami kanałów, zaniku i zachowania luminancji. Zarówno kod, jak i okno dialogowe traktują to jako narzędzie dwukierunkowe: może dodać stylizowane obramowanie soczewki w celu uzyskania energii lub odwrócić znak, aby skorygować łagodną aberrację w materiale źródłowym.

### Zniekształcenie obiektywu

Zniekształcenie obiektywu zmienia kształt obrazu poprzez krzywiznę beczkowatą lub poduszkową, warunki krawędziowe, kompensację powiększenia, przesunięcie środka i rozjaśnienie narożników. Dzięki temu jest przydatny zarówno do korygowania obrazu, który wydaje się optycznie wygięty, jak i do celowego nadawania mu charakteru obiektywu szerokokątnego lub retro.

## Oświetlenie

### Rozkwit

Bloom zamienia jasne obszary w kontrolowaną poświatę, a próg, miękkość, promień i siła określają, jak daleko rozprzestrzenia się światło i jak mocno podnosi obraz. Dodatkowa kontrola ograniczająca ekspozycję sprawia, że ​​można go używać jako efektu podświetlenia, a nie automatycznego rozmycia.

### Niebo

Niebo to coś więcej niż nakładka z odcieniem lub gradientem: renderuje niebo analityczne przy użyciu modeli Preetham, Hosek/Wilkie lub Nishita. Ponieważ okno dialogowe wyświetla projekcję, kąt słońca, zmętnienie, gęstość atmosfery, wysokość nad poziomem morza, elementy sterujące tarczą słoneczną i ekspozycję, może zbudować wszystko, od prostego, przejrzystego tła po bardziej fizycznie ugruntowane niebo o zachodzie słońca lub zmierzchu.

### Winieta

Winieta przyciemnia, koloruje, a nawet wymazuje w kierunku krawędzi obrazu, korzystając z elementów sterujących kształtem, promieniem, miękkością, gamma, proporcjami, ściśnięciem, obrotem i pozycjonowaniem na płótnie. Działa jak klasyczna obróbka krawędzi fotograficznych, ale jest na tyle elastyczna, że ​​może działać jako maska ​​​​kadrująca lub nieregularny reflektor kompozycyjny.

## Hałas

### Hałas HSV

Szum HSV niezależnie losuje odcień, nasycenie i wartość. Dzięki temu jest przydatny, gdy obraz wymaga ożywienia kolorów lub niestabilności analogowej bez całkowitego rozbijania lokalnej struktury.

### Hurl

Hurl to ekstremalna wersja szumu: zastępuje piksele całkowicie losowymi kolorami. Najlepiej myśleć o tym jako o niszczycielskim źródle chaosu powodującym usterki, uszkodzone tekstury lub maski wymagające agresywnego rozbicia.

### Wybierz

Pick zastępuje każdy piksel losowo wybranym sąsiadem, dzięki czemu obraz pozostaje powiązany ze swoim źródłem, zamiast stać się czysto statyczny. Rezultatem jest tasowana, ziarnista odmiana, która może wydawać się bardziej organiczna niż w pełni losowy szum.

### Rozprzestrzenianie się

Rozprzestrzenianie powoduje rozproszenie pikseli poprzez losowe przemieszczanie ich w promieniu. Jest to przydatne, gdy potrzebujesz nieruchomego zakłócenia: popękanej powierzchni, rozmazanej krawędzi lub zniszczonej tekstury, która nadal zachowuje relacje kolorystyczne obrazu źródłowego.

### Fraktal

Fractal generuje fraktalny szum Perlina, który można kafelkować, co czyni go szczególnie cennym źródłem wielokrotnego użytku dla masek, chmur, tekstur papieru, rozpadu przypominającego teren i nakładek proceduralnych. Ponieważ układa się w kafelki, może obsługiwać większe przepływy pracy bez tworzenia widocznych szwów.

### Niebieskie ziarno szumu

Blue Noise Grain to monochromatyczny generator ziarna Lumi w stylu filmu i druku. Ustawienia wstępne dotyczące wielkości ziarna, maskowania niebieskiego szumu, odchylenia tonów średnich, odchylenia cienia i kontroli nasion pokazują, że okno dialogowe zostało zaprojektowane tak, aby równomiernie i w sposób kontrolowany rozmieszczać ziarno, a nie tylko rozpryskiwać przypadkowe monochromatyczne plamki na obrazie.

### Ziarno risografu

Risograph Grain opiera się na tej samej logice ziarna, ale przekształca ją w efekt druku na dwóch płytach. Oddzielne kolory atramentu, balans płyt, celowa błędna rejestracja i początkowe różnice sprawiają, że dobrze nadaje się do prac plakatowych, estetyki druku niezależnego i ilustracji, które powinny sprawiać wrażenie fizycznie nadrukowanego, a nie idealnego cyfrowo.

### Półtony (FM)

Półtony (FM) tworzą stochastyczny, modulowany częstotliwościowo półton przy użyciu szumu niebieskiego lub pokrewnych metod progowania. Dzięki trybom kolorów dla monochromatycznego, duotone i CMYK, a także kontroli wzmocnienia punktu i dekorelacji płyty, ma na celu uzyskanie tekstury przypominającej wydruk, która pozostaje nieregularna i żywa, a nie wpada w sztywną siatkę.

## Krawędzie

### Różnica Gaussa

Różnica Gaussa wykrywa krawędzie, odejmując od siebie dwie rozmyte wersje obrazu. Jest to kompaktowy, przydatny operator do tworzenia map krawędzi, wyodrębniania stylizowanych linii i znajdowania przejść strukturalnych bez konieczności stosowania pełnego zarysu progowego.

## Morfologia

### Mediana

Mediana zastępuje każdy piksel wartością medianą z jego sąsiedztwa, co ma tendencję do usuwania izolowanego szumu przy jednoczesnym zachowaniu silniejszych granic lepiej niż zwykłe rozmycie. Jest to praktyczny filtr czyszczący, służący do spłaszczania drobnych zakłóceń wizualnych bez natychmiastowego zmiękczania całego obrazu.

### Rozciągnij

Dilate powiększa jaśniejsze obszary na zewnątrz, korzystając z tej samej logiki sąsiedztwa uwzględniającej kształty. Jeśli chodzi o tworzenie obrazu, może zagęścić jasne ślady, rozszerzyć jasne kształty lub zamknąć małe ciemne szczeliny.

### Erodować

Erode wykonuje ruch uzupełniający, powiększając ciemniejsze obszary i odciągając jaśniejsze. Przydaje się do rozjaśniania jasnych detali, powiększania ciemnych brył czy zaostrzania masek i kształtów graficznych.

## Wzór

### Szachownica

Szachownica generuje regularny, naprzemienny wzór płytek. Jest to proste, ale dzięki tej prostocie jest przydatne do testowania przezroczystości, budowania masek, blokowania graficznych teł lub tworzenia czystego geometrycznego materiału źródłowego.

### Grid

Grid rysuje powtarzające się podziały poziome i pionowe, dzięki czemu jest przydatna w przypadku prowadnic układu, tła projektowego, ilustracji technicznych i maskowania proceduralnego. Ponieważ jest generowany jako filtr, odstępy i wygląd można dostosować bez ręcznego tworzenia wzoru.

### Woronoj

Voronoi generuje teksturę komórkową z możliwością kafelkowania z rozmieszczonych punktów, z kontrolą typu obiektu, metryki odległości, losowości, szczegółów fraktalnych i płynnego zawijania. W praktyce może przejść od czystych struktur pękniętych komórek do bardziej organicznych wzorów kamienia, skóry, mapy lub abstrakcyjnych wzorów sieci.

### Fala

Fala tworzy wzory pasmowe lub pierścieniowe ukształtowane na podstawie profilu fali, układu geometrycznego, zniekształceń, szczegółów fraktalnych i przesunięcia fazowego. To sprawia, że ​​jest to coś więcej niż proste narzędzie do tworzenia pasków: może generować kontrolowane zmarszczki, pasma topograficzne, grafikę przypominającą morę lub zaszumione koncentryczne pola wzorów.

### Półtony (AM)

Półtony (AM) wykorzystują klasyczny raster punktowy z modulacją amplitudy, z częstotliwością, kształtem punktu, ostrością, trybem koloru i kontrolą kąta CMYK, co pozwala uzyskać strukturę druku w stylu rozety. W porównaniu z półtonowaniem FM jest to bardziej uporządkowana, rozpoznawalna mechaniczna opcja, gdy pożądany wygląd to papier gazetowy, litografia offsetowa lub celowo widoczna geometria rastra.