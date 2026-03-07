---
title: "Buforowanie pędzli"
type: docs
---
Buforowanie pędzli zostało zaprojektowane tak, aby Twoje ulubione pędzle działały jak najszybciej. Zamiast ciągle przeliczać ten sam przekształcony stempel pędzla, Lumi może zachować zapisaną pamięć podręczną kształtów pędzla, których faktycznie używasz, i automatycznie ponownie załadować tę pamięć podręczną później.

## Przegląd

Ta funkcja opiera się na założeniu, że wiele wyrazistych pędzli wciąż powraca do tych samych praktycznych kombinacji rozmiaru, kąta, twardości i proporcji podczas malowania. Kiedy te kombinacje zostaną ponownie użyte, Lumi może udostępnić przekształcony stempel pędzla bezpośrednio z pamięci podręcznej, zamiast go odbudowywać.

Rezultatem jest:

- szybsze uruchamianie skoku po zapisaniu pamięci podręcznej
- płynniejsze wielokrotne korzystanie z ulubionych ustawień
- mniej zbędnych obliczeń podczas długich sesji malarskich
- automatyczne przywracanie zapisanych pamięci podręcznych przy ponownym użyciu ustawienia wstępnego

## Intencja

Buforowanie pędzli jest przeznaczone dla pędzli, do których często powracasz: podstawowych ustawień malowania, ulubionych narzędzi do malowania odręcznego, suchych pędzli z teksturą i innych pędzli, których przekształcone stemple są na tyle drogie, że można je zauważyć.

Celem nie jest wstępne wypalenie każdego teoretycznego stanu pędzla. Celem jest umożliwienie rzeczywistego użycia obrazu w pierwszej kolejności zapełnienia najcenniejszych stanów, a następnie zapisanie zapełnionej pamięci podręcznej, aby pędzel był już ciepły przy następnym użyciu.

## Jak to działa

Buforowanie pędzli współpracuje z kwantyzacją pędzli.

Gdy dla wstępnego ustawienia dynamiki włączona jest kwantyzacja, wyjścia wpływające na transformację są przyciągane do dyskretnych kroków. To daje Lumi skończony zestaw stanów pędzla wielokrotnego użytku. Podczas malowania:

1. Lumi sprawdza, czy przekształcony znaczek już istnieje w pamięci podręcznej.
2. Jeżeli tak się stanie, znaczek zostaje natychmiast wykorzystany ponownie.
3. Jeśli tak się nie stanie, Lumi zbuduje go raz i przechowuje.
4. Z biegiem czasu pamięć podręczna zapełnia się faktycznie używanymi stanami pędzli.

Jeśli zapiszesz tę pamięć podręczną, Lumi będzie mogła ją później załadować automatycznie, dzięki czemu pędzel zacznie działać bliżej stanu rozgrzanego, zamiast budować wszystko od zera.

## Typowy przepływ pracy

1. Wybierz ustawienie pędzla, którego często używasz.
2. Włącz kwantyzację dynamiki.
3. Maluj normalnie przez chwilę, aby pamięć podręczna wypełniła się organicznie.
4. Otwórz **Edytor ustawień wstępnych narzędzi** i sprawdź sekcję **Pamięć podręczna ustawień wstępnych**.
5. Obserwuj statystyki na żywo:
   - **Współczynnik trafień**
   - **Zasięg**
   - **Pamięć**
6. Kliknij **Zapisz**, gdy pamięć podręczna wygląda na wartościową.
7. Podczas późniejszych sesji Lumi automatycznie ładuje zapisaną pamięć podręczną, gdy ustawienie wstępne stanie się aktywne.

Dzięki temu ustawienie wstępne wydaje się szybsze, szczególnie w przypadku pędzli z kosztownymi transformacjami lub dużymi stemplami.

## Gdzie to znaleźć

### Edytor dynamiki

Użyj **Edytora dynamiki**, aby kontrolować kwantyzację:

- włączyć kwantyzację
- wybierz globalną liczbę kroków
- opcjonalnie zastąpić liczbę kroków na oś wyjściową

Kwantyzacja sprawia, że pamięć podręczna jest praktyczna, redukując ciągłe zmiany do pojemników wielokrotnego użytku.

### Edytor ustawień narzędzi

Użyj **Edytora ustawień narzędzi**, aby zarządzać pamięcią podręczną bieżącego ustawienia wstępnego:

- **Zapisz** — zapisz na dysku bieżącą pamięć podręczną znajdującą się w pamięci
- **Wczytaj** — przywróć wcześniej zapisaną pamięć podręczną
- **Wolna pamięć** — zwolnij pamięć podręczną w pamięci bez usuwania zapisanej kopii
- **Usuń** — usuń zapisaną pamięć podręczną z dysku

Ekspander **Preset Cache** pokazuje także współczynnik trafień na żywo, zasięg i wykorzystanie pamięci.

## Co jest buforowane

Cele buforowania pędzli przekształcone stemple pędzla: rozwiązano kosztowne rasteryzowane wyniki po rozmiarze, kącie, twardości, współczynniku kształtu i powiązanych danych wejściowych transformacji.

Jest to najbardziej przydatne, gdy:- pędzel wymaga kosztownej transformacji
- ten sam preset jest używany w wielu sesjach
- pędzel wielokrotnie powraca do podobnych stanów dynamicznych
- liczy się szybkość reakcji podczas uruchamiania

Jest mniej przydatny w przypadku pędzli, których stan transformacji zmienia się gwałtownie i rzadko się powtarza.

## Automatyczne ładowanie

Zapisane skrytki mają pomóc od początku sesji, a nie tylko po pewnym czasie malowania.

Jeśli dla aktywnego ustawienia wstępnego istnieje zapisana pamięć podręczna, Lumi może ją załadować automatycznie, dzięki czemu Twój ulubiony pędzel zacznie działać z wieloma dostępnymi już przydatnymi stanami. Skraca to okres zimnego rozruchu i natychmiastowo przybliża szczotkę do maksymalnej szybkości reakcji.

## Bezpieczeństwo pamięci

Buforowanie szczotek ma na celu poprawę prędkości bez przejmowania maszyny.

Lumi śledzi wykorzystanie pamięci podręcznej, wyświetla je w interfejsie użytkownika i stosuje limity czasu działania pod presją pamięci. Jeśli w systemie brakuje dostępnej pamięci RAM, wzrost pamięci podręcznej jest ograniczany automatycznie.

## Najlepsze przypadki użycia

Buforowanie pędzli jest szczególnie przydatne w przypadku:

- ulubione pędzle codziennego kierowcy
- teksturowane pędzle używane w całym obrazie
- duże, ekspresyjne pędzle o dużym koszcie transformacji
- wstępne ustawienia pędzli współdzielone w powtarzających się procesach tworzenia ilustracji
- presety, które chcesz czuć „gotowe” od razu po ich wybraniu

## W skrócie

Buforowanie pędzli pozwala Lumi poznać stany pędzli, których faktycznie używasz, zapisać je i automatycznie przywrócić później. Jest to praktyczna funkcja szybkości dla ulubionych ustawień wstępnych: maluj pędzlem, pozwól, aby pamięć podręczna się zapełniła, zapisz ją, a przyszłe sesje rozpoczną się szybciej.