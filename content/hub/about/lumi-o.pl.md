---
title: "Lumi-o"
type: docs
weight: 1
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 060cd1cd6261e7278b4bc6e6c7330a374ecb20b5368faedde03a6e15da424d12
url: "hub/about/lumi-o"
---

Lumi to szybka, wydajna aplikacja wyłącznie na Linuksa do tworzenia obrazów rastrowych, rozwijana otwarcie. Decyzje projektowe, dokumenty architektury i historia rozwoju są publiczne, dzięki czemu użytkownicy mogą śledzić, jak program się zmienia.

Lumi stawia na cyfrowe malarstwo i ilustrację, pozostając jednocześnie zdolne do ustrukturyzowanej edycji obrazu i korekty fotograficznej. Korekty, efekty stylu obiektywu, gradacja kolorów oparta na tonach i nieniszczące warstwy filtrów obsługują szeroki zakres procesów edycyjnych.

Lumi opiera się na założeniu, że oprogramowanie do cyfrowego malowania powinno zachowywać się jak niezawodne narzędzie studyjne: przewidywalne, przejrzyste i skupione na samym tworzeniu obrazów.

## Cel

Lumi wspiera ustrukturyzowaną, nieniszczącą metodę tworzenia obrazów — niezależnie od tego, czy źródło jest malowane, rysowane czy fotograficzne. To ukierunkowana, przemyślana alternatywa zarówno dla ogólnych edytorów obrazów, jak i dedykowanego oprogramowania do malowania: bezpłatna i open source, bez subskrypcji, uzależnienia od dostawcy, chmury ani generowania obrazów przez AI.

Niezawodność i długoterminowy dostęp to podstawowe cechy. Otwarty format plików oparty na katalogach pozostaje czytelny bez oprogramowania zamkniętego; obsługiwany jest import i eksport XCF oraz PSD.

## Podstawy artystyczne

Lumi rozwija niezależny artysta z doświadczeniem w sztuce pikselowej, tradycyjnym rysunku i malarstwie, tworzeniu gier, sztuce technicznej, ilustracji i animacji 3D. To tło kształtuje podejście do koloru, linii, warstw, wydajności, odzyskiwania danych, skryptów i doświadczenia użytkownika.

## Filozofia

Lumi łączy system kolorów oparty na pigmentach z responsywnym, nieniszczącym, warstwowym przepływem pracy. System kolorów to świadome odejście od konwencjonalnych suwaków HSV i dowolnych selektorów RGB.

- **Kolor zorientowany na pigment**: profile pigmentów ze świata rzeczywistego (kody Colour Index) są mieszane widmowo, dzięki czemu palety zachowują się bardziej jak prawdziwa farba.
- **Przepływ pracy oparty na paletach**: zapisane, przełączalne palety porządkują pigmenty, mieszanki, pasma wartości i gradienty, utrzymując spójność decyzji kolorystycznych w obrazie lub projekcie.
- **Dotykowe, skupione narzędzia**: pędzle łączą nacisk, nachylenie i prędkość rysika, dając bezpośrednią, dopracowaną kontrolę. Tekstura powierzchni może nadać pociągnięciu ziarno i ząb powierzchni, także przy pociągnięciach kaligraficznych, a skręt może przesunąć to ziarno. Pędzle z włosia można ściśle kształtować i animować wzdłuż pociągnięcia.
- **Responsywne płótno**: OpenGL może rysować stos warstw i malować pociągnięcia pędzla dla poszczególnych presetów narzędzi. Duże aerografy i duże stemple tekstury zostają przy ręce, a poruszanie się po płótnie pozostaje płynne.
- **Nieniszcząca niezawodność**: warstwy i edytowalne filtry skalują się do złożonych projektów, pozostając przewidywalne. Automatyczny zapis, szybkie zapisywanie, zapisy przyrostowe i odzyskiwanie chronią długie sesje malarskie i duże projekty.
- **Aktywne obszary robocze**: nazwane profile zachowują dokowania, narzędzia, ustawienia wstępne, palety i powiązania urządzeń, a następnie przełączają je atomowo w czasie działania.
- **Skrypty Scheme**: Lumi rozszerza tradycję Script-Fu o język wtyczek oparty na Scheme oraz dodatkowe funkcje pomocnicze do tworzenia wtyczek i automatyzacji przepływów pracy.

[Filtry](/hub/features/filters/) — w tym rozmycie bokeh o kształcie obiektywu, tilt-shift, gradacja tonalna, wyostrzanie i redukcja szumów — mogą pozostać edytowalne obok pracy pędzlem.

## Granice

- **Skupione, nie wyczerpujące**: Lumi nie jest przeznaczone do projektowania stron, DTP ani każdej niszy, którą próbuje objąć szeroki edytor taki jak GIMP.
- **Tylko Linux**: Lumi jest rozwijane na Debianie z Cinnamon (X11). GNOME na Waylandzie zostało sprawdzone pod kątem ogólnego wyglądu aplikacji i renderowania OpenGL. Windows i macOS nie są obsługiwane.

## Podziękowania

Lumi powstało na fundamencie GNU Image Manipulation Program (GIMP). Lumi docenia i jest głęboko wdzięczne za wieloletnią pracę twórców, artystów i współtwórców.

![Lumi logo placeholder](/images/lumi.png)
