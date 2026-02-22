---
title: "Mikser palet"
type: docs
---
Palette Mixer uzyskuje nowe kolory z par wpisów palety przy użyciu stałego, trzystopniowego potoku. Ponieważ mieszanie odbywa się w domenie widmowej, a nie w RGB, wyniki zachowują się jak pigmenty fizyczne: niebieski i żółty dają kolor zielony, a nasycone kolory przesuwają się w stronę neutralnego podczas mieszania.

## Rurociąg

Każdy kolor wyprodukowany przez Mikser przechodzi przez trzy etapy w ustalonej kolejności:

1. **Mieszanka**: Widmowe WGM pomiędzy rodzicem A (CCW) i rodzicem B (CW).
2. **Chroma**: Mieszaj w kierunku neutralnego spektrum palety, zmniejszając nasycenie.
3. **Ton**: Mieszaj w kierunku mieszania bieli (odcień) lub czerni (odcień).

Ton jest zawsze stosowany jako ostatni. To sprawia, że ​​dominuje jasność: regulacja tonu osiąga dokładnie zamierzony poziom jasności, bez rozcieńczania przez poprzedzającą ją regulację nasycenia barwy.

## Wybieranie rodziców

Rodzic A i Rodzic B to dwa wpisy, pomiędzy którymi miesza się suwak mieszania. Są one ładowane z mapy palet:

- Przytrzymaj **Shift** na mapie palety i **kliknij lewym przyciskiem**, aby ustawić element nadrzędny A (CCW).
- Przytrzymaj **Shift** i **kliknij prawym przyciskiem myszy**, aby ustawić rodzica B (CW).

Tylko zgłoszenia **klasy A** (mieszanki podstawowe i mieszanki niestandardowe o nienaruszonym pochodzeniu) są akceptowane jako rodzice. Wyłączone są wpisy trzeciorzędne i wpisy o utraconych przodkach.

Pozycje rodzica A i rodzica B miksera są pokazane na mapie jako wyróżnione **pierścionek z brylantem**, dzięki czemu zawsze możesz zobaczyć, które wpisy są załadowane.

## Suwaki

| Suwak | Efekt |
| :--- | :--- |
| **Mieszanka** | Przechodzi pomiędzy rodzicem A (koniec CCW) i rodzicem B (koniec CW). Przy wartości 0,0 wynik jest zgodny z rodzicem A; przy 1.0 pasuje do Rodzica B. |
| **Barwa** | Zmniejsza nasycenie mieszanki w stronę neutralności palety. Wyższe wartości dają bardziej stonowane, ziemiste rezultaty. |
| **Ton** | Przesuwa jasność w kierunku mieszania bieli (kierunek odcienia) lub mieszania czerni (kierunek cienia). |

## Kontrola wartości

**Blokada wartości** zamraża percepcyjną lekkość (CIE L\*) na bieżącym poziomie, podczas gdy pozostałe suwaki się poruszają. Użyj tej opcji, aby zbadać różnice w nasyceniu lub odcieniu bez zmiany wartości miksu.

**Band Clamp** ogranicza wynik do granic bieżącego zakresu wartości (np. w obrębie Lower Mid). Suwak tonów można nadal przeciągać, ale jasność wyjściowa jest ograniczona.

Suwak Ton odzwierciedla także wszelkie luki wartości skonfigurowane w Edytorze palet. Zakresy jasności mieszczące się w szczelinie są pokazane jako półprzezroczyste szare paski na suwaku. Uchwyt suwaka automatycznie przeskakuje nad tymi przerwami: przeciągnięcie przez szary obszar powoduje przeskok do najbliższej prawidłowej granicy pasma po drugiej stronie.

## Mieszanie punktów końcowych (biały, czarny, neutralny)

Etapy tonu i chrominancji wymagają punktów końcowych odniesienia: mieszana biel, mieszana czerń i neutralny. Lumi odkrywa je automatycznie, przeszukując aktywną paletę najlepszych kandydatów:

- **Mieszanie bieli**: podstawowy kolor o najwyższym nasyceniu, najbliższy czystej bieli.
- **Mieszanie czerni**: podstawowa najniższa jasność.
- **Neutralny**: Podstawowy najbliższy achromatycznemu (najniższe nasycenie).

Można je ręcznie zastąpić, klikając prawym przyciskiem myszy wpis w Edytorze palet.

## Zapisywanie miksuKliknij **Dodaj do palety**, aby zapisać bieżący wynik miksera jako **Zapisany miks** (wpis niestandardowy). Przed zapisaniem system stosuje **Best-Match Relocation**: przeszukuje paletę w poszukiwaniu optymalnej receptury, która pozwoli uzyskać ten sam końcowy kolor z najlepszym dopasowaniem przestrzennym na Mapie Palet. Jeśli zostanie znaleziony bliższy przepis, suwaki miksera przeskoczą, aby go odzwierciedlić, potwierdzając, że system znalazł lepsze pochodzenie, a pozycja zapisanego wpisu zrówna się z jego widoczną kropką na mapie.

Zapisane miksy przechowują pełną recepturę (nadrzędne identyfikatory UID A/B, współczynnik mieszania, ton, nasycenie), dzięki czemu można je dokładnie odtworzyć.

## Odzyskiwanie przepisu

Pojedyncze kliknięcie wpisu niestandardowego w Edytorze palet przywraca przepis tego wpisu do miksera:

- Wczytano ponownie rodzica A i rodzica B.
- Suwaki mieszania, tonu i chrominancji powracają do swoich pierwotnych pozycji.
- Dowolna blokada wartości lub zacisk pasma, która była aktywna podczas tworzenia, zostaje ponownie włączona.

Dzięki temu powrót do koloru i jego dalsze dostosowanie lub użycie go jako punktu wyjścia do nowej mieszanki jest łatwe.