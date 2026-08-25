---
title: "Format pliku (.lum)"
type: docs
url: "hub/features/file-format"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: c5e414a0d2870f1b111751c8c462e7ac5a4530103d70cb9be52a9fbd11417028
---

Natywny format `.lum` w Lumi to katalog projektu, nie pojedynczy zamknięty plik. Powstał z myślą o ilustracji warstwowej: głębokich drzewach warstw, dużych płótnach, maskach, nieniszczących efektach i punktach kontrolnych, które nie muszą duplikować całego obrazu.

Zadaniem formatu jest utrzymać tę roboczą strukturę — żeby projekt można było otworzyć wiernie, sprawdzić, gdy coś pójdzie nie tak, i odzyskać z niedawnego punktu kontrolnego, nie traktując grafiki jak jednej nieprzejrzystej bryły.

## Osobne części — celowo

Projekt `.lum` to folder. Drzewo warstw i właściwości obrazu są w czytelnym XML. Każda warstwa i każda maska ma własny bufor pikseli, nazwany według grafiki, a nie według wewnętrznego identyfikatora. Ścieżki wektorowe zapisuje się zwykłym SVG. Ciężkie ustawienia filtrów są we własnych plikach obok obrazu. Profile ICC są przechowywane raz, w katalogu głównym projektu, więc migawki odzyskiwania mogą się do nich odwoływać zamiast je kopiować.

To rozdzielenie umożliwia resztę formatu. Niezmienione warstwy można zostawić na dysku w spokoju. Uszkodzony bufor psuje się sam, zamiast pociągnąć za sobą cały plik. Brakujące piksele warstwy stają się pustymi warstwami, które nadal mają nazwy, pozycje i ustawienia mieszania; brakujący podgląd złożony grupy odtwarza się z warstw podrzędnych. Projekt pozostaje mapą tego, jak powstał obraz.

Palety pigmentów należą do narzędzi koloru Lumi. Projekt może pamiętać, która paleta była powiązana z obrazem, ale sama biblioteka palet jest poza `.lum`.

## Stan edycji, nie spłaszczenie

Plik przechowuje roboczy obraz. Warstwy pozostają warstwami, grupy warstw grupami, a maski maskami — wraz z przesunięciami, blokadami, zachowaniem mieszania i stosami filtrów. Filtry nieniszczące zapisuje się jako operacje i parametry, nie jako już wkomponowane piksele. Warstwa o jednolitym płaskim kolorze w ogóle nie potrzebuje pliku pikseli.

Zwinięte grupy zachowują też złożony widok samych siebie. Ten zapisany podgląd złożony pojawia się na płótnie, gdy grupa jest zamknięta, więc warstw podrzędnych nie trzeba odtwarzać tylko po to, by spojrzeć na obraz. Tryby podglądu wyłącznie do inspekcji zostają poza tym zapisem: wyświetlanie maski lub alfy do edycji wraca jako metadane, a nie zostaje wpisane w zapisaną grupę.

## Duże pliki mogą pozostać częściowo na dysku

Otwarcie `.lum` nie wymaga wczytania wszystkich pikseli. Zawartość zwiniętych grup może pozostać na dysku, a zapisany podgląd złożony grupy pokazuje się od razu. Dopiero rozwinięcie grupy wczytuje te warstwy, maski i zagnieżdżone grupy do pamięci. Grupy, które pozostają zamknięte, pozostają lekkie.

Plik zapisuje też, które grupy były faktycznie w użyciu. Grupy na ścieżce aktywnego zaznaczenia mogą otworzyć się rozwinięte; pozostałe grupy są zapisane jako zwinięte, nawet jeśli w poprzedniej sesji akurat były otwarte. Dzięki temu głęboki plik nie wczytuje do pamięci każdej nieużywanej gałęzi w chwili otwarcia.

Grupowanie jest więc wyborem wydajnościowym, nie tylko organizacyjnym. Duże tła, zarchiwizowane eksperymenty i nieużywane warianty mogą leżeć w zamkniętych grupach, nie zajmując tej samej pamięci co warstwy, na których się maluje. Zapisywanie działa według tej samej zasady: nadal ukryte bufory kopiuje się lub pomija jako pliki, zamiast wpychać je z powrotem do pamięci tylko po to, by znów je zapisać.

## Punkty kontrolne zapisują tylko zmiany

Plik → Zapisz aktualizuje projekt roboczy. Zapisy przyrostowe i Autozapis trafiają do drzewa odzyskiwania i zapisują wyłącznie zmienione dane — zmodyfikowane bufory warstw, nie drugą kopię całego obrazu. Każdy punkt kontrolny niesie jednak pełny opis drzewa warstw, więc dowolny moment tego ciągu można otworzyć, uzupełniając niezmienione piksele ze starszych punktów kontrolnych i, w razie potrzeby, z samego pliku roboczego.

Autozapis stosuje ten sam wzorzec w osobnej pamięci podręcznej, więc automatyczna ochrona nie musi przepisywać pliku na dysku. Jeśli przy otwieraniu projektu istnieją nowsze punkty kontrolne niż ostatni pełny zapis, Lumi może je zaproponować zamiast po cichu odrzucać świeższą pracę. Odzyskane obrazy otwierają się pod inną nazwą, żeby szybki zapis nie nadpisał oryginału.

## Format roboczy

`.lum` służy do kontynuowania malowania w Lumi. Formaty spłaszczone i formaty zgodności służą publikacji, przekazaniu i innym programom. Ponieważ projekt to katalog wielu plików, warto go zarchiwizować, gdy ma podróżować.

Plik roboczy pozostaje bogaty i edytowalny. Eksport to sposób, w jaki skończony lub udostępniany obraz opuszcza tę strukturę.
