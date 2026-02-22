---
title: "Edytor palet"
type: docs
---
Edytor palet to miejsce, w którym tworzysz paletę Lumi i zarządzasz nią. Przechowuje zestaw pigmentów, przechowuje mieszanki zapisane w Mikserze palet, rejestruje kolory faktycznie użyte podczas malowania oraz pozwala skonfigurować strukturę wartości i gradienty palety.

## Wybieranie palety

Paleta to coś więcej niż zbiór pigmentów: to zobowiązanie stylistyczne. Wielu artystów pracuje z małym, stałym zestawem pigmentów, które dobrze znają: sposób, w jaki się mieszają, wytwarzane przez nie neutralne odcienie, różnice temperatur między nimi. Ta znajomość staje się częścią ich wizualnego głosu. Malarz może zachować ciepłą paletę o niskim nasyceniu do prac nad figurami i oddzielną paletę high-key do krajobrazów lub może wykonać całą pracę w ramach jednego zestawu czterech pigmentów, co stanowi celowe ograniczenie ujednolicające całość pracy.

Lumi wspiera ten sposób pracy. Każda paleta ma własne pigmenty, mieszanki, strukturę wartości i gradienty. Przełączanie palet zmienia cały system kolorów: Mapa, Mikser i dostępne miksy są aktualizowane, aby odzwierciedlić nowy zestaw.

Lista rozwijana u góry Edytora palet umożliwia wybranie aktywnej palety. Lumi jest dostarczana z trzema paletami w grupie **Standard**:

| Paleta | Charakter |
| :--- | :--- |
| **Domyślne** | Wszechstronna, ciepła paleta obejmująca pełne koło odcieni. Dobry punkt wyjścia dla większości przedmiotów. |
| **Mistrz** | Duża paleta o pełnym spektrum dla malarzy, którzy chcą maksymalnego krycia odcieni i wyraźnej kontroli nad osiami szarości. |
| **Zorn** | Limitowana czteropigmentowa paleta oparta na podejściu Andersa Zorna. Obejmuje zaskakująco szeroką gamę ciepłych odcieni skóry i neutralnych odcieni o niskim nasyceniu przy użyciu minimalnego zestawu pigmentów. |

Palety można także tworzyć, importować i powielać na karcie Palety.

## Pigmenty paletowe

Sekcja **Pigmenty palety** u góry widoku palety zawiera listę podstawowych wpisów: pigmentów bazowych, z których zbudowana jest reszta palety. Są to wejścia do systemu mieszania widmowego. Elementy drugorzędne i trzeciorzędne są generowane z nich automatycznie i wykorzystywane do wypełniania mapy palet

## Zapisane miksy

Sekcja **Zapisane mieszanki** zawiera kolory, które wyraźnie zachowałeś z Miksera palet za pomocą opcji **Dodaj do palety**. Oto kolory pochodne: wyniki mieszania widma, regulacji tonu i nasycenia barwy zapisane do ponownego wykorzystania.

Zapisane miksy są podzielone na pięć zakresów wartości:

| Zespół | Domyślny zakres jasności |
| :--- | :--- |
| Wysoki klucz | 80 – 100% |
| Górna połowa | 60 – 80% |
| Środek | 40 – 60% |
| Niższy środek | 20 – 40% |
| Głęboko | 0 – 20% |

Lumi automatycznie umieszcza każdy zapisany miks w odpowiednim paśmie w oparciu o jego percepcyjną lekkość (CIE L\*). Pozwala to uporządkować miksy według wartości, zamiast przeszukiwać płaską listę i zazwyczaj odpowiada sposobowi, w jaki artysta myśli o kolorze.

Nazwę zapisanych miksów można zmienić za pomocą przycisku **Zmień nazwę niestandardową** lub menu kontekstowego.

## Używane mieszanki

Sekcja **Używane mieszanki** to historia wywołana farbą. Za każdym razem, gdy kolor z palety jest nakładany na płótno, jest on rejestrowany w tym miejscu. Używane mieszanki są uporządkowane od najnowszych do najnowszych.

Ta sekcja jest przydatna do odzyskiwania koloru, którym namalowałeś, ale który nie został jawnie zapisany. Aby zachować używany miks na stałe, wybierz go i kliknij **Promuj**, a zostanie on przeniesiony do zapisanych miksów w odpowiednim przedziale wartości.

Używane mieszanki są przechowywane na palecie i pozostają pomiędzy sesjami.

## Pasma wartościPasma wartości określają, gdzie przebiegają granice pomiędzy pięcioma strefami jasności. Domyślnie dzielą jasność równomiernie w zakresie 0–100%, ale można je dostosować, aby dopasować je do struktury tonalnej fotografowanego obiektu. Dla malarzy przydatne jest definiowanie i zarządzanie przedziałami wartości _i_ przerwami między nimi.

### Suwak pasma wartości

**Ekspert pasm wartości** w Edytorze palet zawiera suwak z pięcioma przeciąganymi dzielnikami. Przeciągnij dowolny rozdzielacz, aby przesunąć granicę pomiędzy sąsiednimi pasmami. Etykieta nad suwakiem pokazuje nazwę i dokładny zakres procentowy aktywnego pasma.

**Przyciski:**

| Przycisk | Efekt |
| :--- | :--- |
| **Anuluj** | Przywraca suwak do ostatnio zastosowanego stanu |
| **Kopiuj** | Kopiuje bieżącą konfigurację pasma do schowka |
| **Wklej** | Wkleja skopiowaną konfigurację pasma z innej palety |
| **Domyślne** | Przywraca fabryczne ustawienia równego podziału |
| **Zastosuj** | Zatwierdza zmiany i regeneruje paletę |

Aby zmiany były trwałe, wymagane jest **Zastosuj**. Uruchamia pełną regenerację palety i usuwa wszystkie zapisane miksy, których jasność nie mieści się już w żadnym paśmie. Lumi wyświetli okno dialogowe z potwierdzeniem, ile miksów zostanie usuniętych przed kontynuowaniem.

### Pasma wartości i mapa palet

Mapa palet wyświetla paletę w postaci koła odcieni z 36 sektorami odcieni (10° każdy) i 15 komórkami jasności rozmieszczonymi jako koncentryczne pierścienie. Każde pasmo odpowiada trzem pierścieniom: pięć pasm × 3 pierścienie = łącznie 15 komórek.

Dostosowanie pasm wartości powoduje zmianę wartości jasności znajdujących się na każdym poziomie pierścienia. Pasek ściśnięty w kierunku ciemnego końca sprawia, że ​​jego trzy pierścienie obejmują węższy zakres tonalny; szerokie pasmo zapewnia trzem pierścieniom większy rozrzut tonalny. W ten sposób ta sama struktura mapy palet dostosowuje się do palet dostrojonych pod kątem różnych priorytetów tonalnych.

## Gradienty palety

Każda paleta może przechowywać jeden lub więcej **Gradientów**: płynnych przejść pochodzących z wpisów palety, które można zastosować na płótnie jako wypełnienia gradientowe lub wykorzystać jako paski odniesienia.

Gradientami zarządza się w **ekspansie Gradientów**. Kombinacja u góry wyświetla gradienty w bieżącej palecie. **Dodaj** tworzy nowy gradient. **Usuń** usuwa wybrane. **Zmień nazwę** zmienia nazwę.

### Edytor gradientów

**Ekspert Edytora Gradientów** konfiguruje wybrany gradient. Każdy gradient ma trzy punkty końcowe (**A**, **B** i **C**) wyświetlane jako próbki kolorów. Kliknij próbkę, aby stała się aktywnym punktem końcowym do edycji.

Każdy punkt końcowy można ustawić, klikając **Wybierz**, a następnie klikając pozycję palety na mapie palet lub w widoku palety. Punkt końcowy jest powiązany z tą pozycją palety poprzez UID; jeśli wpis ulegnie zmianie, gradient zostanie zaktualizowany.

**Kontrola na punkt końcowy:**

| Kontrola | Efekt |
| :--- | :--- |
| **Siła** | Jak duży wpływ ma kolor punktu końcowego w porównaniu z sąsiadami |
| **Krycie** | Alfa koloru punktu końcowego w gradiencie |
| **Krzywa** | Korekta gamma dla zaniku koloru od tego punktu końcowego |

**Suwaki rozkładu** (S1, S2, S3) ustawiają miejsce, w którym trzy punkty środkowe pomiędzy punktami końcowymi padają wzdłuż paska gradientu. Zresetowanie ich przywraca równe odstępy między punktami środkowymi.

Pasek podglądu gradientu u góry bloku Edytora gradientów pokazuje wynik bieżącego punktu końcowego i ustawień rozkładu.

## Możliwość dokowania paletyDokowany panel **Paleta** (**Panele > Paleta**) to prostszy panel przeznaczony do odczytu, służący do przeglądania i wybierania kolorów z dowolnej palety. Pokazuje ten sam widok składający się z trzech sekcji (Pigmenty palety, Zapisane mieszanki, Wykorzystane mieszanki) bez rozwijaczy Pasma wartości i Gradienty.

Lista rozwijana wyboru palet u góry umożliwia przełączanie pomiędzy wszystkimi dostępnymi paletami. Kliknij dowolny wpis, aby ustawić go jako kolor pierwszego planu. Kliknij dwukrotnie, aby otworzyć edytor nazw kolorów. W przypadku palet zapisywalnych na pasku przycisków dostępne są akcje Edytuj kolor, Nowy kolor z FG i Usuń kolor.

Dokowalna paleta jest przeznaczona do szybkiego dostępu do kolorów podczas malowania, gdy pełny edytor palet zajmowałby zbyt dużo miejsca.

## Zakładka Palety

**Karta Palety** (dostępna jako zakładka dokowalna) pokazuje aktywną paletę w trybie kompaktowym. Nie obejmuje pigmentów skupiających się na zapisanych mieszankach