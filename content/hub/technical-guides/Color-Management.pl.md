---
title: "Zarządzanie kolorami"
type: docs
weight: 15
---
Lumi-o jest skonfigurowany do pracy od razu po wyjęciu z pudełka. Jeśli pracujesz nad obrazem z **16-bitową lub większą precyzją**, oprogramowanie jest już skonfigurowane do korzystania z domyślnego pakietu soft proofingu (CMYK) i wbudowanych profili sRGB; wszystko powinno działać bez żadnej konfiguracji.

Dla tych, którzy potrzebują większej kontroli, ten przewodnik wyjaśnia podstawowy model zarządzania kolorami Lumi, różnicę między profilem obrazu a profilem testowym, gdzie znajdują się elementy sterujące oraz dokładnie, w jaki sposób profile domyślne łączą się z aplikacją.

## Szybkie podsumowanie

Lumi wykorzystuje trzy różne role profilowe:

1. **Profil roboczy obrazu**
   - Definiuje znaczenie liczb RGB lub skali szarości obrazu.
   - Używany do operacji przypisywania/konwertowania.
   - Typowe przykłady: wbudowana sRGB, Adobe RGB.

2. **Wyświetl profil**
   - Opisuje Twój monitor.
   - Służy do prawidłowego wyświetlania obrazu na ekranie.
   - Zwykle dostarczane przez system lub wybierane w Preferencjach.

3. **Profil miękki**
   - Symuluje inne urządzenie wyjściowe lub warunki drukowania.
   - Czy **nie** redefiniuje wartości pikseli obrazu.
   - Typowe przykłady: profile prasowe CMYK, takie jak `CoatedFOGRA39`.

## Profil obrazu a profil miękki

### Profil obrazu

Użyj tej opcji, jeśli chcesz powiedzieć Lumi, w jakiej przestrzeni kolorów faktycznie znajduje się obraz.

Dwie typowe operacje:

- **Przypisz profil**
  - Zmienia etykietę profilu dołączoną do obrazu.
  - Czy **nie** konwertuje wartości pikseli.
  - Używaj tylko wtedy, gdy numery pikseli znajdują się już w przestrzeni tego profilu.

- **Konwertuj na profil**
  - Konwertuje wartości pikseli z bieżącego profilu obrazu na nowy.
  - Użyj, jeśli chcesz, aby obraz rzeczywiście przeniósł się do innej przestrzeni roboczej.

**Lokalizacja menu:**
- Obraz > Zarządzanie kolorami > Przypisz profil kolorów...
- Obraz > Zarządzanie kolorami > Konwertuj na profil kolorów...

### Profil odporny na miękkość

Użyj tej opcji, jeśli chcesz wyświetlić podgląd sposobu reprodukcji obrazu na urządzeniu docelowym lub warunków drukowania.

Miękkie sprawdzanie:
- pozostawia przestrzeń roboczą obrazu w spokoju
- zmienia potok podglądu
- potrafi zaznaczyć kolory spoza gamy
- służy do podglądu, a nie ponownego przypisania danych obrazu

**Lokalizacja menu:**
- Obraz > Zarządzanie kolorami > Ustawienia próbne miękkie > Wybierz profil próbny miękkie...
- Obraz > Zarządzanie kolorami > Ustawienia próbne > Zamiar renderowania
- Obraz > Zarządzanie kolorami > Ustawienia próbne > Kompensacja czarnego punktu
- Widok > Zarządzanie kolorami > Włącz podgląd próbny programowy
- Widok > Zarządzanie kolorami > Zaznacz poza gamą kolorów

## Jak wyświetlić podgląd wersji miękkiej

Istnieją dwa główne punkty wejścia do przełączania wydruków próbnych.

### 1. Wyświetl menu

Użyj:
- Widok > Zarządzanie kolorami > Włącz podgląd próbny programowy

Włącza lub wyłącza symulację podglądu dla bieżącego ekranu.

### 2. Przełączanie paska stanu

Lumi udostępnia także soft proofing bezpośrednio w dolnym pasku stanu.

- **Kliknięcie lewym przyciskiem** (przełącznik): włączenie lub wyłączenie kolorów próbnych
- **Kliknięcie prawym przyciskiem**: otwórz okno sprawdzające oprogramowanie, w którym możesz dostosować:
  - aktualny profil
  - wybór profilu
  - zamiar renderowania
  - kompensacja czarnego punktu
  - oznakowanie poza zakresem

{{< callout type="warning" >}}
**Ważna uwaga dotycząca precyzji**
Podgląd próbny programowy jest włączony tylko dla obrazów **16-bitowych i 32-bitowych**.
W przypadku obrazów **8-bitowych** przełącznik jest wyłączony, a Lumi poprosi Cię o przekonwertowanie precyzji na większą głębię przed dokładnym podglądem kolorów.
{{< /callout >}}

## Preferencje i ustawienia domyślne

Globalne ustawienia domyślne są obecne w:
- Edycja > Preferencje > Zarządzanie koloramiOdpowiednie sekcje:
- **Ręczny profil monitora**
- **Preferowany profil RGB**
- **Preferowany profil w skali szarości**
- **Soft-Proofing**

### Bieżące ustawienia domyślne Lumi

#### Przestrzenie robocze

Pakiety ICC przestrzeni roboczej oferowane obecnie z udostępnionego folderu danych:
- `AdobeRGB1998.icc`
- `AppleRGB.icc`

Do standardowej pracy w trybie sRGB Lumi zapewnia również **wbudowany wewnętrznie profil roboczy sRGB**.

#### Domyślne ustawienia miękkie

Aktualnie zainstalowane profile miękkie w zestawie:
- `CoatedFOGRA39.icc`
- `USWebCoatedSWOP.icc`
- `JapanColor2001Coated.icc`

Jeśli jest dostępny, `CoatedFOGRA39.icc` jest używany jako domyślny dołączony profil referencyjny softproof/CMYK.

## Praktyczne przepływy pracy

### Do malowania i normalnej pracy na ekranie

- Przechowuj obraz we wbudowanej przestrzeni roboczej sRGB lub innej prawidłowej przestrzeni roboczej RGB.
- Pozwól Lumi używać profilu monitora systemu, jeśli jest dostępny.

### Do podglądu wydruku

- Zachowaj obraz w standardowej przestrzeni roboczej RGB.
- Wybierz profil softproof odpowiadający docelowym warunkom druku (np. FOGRA39).
- Włącz podgląd soft-proof.
- Opcjonalnie włącz ostrzeżenia o gamie kolorów, aby zobaczyć przycięte zamiary renderowania.