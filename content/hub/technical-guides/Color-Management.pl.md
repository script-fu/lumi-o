---
title: "Zarządzanie kolorami"
type: docs
weight: 15
url: "hub/technical-guides/Color-Management"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: e124f17c1f65c73f4e135c25dd7962eb44f1d0676147a7e4bcbf6dc8ecf51e69
---

Lumi-o jest skonfigurowany do pracy od razu po uruchomieniu. Jeśli pracujesz nad obrazem z **16-bitową lub większą precyzją**, oprogramowanie jest już ustawione na korzystanie z domyślnego pakietu soft proofingu (CMYK) i wbudowanych profili sRGB; wszystko powinno działać bez dodatkowej konfiguracji.

Dla osób potrzebujących większej kontroli ten przewodnik wyjaśnia podstawowy model zarządzania kolorami w Lumi, różnicę między profilem obrazu a profilem soft proof, gdzie znajdują się elementy sterujące oraz dokładnie, w jaki sposób domyślne profile są dołączone do aplikacji.

## Szybkie podsumowanie

Lumi wykorzystuje trzy różne role profili:

1. **Profil roboczy obrazu**
   - Określa, co oznaczają wartości RGB lub skali szarości obrazu.
   - Używany do operacji przypisywania i konwersji.
   - Typowe przykłady: wbudowany sRGB, Adobe RGB.

2. **Profil monitora**
   - Opisuje Twój monitor.
   - Służy do prawidłowego wyświetlania obrazu na ekranie.
   - Zwykle dostarczany przez system lub wybierany w Preferencjach.

3. **Profil soft proof**
   - Symuluje inne urządzenie wyjściowe lub warunki druku.
   - **Nie** redefiniuje wartości pikseli obrazu.
   - Typowe przykłady: profile prasowe CMYK, takie jak `CoatedFOGRA39`.

## Profil obrazu a profil soft proof

### Profil obrazu

Użyj tej opcji, gdy chcesz powiedzieć Lumi, w jakiej przestrzeni barw obraz faktycznie się znajduje.

Dwie typowe operacje:

- **Przypisz profil**
  - Zmienia etykietę profilu przypisaną do obrazu.
  - **Nie** konwertuje wartości pikseli.
  - Używaj tylko wtedy, gdy wartości pikseli są już w przestrzeni tego profilu.

- **Konwertuj na profil**
  - Konwertuje wartości pikseli z bieżącego profilu obrazu na nowy.
  - Użyj, gdy chcesz, aby obraz rzeczywiście przeszedł do innej przestrzeni roboczej.

**Lokalizacje w menu:**
- Obraz > Zarządzanie kolorami > Przypisz profil kolorów...
- Obraz > Zarządzanie kolorami > Konwertuj na profil kolorów...

### Profil soft proof

Użyj tej opcji, gdy chcesz zobaczyć podgląd tego, jak obraz będzie wyglądał na urządzeniu docelowym lub w danych warunkach druku.

Soft proofing:
- pozostawia przestrzeń roboczą obrazu bez zmian
- zmienia potok podglądu
- może oznaczać kolory spoza gamutu
- służy do podglądu, a nie do ponownego przypisywania danych obrazu

**Lokalizacje w menu:**
- Obraz > Zarządzanie kolorami > Ustawienia soft proof > Wybierz profil soft proof...
- Obraz > Zarządzanie kolorami > Ustawienia soft proof > Intencja renderowania
- Obraz > Zarządzanie kolorami > Ustawienia soft proof > Kompensacja czarnego punktu
- Widok > Zarządzanie kolorami > Włącz podgląd soft proof
- Widok > Zarządzanie kolorami > Oznacz kolory spoza gamutu

## Jak włączyć podgląd soft proof

Istnieją dwa główne sposoby przełączania soft proof.

### 1. Menu Widok

Użyj:
- Widok > Zarządzanie kolorami > Włącz podgląd soft proof

Włącza lub wyłącza symulację podglądu dla bieżącego ekranu.

### 2. Przełącznik na pasku stanu

Lumi udostępnia soft proofing także bezpośrednio na dolnym pasku stanu.

- **Kliknięcie lewym przyciskiem** (przełącznik): włącz lub wyłącz kolory proof
- **Kliknięcie prawym przyciskiem**: otwórz okno soft proofing, w którym możesz dostosować:
  - bieżący profil
  - wybór profilu
  - intencję renderowania
  - kompensację czarnego punktu
  - oznaczanie spoza gamutu

{{< callout type="warning" >}}
**Ważna uwaga dotycząca precyzji**
Podgląd soft proof jest włączony tylko dla obrazów **16-bitowych i 32-bitowych**.
W przypadku obrazów **8-bitowych** przełącznik jest wyłączony, a Lumi poprosi o konwersję precyzji na większą głębię bitową przed dokładnym podglądem kolorów.
{{< /callout >}}

## Preferencje i ustawienia domyślne

Globalne ustawienia domyślne znajdują się w:
- Edycja > Preferencje > Zarządzanie kolorami

Odpowiednie sekcje:
- **Ręczny profil monitora**
- **Preferowany profil RGB**
- **Preferowany profil w skali szarości**
- **Soft proofing**

### Bieżące ustawienia domyślne Lumi

#### Przestrzenie robocze

Dołączone profile ICC przestrzeni roboczych, obecnie dostępne z folderu współdzielonych danych:
- `AdobeRGB1998.icc`
- `AppleRGB.icc`

Do standardowej pracy w sRGB Lumi udostępnia także **wbudowany wewnętrznie profil roboczy sRGB**.

#### Domyślne ustawienia soft proof

Dołączone profile soft proof, obecnie zainstalowane:
- `CoatedFOGRA39.icc`
- `USWebCoatedSWOP.icc`
- `JapanColor2001Coated.icc`

Jeśli jest dostępny, `CoatedFOGRA39.icc` jest używany jako domyślny dołączony profil referencyjny soft proof/CMYK.

## Praktyczne przepływy pracy

### Do malowania i normalnej pracy na ekranie

- Przechowuj obraz we wbudowanej przestrzeni roboczej sRGB lub innej prawidłowej przestrzeni roboczej RGB.
- Pozwól Lumi używać profilu monitora systemowego, jeśli jest dostępny.

### Do podglądu wydruku

- Zachowaj obraz w standardowej przestrzeni roboczej RGB.
- Wybierz profil soft proof odpowiadający docelowym warunkom druku (np. FOGRA39).
- Włącz podgląd soft proof.
- Opcjonalnie włącz ostrzeżenia o gamucie, aby zobaczyć przycięte intencje renderowania.
