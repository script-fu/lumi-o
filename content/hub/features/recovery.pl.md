---
title: "Odzyskiwanie plików"
type: docs
---
Lumi obsługuje dwa niezależne systemy odzyskiwania — automatyczne zapisywanie w tle i ręczne, przyrostowe punkty kontrolne — oba dostępne z jednego okna dialogowego.

## Dostęp

**Plik** → **Odzyskaj obraz**

Otworzy się okno dialogowe wstępnie wypełnione stanami odzyskiwania aktualnie otwartego pliku. Użyj selektora plików u góry, aby przełączyć się na inny plik `.lum`.

---

## Autozapis

Lumi zapisuje migawkę tła Twojej pracy w regularnych odstępach czasu podczas edycji. Autozapisy są zapisywane w **oddzielnym katalogu pamięci podręcznej**, pozostawiając działający plik `.lum` nietknięty:

```
~/.cache/lumi/autosave/~home~user~projects~my-painting.lum/
```

Kodowanie ścieżki wykorzystuje `~` jako separator w celu utworzenia unikalnego katalogu pamięci podręcznej na plik. Oznacza to, że automatyczne zapisywanie jest dostępne nawet w przypadku utraty lub uszkodzenia samego pliku projektu.

- **Częstotliwość**: Konfigurowalna w **Edycja** → **Preferencje** → **Wydajność** → Interwał automatycznego zapisywania.
- **Lokalizacja przechowywania**: Ustawiana także w Preferencjach → Wydajność.
- **Cel**: Odzyskiwanie po awarii. Zakładka Autozapis w oknie dialogowym Odzyskaj obraz pokazuje dostępne stany automatycznego zapisywania wraz ze znacznikami czasu.

Gdy otworzysz plik zawierający nowsze dane z automatycznym zapisem, Lumi powiadomi Cię o tym w momencie otwarcia.

---

## Zapisywanie przyrostowe

Zapisywanie przyrostowe to ręczny system punktów kontrolnych przechowywany **w pliku projektu** pod adresem `recovery/`. Struktura jest następująca:

```
my-painting.lum/recovery/
  └── primary-01.lum/       (full baseline, created on first Ctrl+S)
      ├── delta-0001.lum/   (Ctrl+S checkpoint, only modified buffers)
      ├── delta-0002.lum/
      └── ...
```

Nowa linia bazowa `primary-NN.lum/` jest zapisywana po **Plik → Zapisz**. Kolejne naciśnięcia Ctrl+S tworzą podkatalogi `delta-NNNN.lum/` zawierające tylko bufory, które uległy zmianie od ostatniej linii bazowej. Różnice w automatycznym i ręcznym zapisywaniu korzystają z oddzielnych liczników, więc nie kolidują ze sobą w historii.

Zapisywanie przyrostowe jest **domyślnie wyłączone** i musi być włączone dla każdego projektu:

1. **Plik** → **Zapisz jako** (Shift+Ctrl+S).
2. W oknie dialogowym Zapisz jako zaznacz opcję **Zapis przyrostowy** i opcjonalnie ustaw limit **Maksymalnych zapisów**.
3. Ustawienie zostaje zapisane w projekcie i obowiązuje przy wszystkich kolejnych naciśnięciach Ctrl+S.

Kiedy otwierasz plik `.lum`, który zawiera nowsze zapisy przyrostowe niż zapis podstawowy, Lumi wyświetla komunikat **Wykryto zapis przyrostowy** oferujący załadowanie najnowszego punktu kontrolnego.

---

## Okno dialogowe Odzyskaj obraz

Okno dialogowe składa się z trzech zakładek i dwóch przycisków akcji.

### Zakładka Autozapis

Wyświetla listę wszystkich dostępnych stanów automatycznego zapisywania wybranego pliku wraz ze znacznikami czasu i miniaturami (jeśli są dostępne). Wybierz stan i kliknij **Odzyskaj**, aby go otworzyć.

Użyj tej zakładki, aby:
- Odzyskaj siły po wypadku.
- Przywróć wcześniejszy stan z tej samej sesji.

### Tablica przyrostowa

Wyświetla listę wszystkich stanów punktów kontrolnych przechowywanych w pliku projektu. Każdy wpis zawiera znacznik czasu punktu kontrolnego. Wybierz punkt kontrolny i kliknij **Odzyskaj**, aby go otworzyć.

Użyj tej zakładki, aby:
- Powrót do wcześniejszego punktu sesji bez zapisywania oddzielnych plików.
- Przeglądaj historię wersji projektu.

### Najnowsza zakładka

Domyślna zakładka po otwarciu okna dialogowego. Automatycznie identyfikuje najnowszy dostępny stan odzyskiwania zarówno w przypadku automatycznych zapisów, jak i przyrostowych punktów kontrolnych, i wyświetla jego sygnaturę czasową. Kliknij **Odzyskaj**, aby załadować go natychmiast, bez przeglądania poszczególnych stanów.

---

## Przyciski

| Przycisk | Akcja |
|------------|------------|
| **Odzyskaj** | Otwiera wybrany stan odzyskiwania jako nowy obraz. |
| **Zamknij** | Zamyka okno dialogowe bez odzyskiwania. |
| **Posprzątaj stare stany…** | Otwiera monit czyszczenia (patrz poniżej). |

---

## Posprzątaj stare stanyNagromadzenie stanów odzyskiwania w miarę upływu czasu może zająć znaczną ilość miejsca na dysku. Przycisk **Wyczyść stare stany…** (w lewym dolnym rogu okna dialogowego) otwiera monit o oczyszczenie aktywnej karty (Autozapis lub Przyrostowy).

Komunikat wyświetla:
- Ile pełnych zapisów istnieje dla pliku.
- Całkowite miejsce na dysku, które zajmują.
- Przycisk **Zachowaj najnowsze**, umożliwiający wybranie liczby zachowanych zapisów.

Ustawienie **Zachowaj najnowsze** na `0` usuwa wszystkie stany odzyskiwania. Następne naciśnięcie Ctrl+S po pełnym oczyszczeniu spowoduje zapisanie nowego zapisu podstawowego.

---

## Odzyskiwanie po uruchomieniu

Jeśli podczas uruchamiania Lumi wykryje, że ostatnio otwarty plik zawiera nowsze dane autozapisu niż ostatni pełny zapis, przed załadowaniem wyświetli się monit o odzyskanie. Możesz zaakceptować (wczytać autozapis) lub odrzucić (normalnie otworzyć podstawowy zapis).