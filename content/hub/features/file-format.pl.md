---
title: "Format pliku (.lum)"
type: docs
url: "hub/features/file-format"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: f26bd5ecb0cb647cd3180b1ab39402ee6943085b7ad899518a406ee6ae98c4c9
---

Natywny format pliku Lumi służy projektom malarskim opartym na warstwach, które muszą pozostać niezawodne, możliwe do sprawdzenia i odzyskiwania w czasie. Został zaprojektowany z myślą o realiach pracy ilustracyjnej: wielu warstwach, dużych płótnach, osadzonych danych kolorów, maskach, efektach i danych odzyskiwania.

Zamiast traktować projekt jako jedną nieprzezroczystą całość, format utrzymuje strukturę grafiki widoczną dla aplikacji. Dzięki temu Lumi może zapisywać, ładować i odzyskiwać duże obrazy w sposób bardziej inteligentny, zachowując organizację, na której polegają artyści.

## Otwarta struktura projektu

Projekt Lumi oddziela części dzieła: strukturę obrazu, zawartość warstw, maski, dane kolorów, metadane i informacje o odzyskiwaniu — każda ma wyraźną rolę. Format jest łatwiejszy do zrozumienia i lepiej nadaje się do długoterminowego dostępu niż zamknięty, monolityczny kontener.

Chodzi nie tylko o przechowywanie pikseli, lecz o roboczy stan ilustracji. Warstwy pozostają warstwami, maski maskami, a plik nadal odzwierciedla sposób, w jaki powstała grafika.

## Zaprojektowany pod duże obrazy

Duże obrazy warstwowe szybko stają się ciężkie. Format Lumi obsługuje przepływy pracy, w których nie wszystkie dane obrazu muszą trafić do pamięci naraz. Projekty pozostają responsywne, ładując tylko te części obrazu, które są potrzebne do podglądu, edycji, komponowania lub eksportu.

Takie podejście ułatwia zarządzanie złożonymi plikami, zwłaszcza gdy grafika zawiera wiele ukrytych, zarchiwizowanych, eksperymentalnych lub pogrupowanych warstw.

## Zapisywanie bez przerywania pracy

Format pliku obsługuje zarówno zwykłe zapisywanie projektu, jak i lekkie migawki odzyskiwania. Artyści mogą często chronić pracę bez tworzenia pełnej kopii całego obrazu przy każdym punkcie kontrolnym.

Ponieważ informacje o odzyskiwaniu należą do struktury projektu, Lumi może trzymać użyteczną historię blisko grafiki, a jednocześnie pozwalać, by automatyczne zapisy bezpieczeństwa pozostawały oddzielnie od pliku roboczego.

## Wymiana i eksport

Format natywny służy bieżącej pracy w Lumi; formaty eksportu udostępniają spłaszczone lub zorientowane na kompatybilność wyniki. Import pomaga przenieść istniejącą grafikę do warstwowego środowiska Lumi, a eksport pozwala gotowym pracom opuścić format projektu, gdy są gotowe do publikacji, dostarczenia lub dalszej obróbki.

Dzięki temu plik roboczy pozostaje bogaty i edytowalny, a końcowe obrazy można tworzyć w popularnych formatach zewnętrznych.

## Długoterminowa niezawodność

Krótko mówiąc, format `.lum` to praktyczny kontener do poważnej pracy malarskiej: wystarczająco otwarty do inspekcji, wystarczająco ustrukturyzowany do odzyskiwania i wystarczająco elastyczny, by ekonomicznie obsługiwać złożone obrazy warstwowe.
