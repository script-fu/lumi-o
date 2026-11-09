---
title: "OpenGL"
type: docs
url: "hub/features/opengl"
translation_provenance: ai-reviewed
translation_source_sha256: af2a57a988eced14801b7c4673253fe79e9a6b6ab4fcf53a075b62d4a9035351
translation_lock: true
---

Lumi może rysować stos warstw za pomocą OpenGL i w ten sam sposób malować pociągnięcia pędzla. Poruszanie się po płótnie pozostaje płynne. Duże aerografy i duże stemple tekstury są znacznie szybsze. To, czy dane narzędzie maluje w ten sposób, jest zapisane w jego presecie.

## Stos warstw

Okno obrazu pokazuje złożony stos warstw. Przy OpenGL ten obraz jest trzymany jako kafelki na procesorze graficznym, już przygotowane do wyświetlania. Przesuwanie, powiększanie, obrót i odbicie zmieniają widok tych kafelków. Na dużym płótnie gesty pozostają płynne, bo nawigacja jest zmianą widoku obrazu, który już tam jest.

Edycje aktualizują kafelki, których dotykają. Prowadnice, kursor pędzla i obrys zaznaczenia są rysowane nad tym obrazem.

Gdy pióro jest dociśnięte, pociągnięcie korzystające z OpenGL może być pokazane na miejscu, wśród warstw wokół niego, w tym warstw powyżej tej, która jest malowana. Znak jest zapisywany w warstwie, gdy pióro się unosi.

## Pociągnięcia, według presetu narzędzia

Malowanie OpenGL jest wyborem w presecie narzędzia, obok pędzla i dynamiki. Jeden preset może malować na procesorze graficznym. Inny może zachować zwykłą ścieżkę pociągnięcia. Powrót do narzędzia przywraca ten wybór.

Zysk jest największy tam, gdzie stemple są duże. Na zwykłej ścieżce szeroki aerograf zostaje w tyle za rysikiem, w miarę jak znak rośnie: plamki ustawiają się w kolejce, a w pociągnięciu pojawiają się przerwy, także gdy pióro jest przytrzymane i pędzel dalej emituje. Z OpenGL ten aerograf nadal kładzie znaki razem z ręką.

Duży stempel tekstury pokazuje tę samą zmianę. Zwykła ścieżka spędza dłużej na każdej plamce, w miarę jak stempel rośnie, aż pociągnięcie staje w miejscu. Pociągnięcie OpenGL zostaje przy geście. Stemple, które niosą ziarno powierzchni, też mogą być rysowane z OpenGL.

Znaki z włosia też można rysować w ten sposób. Stempel włosia z okrągłą końcówką może być malowany z OpenGL, podczas gdy włosie zmienia się wzdłuż pociągnięcia. Pociągnięcie kaligraficzne może być rysowane jako jedna ciągła wstęga, a tekstura powierzchni może maskować tę wstęgę. [Tekstura powierzchni](/hub/features/paintbrush/#tekstura-powierzchni) i [włosie wzdłuż pociągnięcia](/hub/features/paintbrush/#włosie-wzdłuż-pociągnięcia) należą do samego pędzla.

Niektóre pociągnięcia zostają na zwykłej ścieżce. Są wśród nich mieszanie widmowe, filtr na malowanej warstwie i niektóre tryby łączenia. Te znaki nadal są malowane.

## Gdy zostaje zwykłe płótno

OpenGL jest używany, gdy sprzęt graficzny potrafi go uruchomić, a obraz mieści się w pamięci wydzielonej na płótno. W przeciwnym razie Lumi rysuje okno jak dawniej. Opublikowany AppImage niesie własne biblioteki graficzne. Na sterowniku, którego te biblioteki nie mogą użyć, w tym na własnościowym sterowniku NVIDIA, płótno zostaje na tej zwykłej ścieżce.

## Cinnamon na X11 i GNOME na Waylandzie

Lumi jest rozwijane na Debianie z Cinnamon i X11. To nadal środowisko rozwoju. GNOME na Waylandzie zostało sprawdzone pod kątem ogólnego wyglądu aplikacji i tego renderowania OpenGL. Poruszanie się po płótnie jest tam również płynne.
