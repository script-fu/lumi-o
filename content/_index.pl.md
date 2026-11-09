---
title: "Malowanie cyfrowe open source dla Linuksa"
toc: false
type: docs
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 4f453bf4100140e9a2779d4b9806a0ef8f27e68af8a9b6f680d01a6a5d2f2341
---
[![gui-01](/images/screens/gui-00.jpg)](/images/screens/gui-01.jpg)

Lumi-o jest przeznaczone do warstwowego malowania rastrowego — celowo lekkie i stabilne. Obsługa tabletów Wacom zapewnia płynne i responsywne sterowanie, jakiego wymaga profesjonalna ilustracja.

System kolorów Lumi oparty na paletach spektralnie miesza profile pigmentów ze świata rzeczywistego. Twórz palety w oparciu o wizualizowane relacje między kolorami, kontroluj wartości tonalne i nadaj kolorom nazwy. Mapa palet i Mikser palet pomagają utrzymać spójność decyzji kolorystycznych w obrazie lub projekcie.

Zbudowane na rdzennej architekturze GIMP-a, Lumi-o zostało zaprojektowane i dopracowane przez artystę. Jego narzędzia pozostają przystępne, oferując jednocześnie możliwości dostosowania potrzebne w zaawansowanych przepływach pracy. Szybkie ładowanie i zapisywanie oraz w pełni zintegrowany system automatycznego zapisu chronią Twoją pracę bez przerywania pracy.

Pociągnięcia pędzla mogą przyjąć teksturę powierzchni ze ziarna i zęba, także pociągnięcia kaligraficzne. Nowe pociągnięcie albo skręt w pociągnięciu może przesunąć to ziarno, więc cieniowanie tam i z powrotem buduje się ze świeżej fazy przy każdym nawróceniu. Pędzle z włosia można kształtować w drobnym szczególe, a włosie może przesuwać się i odnawiać wzdłuż pociągnięcia. OpenGL rysuje stos warstw w oknie obrazu i może malować pociągnięcia dla poszczególnych presetów narzędzi. Duże aerografy i duże stemple tekstury są znacznie szybsze, a poruszanie się po płótnie pozostaje płynne.

Leniwe ładowanie pozwala Lumi otwierać duże, złożone projekty przy znacznie mniejszym zużyciu pamięci RAM. Gdy grupa jest zwinięta, Lumi ładuje zapisany obraz złożony, a jej warstwy, maski i zagnieżdżone grupy pozostają lekkimi obiektami zastępczymi, dopóki grupa nie zostanie rozwinięta.

Lumi-o jest rozwijane na Debianie 13 z Cinnamon na X11. GNOME na Waylandzie zostało sprawdzone pod kątem ogólnego wyglądu aplikacji i renderowania OpenGL. Jeśli przechodzisz z systemu Windows, pierwszym krokiem jest instalacja Debiana. Zobacz [Jak zainstalować Linuksa](/hub/install-linux/).

{{< cards >}}
  {{< card link="hub/about/" title="O programie" icon="information-circle" >}}
  {{< card link="hub/quick-start/" title="Szybki start" icon="play" >}}
  {{< card link="hub/features/" title="Funkcje" icon="book-open" >}}
{{< /cards >}}
