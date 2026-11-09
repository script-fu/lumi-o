---
title: "OpenGL"
type: docs
url: "hub/features/opengl"
translation_provenance: ai-reviewed
translation_source_sha256: af2a57a988eced14801b7c4673253fe79e9a6b6ab4fcf53a075b62d4a9035351
translation_lock: true
---

Lumi kann den Ebenenstapel mit OpenGL zeichnen und Pinselstriche auf dieselbe Weise malen. Die Navigation auf der Leinwand bleibt flüssig. Große Airbrush-Pinsel und große Texturstempel sind deutlich schneller. Ob ein bestimmtes Werkzeug so malt, ist in seiner Werkzeugvoreinstellung gespeichert.

## Der Ebenenstapel

Das Bildfenster zeigt den zusammengesetzten Ebenenstapel. Mit OpenGL liegt dieses Bild als Kacheln auf dem Grafikprozessor, bereits für die Anzeige vorbereitet. Verschieben, Zoomen, Drehen und Spiegeln ändern die Ansicht dieser Kacheln. Auf einer großen Leinwand bleiben die Gesten flüssig, weil die Navigation nur die Ansicht eines Bildes wechselt, das schon da ist.

Bearbeitungen aktualisieren die Kacheln, die sie berühren. Hilfslinien, der Pinselcursor und der Auswahlumriss werden über dieses Bild gezeichnet.

Solange der Stift aufliegt, kann ein Strich, der OpenGL nutzt, an Ort und Stelle zwischen den umgebenden Ebenen erscheinen, einschließlich der Ebenen über der gerade bemalten Ebene. Die Marke wird in die Ebene geschrieben, wenn der Stift abhebt.

## Striche je Werkzeugvoreinstellung

Malen mit OpenGL ist eine Wahl der Werkzeugvoreinstellung, neben Pinsel und Dynamik. Eine Voreinstellung kann auf dem Grafikprozessor malen. Eine andere kann den gewöhnlichen Strichweg behalten. Die Rückkehr zu einem Werkzeug bringt diese Wahl mit zurück.

Der Gewinn ist am größten, wo die Stempel groß sind. Auf dem gewöhnlichen Weg bleibt ein breiter Airbrush hinter dem Stift zurück, während die Marke wächst: die Tupfer stauen sich in einer Warteschlange, und der Strich bekommt Lücken, auch während der Stift gehalten wird und der Pinsel weiter ausgibt. Mit OpenGL setzt dieser Airbrush die Marken weiter im Takt der Hand.

Ein großer Texturstempel zeigt dieselbe Veränderung. Der gewöhnliche Weg braucht für jeden Tupfer länger, je größer der Stempel wird, bis ein Strich stockt. Der OpenGL-Strich folgt der Geste. Stempel, die eine Oberflächenkörnung tragen, lassen sich ebenfalls mit OpenGL zeichnen.

Auch Borstenmarken können so gezeichnet werden. Ein Borstenstempel mit runder Spitze kann mit OpenGL gemalt werden, während sich die Borsten entlang des Strichs ändern. Ein kalligrafischer Strich kann als ein durchgehendes Band gezeichnet werden, und die Oberflächentextur kann dieses Band maskieren. [Oberflächentextur](/hub/features/paintbrush/#oberflächentextur) und [Borsten entlang des Strichs](/hub/features/paintbrush/#borsten-entlang-des-strichs) gehören zum Pinsel selbst.

Manche Striche bleiben auf dem gewöhnlichen Weg. Spektrales Mischen, ein Filter auf der bemalten Ebene und einige Überblendmodi gehören dazu. Diese Marken werden trotzdem gemalt.

## Wenn die gewöhnliche Leinwand bleibt

OpenGL wird verwendet, wenn die Grafikhardware es ausführen kann und das Bild in den für die Leinwand vorgesehenen Speicher passt. Andernfalls zeichnet Lumi das Fenster wie zuvor. Das veröffentlichte AppImage bringt eigene Grafikbibliotheken mit. Bei einem Treiber, den diese Bibliotheken nicht nutzen können, einschließlich des proprietären Treibers von NVIDIA, bleibt die Leinwand auf diesem gewöhnlichen Weg.

## Cinnamon unter X11 und GNOME unter Wayland

Lumi wird auf Debian mit Cinnamon und X11 entwickelt. Das ist weiterhin die Entwicklungsumgebung. GNOME unter Wayland wurde auf das allgemeine Erscheinungsbild der Anwendung und auf dieses OpenGL-Rendering hin getestet. Auch dort bleibt die Navigation auf der Leinwand flüssig.
