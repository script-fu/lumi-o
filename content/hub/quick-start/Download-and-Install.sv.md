---
title: "Ladda ner och installera"
type: docs
---
Om du redan är på Linux och vill köra Lumi snabbt, använd den senaste **utvecklingen AppImage** från GitLab-artefakter:

https://gitlab.gnome.org/pixelmixer/lumi-dev/-/artifacts

1. Ladda ner den senaste utvecklingen av AppImage artifact zip.
2. Dra ut blixtlåset.
3. Dubbelklicka på filen `Lumi*.AppImage` för att köra den.

AppImage bör redan vara körbar. Om det inte är det, aktivera **Tillåt körning av fil som program** i filens behörigheter, eller använd terminalmetoden nedan.

```bash
chmod +x Lumi*.AppImage
./Lumi*.AppImage
```