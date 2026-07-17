---
title: "Ladda ner och installera"
type: docs
url: "hub/quick-start/Download-and-Install"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 5f17d7e9009aeeacf256152bef94386ccc5a8eea87cf0feebef073488fb59283
---
Om du redan är på Linux och vill köra Lumi snabbt, använd den senaste **utvecklings-AppImage** från GitLab-artefakter:

https://gitlab.gnome.org/pixelmixer/lumi-dev/-/artifacts

1. Ladda ner den senaste utvecklingen av AppImage artifact zip.
2. Packa upp zip-filen.
3. Dubbelklicka på filen `Lumi*.AppImage` för att köra den.

AppImage bör redan vara körbar. Om det inte är det, aktivera **Tillåt körning av fil som program** i filens behörigheter, eller använd terminalmetoden nedan.

```bash
chmod +x Lumi*.AppImage
./Lumi*.AppImage
```