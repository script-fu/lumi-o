---
title: "Avvio rapido"
type: docs
---
Lumi non è ancora stato rilasciato, è disponibile come versione di sviluppo.

Se utilizzi già Linux e desideri eseguire Lumi rapidamente, utilizza l'ultima **AppImage di sviluppo** degli artefatti GitLab:

https://gitlab.gnome.org/pixelmixer/lumi-dev/-/artifacts

1. Scarica l'ultimo file zip dell'artefatto AppImage di sviluppo.
2. Estrarre la zip.
3. Fare doppio clic sul file `Lumi*.AppImage` per eseguirlo.

L'AppImage dovrebbe essere già eseguibile. In caso contrario, attiva **Consenti l'esecuzione del file come programma** nelle autorizzazioni del file o utilizza il metodo terminale riportato di seguito.

```bash
chmod +x Lumi*.AppImage
./Lumi*.AppImage
```

## Configurazione Wacom su Linux

Per la pittura digitale in Lumi, una semplice **impostazione della pressione lineare** è solitamente la migliore:

- Mantenere lineare la curva di pressione del driver della tavoletta.
- Mantenere le curve di pressione/ingresso in Lumi lineari.
- Dai forma alla sensazione con il pennello stesso, poiché la dinamica del pennello può essere non lineare.

Puoi controllare e reimpostare la curva del driver Linux con:

```bash
xsetwacom --get "Wacom Intuos Pro L Pen stylus" PressureCurve
xsetwacom --set "Wacom Intuos Pro L Pen stylus" PressureCurve 0 0 100 100
```

Consigli pratici:

- Lumi attualmente blocca il problematico input del pad Wacom/touch-ring per evitare problemi con X11. Mappa invece i pulsanti del tablet sulla dimensione **relativa** del pennello su/giù.
- Se il trascinamento delle dimensioni del pennello con `Alt` non funziona, il desktop potrebbe utilizzare `Alt` per spostare le finestre. Cambia il collegamento del gestore finestre in `Super` o disabilitalo.

Se vuoi lavorare dal codice sorgente, vai su [Technical Guides](/hub/technical-guides/) e [Installation](/hub/technical-guides/Installation/).