---
title: "Construire une version de débogage"
type: docs
url: "hub/technical-guides/Building-a-Debug-Version"
---
Ce guide décrit le **workflow de débogage local** pour Lumi à l'aide de scripts dans `build/lumi/scripts`.

Le flux de travail est conçu pour :

- utiliser des artefacts de construction locaux (aucun téléchargement de symboles requis),
- vérifier que les symboles de débogage sont bien présents,
- lancez GDB avec le mode symbole hors ligne par défaut.

## Prérequis

- Linux basé sur Debian (base de référence du projet : Debian 13)
- Arbre source Lumi déjà cloné

## Configuration GDB unique (facultatif mais recommandé)

Installez les outils GDB :

```bash
sudo apt update
sudo apt install gdb gdbserver
```

Configuration facultative de la journalisation locale :

```bash
mkdir -p ~/code/gdb_logs
cat > ~/.gdbinit <<'EOF'
set logging file ~/code/gdb_logs/gdb_log.txt
set logging enabled on
set logging overwrite on
EOF
```

Remarque : les scripts de débogage locaux de Lumi désactivent `debuginfod` par défaut pour conserver la résolution des symboles locale et reproductible.

## Démarrage rapide

Depuis le répertoire des scripts :

```bash
cd ~/code/lumi-dev/build/lumi/scripts
```

### Débogage Build + Lancement (par défaut)

Utilisez-le pour les sessions de débogage normales.

```bash
bash lumi-debug-local.sh lumi-dev build
```

Cette commande :

1. construit Lumi en mode débogage,
2. vérifie les symboles de débogage,
3. lance Lumi sous GDB.

### Debug Build uniquement (pour une session TTY/à distance ultérieure)

Utilisez-le lorsque vous souhaitez construire maintenant et lancer/déboguer plus tard.

```bash
bash lumi-build-debug.sh lumi-dev build
```

## Utilisation des ATS sous Linux

Les ATS (consoles texte) sont souvent le moyen le plus fiable de déboguer les blocages matériels.

- Passez à un TTY avec `Ctrl + Alt + F1` via `Ctrl + Alt + F6`
- Connectez-vous à partir de l'invite de texte
- Revenir à la session graphique avec `Ctrl + Alt + F7` (ou `F2` sur certains systèmes)

Pourquoi est-ce important : si la session de bureau est bloquée, un téléscripteur répond souvent, vous pouvez donc attacher GDB, capturer une trace et récupérer des données de crash utiles.

## Facultatif : débogage à distance/ATS

Pour les gels durs ou les blocages d'affichage, utilisez `gdbserver` :

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash gdbserver.sh
```

Puis depuis un TTY (recommandé pour les scénarios de gel) ou un autre terminal :

```bash
gdb /home/mark/code/lumi-dev/bin/lumi-0.1
(gdb) target remote localhost:9999
(gdb) continue
```

Pour un lancement GDB local (chemin non TTY) :

```bash
bash lumi-debug-launch.sh --repo lumi-dev
```

## Note sur les performances

Les builds de débogage sont de par leur conception plus lentes. Une fois le débogage terminé, revenez à une version plus rapide :

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# Full release reset of all major components
bash lumi-debug-reset-release.sh lumi-dev

# Optional faster local-only variant
bash lumi-build-script.sh --scope build --dir lumi-dev --type debugoptimized
```