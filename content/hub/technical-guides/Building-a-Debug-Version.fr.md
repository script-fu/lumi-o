---
title: "Construire une version de débogage"
type: docs
url: "hub/technical-guides/Building-a-Debug-Version"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: fecc781e73b4f30881c5150c958ae9b2df4164acd4cf86926186acb8e2021d5f
---

Ce guide décrit le **workflow de débogage local** pour Lumi à l'aide des scripts dans `build/lumi/scripts`.

Le workflow est conçu pour :

- utiliser des artefacts de build locaux (aucun téléchargement de symboles requis),
- vérifier que les symboles de débogage sont bien présents,
- lancer GDB avec le mode symbole hors ligne par défaut.

## Prérequis

- Linux basé sur Debian (base de référence du projet : Debian 13)
- Arborescence source Lumi déjà clonée

## Configuration GDB unique (facultatif mais recommandé)

Installez les outils GDB :

```bash
sudo apt update
sudo apt install gdb gdbserver
```

Configuration facultative de la journalisation locale :

```bash
mkdir -p ~/code/gdb_logs
cat > ~/.gdbinit <<'EOF'
set logging file ~/code/gdb_logs/gdb_log.txt
set logging enabled on
set logging overwrite on
EOF
```

Remarque : les scripts de débogage locaux de Lumi désactivent `debuginfod` par défaut pour conserver la résolution des symboles locale et reproductible.

## Démarrage rapide

Depuis le répertoire des scripts :

```bash
cd ~/code/lumi-dev/build/lumi/scripts
```

### Build de débogage + lancement (par défaut)

Utilisez cette commande pour les sessions de débogage habituelles.

```bash
bash lumi-debug-local.sh lumi-dev build
```

Cette commande :

1. construit Lumi en mode débogage,
2. vérifie les symboles de débogage,
3. lance Lumi sous GDB.

### Build de débogage uniquement (pour une session TTY/distance ultérieure)

Utilisez cette commande lorsque vous souhaitez construire maintenant et lancer/déboguer plus tard.

```bash
bash lumi-build-debug.sh lumi-dev build
```

## Utilisation des TTY sous Linux

Les TTY (consoles texte) sont souvent le moyen le plus fiable de déboguer les blocages complets.

- Passez à un TTY avec `Ctrl + Alt + F1` à `Ctrl + Alt + F6`
- Connectez-vous depuis l'invite texte
- Revenez à la session graphique avec `Ctrl + Alt + F7` (ou `F2` sur certains systèmes)

Pourquoi c'est important : si la session de bureau est bloquée, un TTY répond souvent encore, ce qui vous permet d'attacher GDB, de capturer une trace arrière et de récupérer des données de crash utiles.

## Facultatif : débogage à distance / TTY

Pour les blocages complets ou les verrouillages d'affichage, utilisez `gdbserver` :

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash gdbserver.sh
```

Puis depuis un TTY (recommandé pour les scénarios de blocage) ou un autre terminal :

```bash
gdb /home/mark/code/lumi-dev/bin/lumi-0.1
(gdb) target remote localhost:9999
(gdb) continue
```

Pour un lancement GDB local (sans TTY) :

```bash
bash lumi-debug-launch.sh --repo lumi-dev
```

## Note sur les performances

Les builds de débogage sont plus lents par conception. Une fois le débogage terminé, repassez à un build plus rapide :

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# Réinitialisation complète en release pour tous les composants principaux
bash lumi-debug-reset-release.sh lumi-dev

# Variante locale plus rapide (facultatif)
bash lumi-build-script.sh --scope build --dir lumi-dev --type debugoptimized
```
