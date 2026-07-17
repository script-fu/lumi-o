---
title: "GitLab CI"
type: docs
url: "hub/technical-guides/GitLab-CI"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 9917cebc417adeeae24d91b05b919b679a397d5db652cf4442d4330c0f8eeea5
---

Continuous Integration (CI) ist eine Möglichkeit, Ihren Code automatisch zu testen, zu erstellen und zu validieren, wenn Änderungen vorgenommen werden.

**GitLab** bietet integrierte CI/CD-Funktionen über seine Datei `.gitlab-ci.yml`. Diese Datei, die im Stammverzeichnis Ihres Repositorys abgelegt wird, teilt GitLab mit, wie Ihr Projekt erstellt und getestet werden soll. Sie definiert Stages und Skripte, die bei jedem Push in einer sauberen Umgebung ausgeführt werden.

Dieses Dokument beschreibt die Funktionsweise der GitLab CI/CD-Pipeline von Lumi, einschließlich der Rolle der Datei `.gitlab-ci.yml`, Shell-Skripte und externer Tools wie Meson und Ninja.

Eine ausführliche technische Dokumentation des Lumi-CI-Build-Prozesses finden Sie unter [README-CI.md](https://gitlab.gnome.org/pixelmixer/lumi/-/blob/main/build/linux/appimage/README-CI.md) im Repository.

## GitLab CI/CD-Grundlagen

Das CI wird von einer Datei namens `.gitlab-ci.yml` gesteuert. Diese Datei definiert:

- **Stages**: Geordnete Gruppen von Jobs (z. B. `build-this`, `build-that`, `package-up`)
- **Jobs**: Einzelne Aufgaben, die in jeder Stage ausgeführt werden
- **Skripte**: Shell-Befehle, die für jeden Job ausgeführt werden
- **Runners**: Computer, die GitLab zum Ausführen von in der Pipeline definierten Jobs verwendet

In Lumi sind die Pipeline-Stages:

- `dependencies`
- `build lumi`
- `appimage`

## Containerbasierte Builds

Die Lumi-Pipeline nutzt Containerisierung für konsistente Builds:

1. **Erstellen des Build-Containers**: In der ersten Stage wird Buildah verwendet, um ein Docker-Image mit allen Abhängigkeiten zu erstellen
2. **Verwendung des Containers**: Nachfolgende Stages werden in diesem Container ausgeführt, um eine konsistente Umgebung sicherzustellen
3. **Reproduzierbare Builds**: Die Containerisolierung garantiert die gleichen Ergebnisse auf verschiedenen Runnern

Dieser Ansatz stellt sicher, dass Builds auf allen GitLab-Runnern gleich funktionieren und bietet eine kontrollierte Umgebung für komplexe Build-Prozesse.

### Integrierte Abhängigkeitsquellen

Das CI-Abhängigkeitsimage von Lumi erstellt den geforkten Stack aus **in-Repo-integrierten Quellen** (keine externen Klone):

- `lumi-babl/` (BABL)
- `lumi-gegl/` (GEGL)
- `lumi-gtk3/` (GTK3)

Diese Verzeichnisse werden in den Container-Build-Kontext kopiert und in das Abhängigkeitspräfix kompiliert (normalerweise `/opt/lumi-deps`). Dadurch bleibt CI reproduzierbar und stellt sicher, dass der AppImage-Build dieselbe Quelle der Wahrheit verwendet wie die lokale Entwicklung.

## Rolle von Shell-Skripten

Jobs in `.gitlab-ci.yml` rufen Shell-Befehle normalerweise direkt auf. Komplexe Vorgänge werden häufig in separate Skripte verschoben, die im Repository gespeichert sind.

Das Lumi-CI verwendet modulare Shell-Skripte, um die Build-Logik zu organisieren:

**Beispiel für einen Skriptaufruf:**
```yaml
script:
  - bash build/linux/appimage/lumi-goappimage.sh 2>&1 | tee appimage_creation.log
```

**Vorteile dieses Ansatzes:**
- **Sauberes YAML**: Die `.gitlab-ci.yml`-Datei konzentriert sich auf die Jobstruktur
- **Wartbarkeit**: Komplexe Logik lässt sich in Shell-Skripten einfacher debuggen und ändern
- **Wiederverwendbarkeit**: Skripte können in verschiedenen Kontexten oder Umgebungen verwendet werden
- **Modularität**: Verschiedene Aspekte des Builds können in fokussierte Skripte unterteilt werden

Dadurch bleibt die CI-Konfiguration sauber und ermöglicht gleichzeitig anspruchsvolle Build-Prozesse.

## Integration mit Build-Systemen

Lumi verwendet **Meson** und **Ninja**, um den Code vorzubereiten und dann zu erstellen.

Zum Beispiel:

```
script:
  - meson setup _build-${CI_RUNNER_TAG} -Dprefix="${LUMI_PREFIX}"
  - ninja -C _build-${CI_RUNNER_TAG}
  - ninja -C _build-${CI_RUNNER_TAG} install
```

Hier:

- `meson setup` bereitet das Build-Verzeichnis vor und generiert `build.ninja`
- `ninja` führt die Build-Befehle wie definiert aus

## Struktur des Meson-Build-Systems

Das **Meson**-Build-System verwendet eine Stammdatei `meson.build`, die im Stammverzeichnis des Projekts abgelegt wird. Diese Datei definiert die Build-Konfiguration der obersten Ebene und den Einstiegspunkt für den Build-Prozess.

- Das Stammverzeichnis `meson.build` befindet sich normalerweise im selben Verzeichnis wie `.gitlab-ci.yml`
- Von dort aus **kaskadiert es rekursiv** in Unterverzeichnisse, von denen jedes seine eigene `meson.build`-Datei haben kann
- Diese Unterverzeichnisdateien definieren Ziele, Quellen, Abhängigkeiten und Buildanweisungen, die für dieses Verzeichnis relevant sind

## Umgebungsvariablen

Zu den Schlüsselvariablen in der Lumi-Pipeline gehören:

```yaml
variables:
  DEBIAN_FRONTEND: "noninteractive"  # Prevents interactive prompts
  DEB_VERSION: "trixie"              # Debian version for consistency
  CI_RUNNER_TAG: "x86_64"            # Architecture specification
```

**Jobspezifische Variablen:**
```yaml
build-lumi:
  variables:
    COMPILER: "clang"                                           # Compiler selection
    LINKER: "lld"                                               # Linker selection
    LUMI_PREFIX: "${CI_PROJECT_DIR}/_install-${CI_RUNNER_TAG}"  # Installation path
    DEPS_PREFIX: "/opt/lumi-deps"                               # Prebuilt dependency prefix
    MESON_OPTIONS: "-Dpkgconfig.relocatable=true -Drelocatable-bundle=yes"  # Build configuration
```

Diese Variablen steuern das Build-Verhalten und stellen die Konsistenz über verschiedene Stages und Runner hinweg sicher.

## Beispielstruktur

```
project-root/
├── .gitlab-ci.yml
├── meson.build              <-- Root Meson file
├── src/
│   ├── meson.build          <-- Subdirectory Meson file
│   └── some_source.c
├── data/
│   ├── meson.build
│   └── icons/
```

In dieser Struktur:

- Die Stammdatei `meson.build` konfiguriert die gesamte Build-Umgebung
- Die `meson.build`-Dateien in Unterverzeichnissen verarbeiten Kompilierungsdetails für bestimmte Komponenten oder Module
- Durch dieses hierarchische Layout bleibt die Build-Logik modular und wartbar

## Artefakte zwischen den Stages

Artefakte sind von Jobs generierte Dateien, die in nachfolgenden Stages benötigt werden:

```yaml
build-lumi:
  # ...job configuration...
  artifacts:
    paths:
      - "${LUMI_PREFIX}/"      # Installation files
      - _build-${CI_RUNNER_TAG}/meson-logs/meson-log.txt  # Build logs
```

## Pipeline-Stages und Abhängigkeiten

Die Lumi-Pipeline besteht aus drei Hauptstages:

1. **Dependencies**: Erstellt eine containerisierte Build-Umgebung mit allen erforderlichen Tools und Bibliotheken
2. **Build Lumi**: Kompiliert Lumi mit Meson und Ninja in der vorbereiteten Umgebung
3. **AppImage**: Packt die erstellte Anwendung in ein verteilbares AppImage-Format

**Abhängigkeiten zwischen Stages:**
```yaml
build-lumi:
  needs: [deps-debian]  # Waits for dependency container

lumi-appimage:
  needs: [build-lumi] # Waits for application build
```

Jede Stage wird erst ausgeführt, nachdem ihre Abhängigkeiten erfolgreich abgeschlossen wurden, wodurch die richtige Build-Reihenfolge und Artefaktverfügbarkeit sichergestellt wird.

## Aktuelle Jobnamen

Das Lumi-`.gitlab-ci.yml` definiert derzeit diese Jobnamen:

- `deps-debian`
- `build-lumi`
- `lumi-appimage`

## Zusammenfassung

- `.gitlab-ci.yml` definiert die Struktur und Logik der Pipeline
- Jobs enthalten Shell-Befehle oder externe Skripte
- Tools wie Meson und Ninja werden innerhalb von Jobs als Teil des Build-Prozesses verwendet

Lumi verwendet GitLab CI, um sein AppImage automatisch für Debian-basierte Plattformen zu erstellen. Die Pipeline erstellt Abhängigkeiten, kompiliert Lumi und packt dann ein AppImage.

Für Details auf Quellebene verwenden Sie:

- `.gitlab-ci.yml` im Stammverzeichnis des Lumi-Repositorys
- `build/linux/appimage/lumi-goappimage.sh`
- `build/linux/appimage/README-CI.md`

Ausführliche technische Details zum Lumi-CI-Build-Prozess, einschließlich Umgebungseinrichtung, Skriptarchitektur und Fehlerbehebung, finden Sie unter [README-CI.md](https://gitlab.gnome.org/pixelmixer/lumi/-/blob/main/build/linux/appimage/README-CI.md).
