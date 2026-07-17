---
title: "Git unter Linux verwenden"
type: docs
url: "hub/technical-guides/Using-Git-on-Linux"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 7054a9ff9efeb93b4f494197e09ed1fe34d5d6bde7bc305480693c3982d375ae
---

Willkommen zu diesem Anfängerleitfaden zur Verwendung von Git unter Linux! Dieser Leitfaden hilft Ihnen beim Einstieg in Git und GitLab und vermittelt ein grundlegendes Verständnis dieser Tools.

## Git-Übersicht

Der zum Erstellen von Anwendungen verwendete Code wird in einer Sammlung von Ordnern und Dateien auf Ihrem System gespeichert. Git ist eine Anwendung, die es ermöglicht, diese Sammlung zu sichern, zu teilen und zu kopieren. Git ist ein Versionskontrollsystem, mit dem Sie Änderungen an Ihrem Code verfolgen und mit anderen zusammenarbeiten können. Es ist ein leistungsstarkes Tool, das in der Open-Source-Community weit verbreitet ist. GitLab ist eine webbasierte Plattform, die es Ihnen ermöglicht, Ihre Git-Repositorys online zu hosten und zu verwalten, sodass Sie einfach mit anderen zusammenarbeiten und Änderungen verfolgen können.

## Was ist ein Repository?

Ein _Repo_, kurz für Repository, ist ein von Git verwalteter lokaler Ordner mit einer Online-Kopie. Ein GitLab-Repository ist eine Sammlung von Dateien und Ordnern, aus denen ein Projekt besteht. Es kann _Branches_ haben, unabhängige Kopien desselben Projekts. Ein Branch ist eine separate Version Ihres Projekts, die es Ihnen ermöglicht, Änderungen vorzunehmen, ohne die Hauptversion zu beeinflussen. Das ist nützlich, um neue Funktionen zu testen oder Fehler zu beheben, ohne das Hauptprojekt zu stören. Es gibt Ihr lokales Repo, das auf Ihrer Festplatte gespeichert ist, und das Remote-Repo, das online mit Git und GitLab gespeichert ist.

## Git verwenden

Sie müssen Git auf Ihrem System installieren. Auf Debian-basierten Systemen können Sie den Befehl apt verwenden, um Softwarepakete zu installieren. Hier verwenden wir ihn, um Git zu installieren, ein Paket, das das Git-Versionskontrollsystem bereitstellt. Der Befehl sudo erteilt dem Installationsprogramm die Berechtigung zur Installation auf Ihrem System.

```bash
 sudo apt install git
```

## GitLab-Zugang

Bevor Sie [GitLab](https://gitlab.com/users/sign_up) verwenden können, müssen Sie ein Konto erstellen, indem Sie die GitLab-Website besuchen und den Registrierungsprozess abschließen.

GitLab benötigt _SSH_ für die sichere und authentifizierte Kommunikation zwischen einem Client (z. B. Ihnen) und dem GitLab-Server bei Git-Vorgängen wie _Klonen_, _Pushen_ und _Fetchen_ von Repositorys. Beim Klonen wird eine lokale Kopie des Repositorys erstellt, beim Fetchen werden Änderungen aus dem Remote-Repository in Ihre lokale Kopie geholt, und beim Pushen werden Änderungen und Inhalte an das Server-Repository gesendet. SSH (Secure Shell) ist ein Netzwerkprotokoll, das sicheren Fernzugriff ermöglicht und _Schlüsselpaare_ zur Authentifizierung und zum Aufbau sicherer Verbindungen verwendet. Um ein SSH-Schlüsselpaar zu generieren, verwenden Sie den Befehl ssh-keygen in Ihrem Terminal.

```bash
 ssh-keygen
```

Geben Sie einen Dateinamen an oder verwenden Sie den Standardnamen, indem Sie die Eingabetaste drücken, und optional ein Passwort. In Ihrem Home-Verzeichnis, in einem versteckten Ordner namens .ssh, befinden sich jetzt zwei id_rsa-Dateien, wenn Sie Standardnamen verwendet haben. Die .pub-Datei ist der öffentliche Schlüssel; Sie können ihren Inhalt mit einem Texteditor anzeigen.

Melden Sie sich bei Ihrem GitLab-Konto an und navigieren Sie zu Ihren Benutzereinstellungen. Klicken Sie im linken Navigationsmenü auf „SSH Keys“. Kopieren Sie Ihren öffentlichen Schlüssel, fügen Sie ihn in das Feld „Key“ ein und geben Sie dem Schlüssel einen relevanten Titel, z. B. PC@Home. Klicken Sie auf die Schaltfläche „Add Key“, um den Schlüssel zu speichern. Ihr öffentlicher SSH-Schlüssel ist jetzt Ihrem GitLab-Konto hinzugefügt und Sie können ihn zur Authentifizierung bei GitLab-Repositorys verwenden. Testen Sie mit dem Befehl ssh -T, ob Ihre Schlüssel und Ihre Verbindung funktionieren, um eine Willkommensnachricht von GitLab anzuzeigen.

```bash
 $ ssh -T git@ssh.gitlab.gnome.org
 Welcome to GitLab, @username!
```

## Grundlegende Git-Befehle

Nachdem Sie Git installiert und Ihren SSH-Schlüssel mit GitLab eingerichtet haben, gehen wir einige wichtige Git-Befehle zum Verwalten von Repositorys durch. Diese Befehle helfen Ihnen, mit vorhandenen Projekten zu arbeiten, sie auf dem neuesten Stand zu halten und Änderungen sicher vorzunehmen.

### 1. **Ein Repository klonen**

Beim Klonen wird eine lokale Kopie eines Remote-Repositorys erstellt. Das ist nützlich, wenn Sie an einem Projekt arbeiten möchten, das bereits auf GitLab vorhanden ist. Um ein Repository zu klonen, verwenden Sie den Befehl `git clone` gefolgt von der Repository-URL:

```sh
git clone https://gitlab.com/username/repository.git
```

Ersetzen Sie `https://gitlab.com/username/repository.git` durch die URL des Repositorys, das Sie klonen möchten. Dieser Befehl erstellt eine lokale Kopie des Repositorys in einem neuen Verzeichnis.

### 2. **Repository-Status prüfen**

Um zu sehen, ob an Ihrem lokalen Repository Änderungen vorgenommen wurden, oder um seinen aktuellen Status anzuzeigen, verwenden Sie:

```sh
git status
```

Dieser Befehl zeigt Ihnen, welche Dateien in Ihrer lokalen Kopie des Repositorys geändert, hinzugefügt oder gelöscht wurden.

### 3. **Remote-Repositorys**

Remote-Repositorys sind Versionen Ihres Projekts, die online gehostet werden, beispielsweise auf GitLab. Sie dienen als zentraler Ort, an dem Ihr Code gespeichert wird und auf den andere zugreifen können. Das standardmäßige Remote-Repository, das Git erstellt, wenn Sie ein Projekt klonen, heißt `origin`. Mit den folgenden Befehlen können Sie Remote-Repositorys hinzufügen, entfernen oder auflisten:

- **Remotes auflisten:**

  Um zu sehen, welche Remote-Repositorys mit Ihrem lokalen Projekt verknüpft sind, verwenden Sie:

  ```sh
  git remote -v
  ```

  Dieser Befehl listet alle Remotes und ihre URLs auf. Normalerweise sehen Sie hier `origin`.

- **Remote hinzufügen:**

  Wenn Sie ein neues Remote-Repository hinzufügen müssen, verwenden Sie:

  ```sh
  git remote add <name> <url>
  ```

  Ersetzen Sie `<name>` durch einen Namen für das Remote und `<url>` durch die URL des Repositorys.

- **Remote entfernen:**

  Um ein Remote-Repository zu entfernen, verwenden Sie:

  ```sh
  git remote remove <name>
  ```

  Ersetzen Sie `<name>` durch den Namen des Remotes, das Sie entfernen möchten.

### 4. **Änderungen aus dem Remote-Repository abrufen**

Wenn Sie sehen möchten, welche Änderungen am Remote-Repository vorgenommen wurden, ohne sie auf Ihre lokale Kopie anzuwenden, verwenden Sie:

```sh
git fetch origin
```

Dieser Befehl ruft die neuesten Änderungen aus dem Remote-Repository ab, führt sie jedoch nicht in Ihrem lokalen Branch zusammen. So können Sie nach Updates suchen, bevor Sie sich für deren Integration entscheiden.

### 5. **Lokales Repository zurücksetzen**

Wenn Sie Ihr lokales Repository so zurücksetzen möchten, dass es genau mit dem Remote-Repository übereinstimmt, können Sie einen „Hard“-Reset verwenden. **Warnung:** Dadurch werden alle von Ihnen vorgenommenen lokalen Änderungen überschrieben.

```sh
git reset --hard origin/branch-name
```

Ersetzen Sie `branch-name` durch den Namen des Branches, den Sie zurücksetzen möchten. Dieser Befehl verwirft alle lokalen Änderungen und macht Ihr lokales Repository mit dem Remote-Repository identisch.

### 6. **Commit-Verlauf anzeigen**

Um eine Liste der im Laufe der Zeit am Repository vorgenommenen Änderungen anzuzeigen, verwenden Sie:

```sh
git log
```

Dieser Befehl zeigt einen Verlauf der Commits an, einschließlich Autor, Datum und Nachricht für jede Änderung. Das ist hilfreich, um zu verstehen, welche Änderungen wann vorgenommen wurden.

### Zusammenfassung

Diese grundlegenden Git-Befehle helfen Ihnen bei der Arbeit mit Repositorys, halten Ihre lokalen Kopien auf dem neuesten Stand und stellen sicher, dass Sie Remote-Repositorys sicher verwalten können. Repositorys klonen, den Status Ihrer lokalen Kopie prüfen und Remotes verwalten sind Schlüsselkompetenzen für die Projektverwaltung mit Git.
