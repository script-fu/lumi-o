---
title: "Git unter Linux verwenden"
type: docs
url: "hub/technical-guides/folder/Using-Git-on-Linux"
---
Willkommen zu diesem Anfängerleitfaden zur Verwendung von Git unter Linux! Dieser Leitfaden soll Ihnen den Einstieg in Git und GitLab erleichtern und ein grundlegendes Verständnis für die Verwendung dieser Tools vermitteln.

## Git-Übersicht

Der zum Erstellen von Anwendungen verwendete Code wird in einer Sammlung von Ordnern und Dateien auf Ihrem System gespeichert. Git ist eine Anwendung, die es uns ermöglicht, diese Sammlung zu sichern, zu teilen und zu kopieren. Git ist als Versionskontrollsystem bekannt, mit dem Sie Änderungen an Ihrem Code verfolgen und mit anderen zusammenarbeiten können. Es handelt sich um ein leistungsstarkes Tool, das in der Open-Source-Community weit verbreitet ist. GitLab ist eine webbasierte Plattform, die es Ihnen ermöglicht, Ihre Git-Repositories online zu hosten und zu verwalten, sodass Sie ganz einfach mit anderen zusammenarbeiten und Änderungen an Ihrem Code verfolgen können.

## Was ist ein Repository?

Ein _repo_, kurz für Repository, ist ein von Git verwalteter lokaler Ordner mit einer Online-Kopie. Ein Git Lab-Repository ist eine Sammlung von Dateien und Ordnern, aus denen ein Projekt besteht. Es kann _Zweige_ haben, die unabhängige Kopien desselben Projekts sind. Ein Branch ist eine separate Version Ihres Projekts, die es Ihnen ermöglicht, Änderungen vorzunehmen, ohne dass dies Auswirkungen auf die Hauptversion hat. Dies ist nützlich, um neue Funktionen zu testen oder Fehler zu beheben, ohne das Hauptprojekt zu unterbrechen. Es gibt Ihr lokales Repo, das auf Ihrer Festplatte gespeichert ist, und das Remote-Repo, das online mit Git und GitLab gespeichert ist.

## Mit Git

Sie müssen Git auf Ihrem System installieren. Auf Debian-basierten Systemen können Sie den Befehl apt verwenden, um Softwarepakete zu installieren. In diesem Fall verwenden wir es, um Git zu installieren, ein Paket, das das Git-Versionskontrollsystem bereitstellt. Der Befehl sudo erteilt dem Installationsprogramm die Berechtigung zur Installation auf Ihrem System.

```bash
 sudo apt install git
```

## Greifen Sie auf GitLab zu

Bevor Sie [GitLab](https://gitlab.com/users/sign_up) verwenden können, müssen Sie ein Konto erstellen, indem Sie die GitLab-Website besuchen und den Registrierungsprozess abschließen.

GitLab benötigt _SSH_ für die sichere und authentifizierte Kommunikation zwischen einem Client (z. B. Ihnen) und dem GitLab-Server, wenn Git-Vorgänge wie _Klonen_, _Pushing_ und _Abrufen_ von Repositorys ausgeführt werden. Beim Klonen wird eine lokale Kopie des Repositorys erstellt, beim Abrufen werden alle im Repository vorgenommenen Änderungen in Ihre lokale Kopie übertragen und beim Pushen werden Änderungen und Inhalte an das Server-Repository gesendet. SSH (Secure Shell) ist ein Netzwerkprotokoll, das sicheren Fernzugriff ermöglicht und _Schlüsselpaare_ zur Authentifizierung und zum Aufbau sicherer Verbindungen verwendet. Um ein SSH-Schlüsselpaar zu generieren, können Sie den Befehl ssh-keygen in Ihrem Terminal verwenden.

```bash
 ssh-keygen
```

Geben Sie einen Dateinamen an oder verwenden Sie den Standardnamen, indem Sie die Eingabetaste drücken, und optional ein Passwort. In Ihrem Home-Verzeichnis, in einem versteckten Ordner namens .ssh, befinden sich jetzt zwei id_rsa-Dateien, wenn Sie Standardnamen verwendet haben. Die .pub-Datei ist der öffentliche Schlüssel und Sie können seinen Inhalt mit einem Texteditor anzeigen.

Melden Sie sich bei Ihrem GitLab-Konto an und navigieren Sie zu Ihren Benutzereinstellungen. Klicken Sie im linken Navigationsmenü auf „SSH-Schlüssel“. Kopieren Sie Ihren öffentlichen Schlüssel, fügen Sie ihn in das Feld „Schlüssel“ ein und geben Sie dem Schlüssel einen relevanten Titel, z. B. „PC@Home“. Klicken Sie auf die Schaltfläche „Schlüssel hinzufügen“, um den Schlüssel zu speichern. Ihr öffentlicher SSH-Schlüssel wird jetzt Ihrem GitLab-Konto hinzugefügt und Sie können ihn zur Authentifizierung bei GitLab-Repositorys verwenden. Testen Sie mit dem Befehl ssh -T, ob Ihre Schlüssel und Ihre Verbindung funktionieren, um eine Willkommensnachricht von GitLab anzuzeigen.

```bash
 $ ssh -T git@ssh.gitlab.gnome.org
 Welcome to GitLab, @username!
```

## Grundlegende Git-BefehleNachdem Sie Git nun installiert und Ihren SSH-Schlüssel mit GitLab eingerichtet haben, gehen wir nun einige wichtige Git-Befehle zum Verwalten von Repositorys durch. Diese Befehle helfen Ihnen, mit vorhandenen Projekten zu arbeiten, sie auf dem neuesten Stand zu halten und Änderungen sicher vorzunehmen.

### 1. **Klonen eines Repositorys**

Beim Klonen wird eine lokale Kopie eines Remote-Repositorys erstellt. Dies ist nützlich, wenn Sie an einem Projekt arbeiten möchten, das bereits auf GitLab vorhanden ist. Um ein Repository zu klonen, verwenden Sie den Befehl `git clone` gefolgt von der Repository-URL:

```sh
git clone https://gitlab.com/username/repository.git
```

Ersetzen Sie `https://gitlab.com/username/repository.git` durch die URL des Repositorys, das Sie klonen möchten. Dieser Befehl erstellt eine lokale Kopie des Repositorys in einem neuen Verzeichnis.

### 2. **Überprüfen des Repository-Status**

Um zu sehen, ob an Ihrem lokalen Repository Änderungen vorgenommen wurden, oder um seinen aktuellen Status anzuzeigen, verwenden Sie:

```sh
git status
```

Dieser Befehl zeigt Ihnen, welche Dateien in Ihrer lokalen Kopie des Repositorys geändert, hinzugefügt oder gelöscht wurden.

### 3. **Remote-Repositorys**

Remote-Repositorys sind Versionen Ihres Projekts, die online gehostet werden, beispielsweise auf GitLab. Sie dienen als zentraler Ort, an dem Ihr Code gespeichert wird und auf den andere zugreifen können. Das standardmäßige Remote-Repository, das Git erstellt, wenn Sie ein Projekt klonen, heißt `origin`. Mit den folgenden Befehlen können Sie Remote-Repositorys hinzufügen, entfernen oder auflisten:

- **Fernbedienungen auflisten:**

  Um zu sehen, welche Remote-Repositorys mit Ihrem lokalen Projekt verknüpft sind, verwenden Sie:

  ```sh
  git remote -v
  ```

  Dieser Befehl listet alle Fernbedienungen und ihre URLs auf. Normalerweise werden hier `origin` aufgeführt.

- **Hinzufügen einer Fernbedienung:**

  Wenn Sie ein neues Remote-Repository hinzufügen müssen, können Sie dies wie folgt tun:

  ```sh
  git remote add <name> <url>
  ```

  Ersetzen Sie `<name>` durch einen Namen für die Fernbedienung und `<url>` durch die URL des Repositorys.

- **Entfernen einer Fernbedienung:**

  Um ein Remote-Repository zu entfernen, verwenden Sie:

  ```sh
  git remote remove <name>
  ```

  Ersetzen Sie `<name>` durch den Namen der Fernbedienung, die Sie entfernen möchten.

### 4. **Änderungen aus dem Remote-Repository abrufen**

Wenn Sie sehen möchten, welche Änderungen am Remote-Repository vorgenommen wurden, ohne sie auf Ihre lokale Kopie anzuwenden, verwenden Sie Folgendes:

```sh
git fetch origin
```

Dieser Befehl ruft die neuesten Änderungen aus dem Remote-Repository ab, führt sie jedoch nicht in Ihrem lokalen Zweig zusammen. Auf diese Weise können Sie nach Updates suchen, bevor Sie sich für deren Integration entscheiden.

### 5. **Zurücksetzen Ihres lokalen Repositorys**

Wenn Sie Ihr lokales Repository so zurücksetzen möchten, dass es genau mit dem Remote-Repository übereinstimmt, können Sie einen „Hard“-Reset verwenden. **Warnung:** Dadurch werden alle von Ihnen vorgenommenen lokalen Änderungen überschrieben.

```sh
git reset --hard origin/branch-name
```

Ersetzen Sie `branch-name` durch den Namen des Zweigs, den Sie zurücksetzen möchten. Dieser Befehl verwirft alle lokalen Änderungen und macht Ihr lokales Repository mit dem Remote-Repository identisch.

### 6. **Commit-Verlauf anzeigen**

Um eine Liste der im Laufe der Zeit am Repository vorgenommenen Änderungen anzuzeigen, verwenden Sie:

```sh
git log
```

Dieser Befehl zeigt einen Verlauf der Commits an, einschließlich Autor, Datum und Nachricht für jede Änderung. Dies ist hilfreich, um zu verstehen, welche Änderungen wann vorgenommen wurden.

### Zusammenfassung

Diese grundlegenden Git-Befehle helfen Ihnen bei der Arbeit mit Repositorys, halten Ihre lokalen Kopien auf dem neuesten Stand und stellen sicher, dass Sie Remote-Repositorys sicher verwalten können. Das Klonen von Repositorys, das Überprüfen des Status Ihrer lokalen Kopie und die Verwaltung von Remote-Repositorys sind Schlüsselkompetenzen für die Projektverwaltung mit Git.