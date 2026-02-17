---
title: "Git"
type: docs
---
Verwenden Sie Git, um Änderungen an Ihren Plug-Ins zu verfolgen, Fehler rückgängig zu machen und Code maschinenübergreifend zu teilen.

## Warum Ihren Code organisieren?

Sobald Sie mehr als ein Skript haben, spart eine konsistente Ordnerstruktur Zeit und vereinfacht die Versionskontrolle.

## Einrichten einer Code-Ordnerstruktur

Eine der einfachsten Möglichkeiten, Ihre Projekte zu organisieren, besteht darin, einen dedizierten **Codeordner** auf Ihrem lokalen Computer zu erstellen. In diesem Ordner können Sie Unterordner für jedes Projekt oder Repository erstellen. Hier ist eine empfohlene Ordnerstruktur:

```plaintext
/home/your-username/code/
  ├── project1/
  ├── project2/
  └── project3/
```

Jeder Unterordner (z. B. `project1`) stellt ein **Repository** dar, in dem Sie die Dateien und den Code für dieses Projekt speichern.

## Was ist ein Repository?

Ein **Repository** (oder **Repo**) ist im Wesentlichen ein Ordner mit Inhalten, die Git verfolgt. Wenn Sie lokal ein Repo erstellen, initialisieren Sie Git in diesem Ordner, sodass Sie alle Änderungen an einem Online-Klon speichern können.

### Lokale und Remote-Repositorys

- **Lokales Repository**: Dies ist das Repository, das auf Ihrem Computer in einem Ihrer Projektordner gespeichert ist.
- **Remote Repo**: Eine online gespeicherte Version des Repositorys (z. B. auf GitLab oder GitHub).

## Mit Git und GitHub

Sobald Ihre Ordnerstruktur eingerichtet ist, können Sie Git initialisieren und Ihre lokalen Projekte mit GitHub verbinden. Befolgen Sie diese Schritte, um zu beginnen:

### Grundlegende Schritte zur Verwendung von Git und GitHub

1. **Git installieren**
2. **Erstellen Sie ein GitHub-Konto**
3. **Erstellen Sie ein leeres Repository auf GitHub**
4. **Git in Ihrem lokalen Projekt initialisieren**
5. **Verbinden Sie Ihr lokales Repo mit GitHub**
6. **Stellen Sie Ihre Dateien bereit**
7. **Übernehmen Sie Ihre Änderungen**
8. **Übertragen Sie Ihre Änderungen auf GitHub**
9. **Sehen Sie sich Ihr Repository online an**

### 1. Git installieren

Wenn Sie Git noch nicht installiert haben, können Sie dies unter Linux tun mit:

```sh
sudo apt install git
```

### 2. Erstellen Sie ein GitHub-Konto

Wenn Sie noch kein Konto haben, besuchen Sie [GitHub](https://github.com/), um sich anzumelden. Nach der Registrierung können Sie auf GitHub Repositories erstellen, um Ihren Code online zu speichern.

### 3. Erstellen Sie ein leeres Repository auf GitHub

1. **Bei GitHub anmelden**: Gehen Sie zu [GitHub](https://github.com/) und melden Sie sich bei Ihrem Konto an.
2. **Neues Repository erstellen**:
   - Klicken Sie auf das ***-Symbol in der oberen rechten Ecke und wählen Sie **Neues Repository**.
   - Geben Sie einen Repository-Namen ein (z. B. `your-repository`).
   - Fügen Sie bei Bedarf eine Beschreibung hinzu.
   - Wählen Sie die Sichtbarkeit **Öffentlich** oder **Privat**.
   - **Initiieren** Sie das Repository nicht mit einer README-Datei, `.gitignore` oder einer Lizenz (um Konflikte zu vermeiden).
   - Klicken Sie auf **Repository erstellen**.

### 4. Initialisieren Sie Git in Ihrem lokalen Projekt

Um mit der Verfolgung eines Projektordners mit Git zu beginnen, öffnen Sie Ihr Terminal, navigieren Sie zum Projektordner und führen Sie Folgendes aus:

```sh
cd code/your/project/folder
git init
```

Dieser Befehl initialisiert ein leeres Git-Repository in Ihrem Projektordner.

### 5. Verbinden Sie Ihr lokales Repo mit GitHub

Als Nächstes möchten Sie Ihr lokales Repository mit GitHub verbinden. Nachdem Sie ein leeres Repository auf GitHub erstellt haben, fügen Sie es als Remote zu Ihrem lokalen Projekt hinzu:

```sh
cd code/your/project/folder
git remote add origin https://github.com/your-username/your-repository.git
```

Ersetzen Sie `your-username` und `your-repository` durch Ihren tatsächlichen GitHub-Benutzernamen und den Repository-Namen. Dieser Befehl verknüpft Ihr lokales Projekt mit dem Remote-Repository auf GitHub.

### 6. Stellen Sie Ihre Dateien bereit

Bevor Sie Ihre Änderungen in Git speichern können, müssen Sie Git mitteilen, welche Dateien Sie geändert haben und speichern möchten. Dies wird als „Staging“ Ihrer Dateien bezeichnet. Verwenden Sie den folgenden Befehl, um alle geänderten oder neuen Dateien bereitzustellen:

```sh
git add .
```Dadurch wird Git angewiesen, die Änderungen zu verfolgen, die Sie an allen Dateien in Ihrem Projekt vorgenommen haben. Sie können auch bestimmte Dateien bereitstellen, indem Sie `.` durch den Namen der Datei ersetzen.

### 7. Übernehmen Sie Ihre Änderungen

Nach dem Staging besteht der nächste Schritt darin, die Änderungen in Ihrem lokalen Git-Repository zu speichern (oder zu „festschreiben“). Beim Festschreiben sollten Sie immer eine Nachricht hinzufügen, die beschreibt, welche Änderungen Sie vorgenommen haben. Zum Beispiel:

```sh
git commit -m "Add new feature"
```

Mit dem Flag `-m` können Sie eine Nachricht schreiben, die die von Ihnen vorgenommenen Änderungen zusammenfasst. Diese Nachricht hilft Ihnen und anderen zu verstehen, was in diesem Commit geändert wurde.

### 8. Übertragen Sie Ihre Änderungen auf GitHub

Sobald Sie die Änderungen lokal übernommen haben, können Sie sie nun an GitHub „pushen“, damit Ihr Remote-Repository aktualisiert wird. Führen Sie den folgenden Befehl aus, um Ihre Änderungen hochzuladen:

```sh
git push -u origin main
```

Der Zweig `main` ist der Standardzweig in GitHub, in dem der Code gespeichert wird. Mit diesem Befehl werden Ihre lokalen Änderungen in das Remote-Repository hochgeladen, sodass sie online zugänglich sind.

### 9. Sehen Sie sich Ihren Code auf GitHub an

Sobald Sie Ihren Code an GitHub übertragen haben, können Sie Ihr Repository in der GitHub-Weboberfläche anzeigen. Sie sollten die Dateien aus Ihrem lokalen Repo sehen, zusammen mit einem Commit-Verlauf, der die von Ihnen vorgenommenen Änderungen zeigt.

## Fazit

Indem Sie Ihren Code in speziellen Ordnern organisieren und GitHub zum Verwalten und Sichern Ihrer Repositorys verwenden, bleiben Ihre Projekte gut strukturiert und leicht zugänglich. Sobald Sie eine funktionierende Version Ihres Codes haben, übertragen Sie ihn auf GitHub. Anschließend können Sie alle Änderungen problemlos über die GitHub-Weboberfläche oder Visual Studio Code verfolgen, der geänderte Zeilen hervorhebt. Mit diesem Ansatz können Sie Ihren Code weiter verfeinern und erweitern, ohne den Überblick über Fortschritte oder Änderungen zu verlieren.

Git und Plattformen wie GitHub und GitLab sind leistungsstarke Tools, und obwohl sie komplex sein können, stehen online zahlreiche Ressourcen zur Verfügung, die Ihnen helfen, sie besser zu verstehen. Eine der wertvollsten Ressourcen, die ich gefunden habe, sind KI-Helfer wie ChatGPT. Sie können beschreiben, was Sie erreichen müssen, und diese Tools werden Sie geduldig Schritt für Schritt durch den Prozess führen.

## Glossar

Hier sind einige gebräuchliche Begriffe, die Ihnen bei der Arbeit mit Git und GitHub begegnen:- **Commit**: Eine Momentaufnahme Ihrer Änderungen im Repository. Jeder Commit enthält eine Nachricht, die beschreibt, was geändert wurde, und erstellt einen historischen Datensatz, auf den Sie später verweisen oder zurückgreifen können.
- **Repository (Repo)**: Eine Sammlung von Dateien und deren Verlauf, die von Git verfolgt werden. Repositorys können lokal auf Ihrem Computer oder remote auf Plattformen wie GitHub vorhanden sein. Jedes Projekt wird normalerweise in einem eigenen Repository gespeichert.
- **Remote**: Ein Remote-Repository ist eine Version Ihres Projekts, die auf einer Plattform wie GitHub gehostet wird. Die lokale Version Ihres Projekts auf Ihrem Computer ist mit dieser Remote-Version verknüpft, sodass Sie Änderungen hochladen (Push) und herunterladen (Pull) können.
- **Staging**: Der Prozess der Vorbereitung von Dateien für einen Commit. Wenn Sie eine Datei bereitstellen, teilen Sie Git mit, dass Sie sie in den nächsten Commit einschließen möchten. Beim Staging können Sie auswählen, welche Änderungen in einen Commit einbezogen werden sollen.
- **Push**: Der Vorgang des Sendens Ihrer festgeschriebenen Änderungen von Ihrem lokalen Repository an ein Remote-Repository (z. B. GitHub), damit andere auf die aktualisierte Version Ihres Codes zugreifen können.
- **Pull**: Der Vorgang des Abrufens von Änderungen aus einem Remote-Repository, um Ihre lokale Kopie zu aktualisieren. Sie ziehen Änderungen, wenn Sie Ihr lokales Repository mit der neuesten Version von der Fernbedienung synchronisieren möchten.
- **Ursprung**: Der Standardname für ein Remote-Repository, wenn Sie Ihr lokales Repository zum ersten Mal mit einem Remote-Repository verbinden. Bezieht sich normalerweise auf die Haupt-URL Ihres Projekts auf GitHub.