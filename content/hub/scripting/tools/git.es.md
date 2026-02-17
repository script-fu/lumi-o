---
title: "git"
type: docs
---
Utilice Git para realizar un seguimiento de los cambios en sus complementos, revertir errores y compartir código entre máquinas.

## ¿Por qué organizar su código?

Una vez que tenga más de un script, una estructura de carpetas coherente ahorrará tiempo y simplificará el control de versiones.

## Configurar una estructura de carpetas de códigos

Una de las formas más sencillas de organizar sus proyectos es crear una **carpeta de código** dedicada en su máquina local. Dentro de esta carpeta, puede crear subcarpetas para cada proyecto o repositorio. A continuación se muestra una estructura de carpetas recomendada:

```plaintext
/home/your-username/code/
  ├── project1/
  ├── project2/
  └── project3/
```

Cada subcarpeta (por ejemplo, `project1`) representa un **repositorio**, que es donde almacenará los archivos y el código de ese proyecto.

## ¿Qué es un repositorio?

Un **repositorio** (o **repo**) es esencialmente una carpeta con contenido que Git rastrea. Cuando crea un repositorio localmente, inicializa Git dentro de esa carpeta, lo que le permite guardar cualquier cambio en un clon en línea.

### Repositorios locales y remotos

- **Repositorio local**: Este es el repositorio almacenado en su computadora, en una de las carpetas de su proyecto.
- **Repositorio remoto**: una versión del repositorio almacenado en línea (por ejemplo, en GitLab o GitHub).

## Usando Git y GitHub

Una vez que su estructura de carpetas esté en su lugar, puede inicializar Git y conectar sus proyectos locales a GitHub. Siga estos pasos para comenzar:

### Pasos básicos para usar Git y GitHub

1. **Instalar Git**
2. **Crea una cuenta de GitHub**
3. **Cree un repositorio en blanco en GitHub**
4. **Inicializa Git en tu proyecto local**
5. **Conecte su repositorio local a GitHub**
6. **Prepara tus archivos**
7. **Confirma tus cambios**
8. **Envía tus cambios a GitHub**
9. **Vea su repositorio en línea**

### 1. Instalar Git

Si aún no has instalado Git, puedes hacerlo en Linux usando:

```sh
sudo apt install git
```

### 2. Cree una cuenta de GitHub

Si aún no tiene una cuenta, visite [GitHub](https://github.com/) para registrarse. Una vez registrado, puede crear repositorios en GitHub para almacenar su código en línea.

### 3. Cree un repositorio en blanco en GitHub

1. **Inicie sesión en GitHub**: vaya a [GitHub](https://github.com/) e inicie sesión en su cuenta.
2. **Crear un nuevo repositorio**:
   - Haga clic en el ícono **+** en la esquina superior derecha y seleccione **Nuevo repositorio**.
   - Ingrese un nombre de repositorio (por ejemplo, `your-repository`).
   - Agregue una descripción si lo desea.
   - Elija visibilidad **Pública** o **Privada**.
   - **No** inicialice el repositorio con un README, `.gitignore` o una licencia (para evitar conflictos).
   - Haga clic en **Crear repositorio**.

### 4. Inicialice Git en su proyecto local

Para comenzar a rastrear una carpeta de proyecto con Git, abra su terminal, navegue hasta la carpeta del proyecto y ejecute:

```sh
cd code/your/project/folder
git init
```

Este comando inicializa un repositorio Git vacío en la carpeta de su proyecto.

### 5. Conecte su repositorio local a GitHub

A continuación, querrás conectar tu repositorio local a GitHub. Después de crear un repositorio en blanco en GitHub, agréguelo como remoto a su proyecto local:

```sh
cd code/your/project/folder
git remote add origin https://github.com/your-username/your-repository.git
```

Reemplace `your-username` y `your-repository` con su nombre de usuario de GitHub real y el nombre del repositorio. Este comando vincula su proyecto local con el repositorio remoto en GitHub.

### 6. Organice sus archivos

Antes de poder guardar los cambios en Git, debe indicarle a Git qué archivos ha modificado y desea guardar. A esto se le llama "poner en escena" sus archivos. Utilice el siguiente comando para preparar todos los archivos nuevos o modificados:

```sh
git add .
```Esto le dice a Git que rastree los cambios que ha realizado en todos los archivos de su proyecto. También puede preparar archivos específicos reemplazando `.` con el nombre del archivo.

### 7. Confirme sus cambios

Después de la preparación, el siguiente paso es guardar (o "confirmar") los cambios en su repositorio Git local. Al confirmar, siempre debes incluir un mensaje que describa los cambios que has realizado. Por ejemplo:

```sh
git commit -m "Add new feature"
```

La bandera `-m` le permite escribir un mensaje que resuma los cambios que realizó. Este mensaje le ayuda a usted y a otros a comprender qué se modificó en esta confirmación.

### 8. Envíe sus cambios a GitHub

Una vez que haya confirmado los cambios localmente, ahora puede "enviarlos" a GitHub para que se actualice su repositorio remoto. Ejecute el siguiente comando para cargar sus cambios:

```sh
git push -u origin main
```

La rama `main` es la rama predeterminada en GitHub donde se almacena el código, y este comando carga sus cambios locales en el repositorio remoto, haciéndolos accesibles en línea.

### 9. Vea su código en GitHub

Una vez que haya enviado su código a GitHub, podrá ver su repositorio en la interfaz web de GitHub. Deberías ver los archivos de tu repositorio local, junto con un historial de confirmaciones que muestra los cambios que has realizado.

## Conclusión

Al organizar su código en carpetas dedicadas y usar GitHub para administrar y realizar copias de seguridad de sus repositorios, mantendrá sus proyectos bien estructurados y fácilmente accesibles. Una vez que tenga una versión funcional de su código, envíela a GitHub. Luego puede realizar fácilmente un seguimiento de cualquier cambio utilizando la interfaz web de GitHub o Visual Studio Code, que resalta las líneas modificadas. Este enfoque le permite continuar refinando y expandiendo su código sin perder de vista el progreso o los cambios.

Git y plataformas como GitHub y GitLab son herramientas poderosas y, si bien pueden ser complejas, existen numerosos recursos disponibles en línea para ayudarlo a comprenderlas mejor. Uno de los recursos más valiosos que he encontrado son los asistentes de IA como ChatGPT. Puede describir lo que necesita lograr y estas herramientas lo guiarán pacientemente a través del proceso paso a paso.

## Glosario

A continuación se muestran algunos términos comunes que encontrará cuando trabaje con Git y GitHub:- **Commit**: una instantánea de sus cambios en el repositorio. Cada confirmación incluye un mensaje que describe lo que se cambió y crea un registro histórico al que puede consultar o volver a consultar más adelante.
- **Repositorio (Repo)**: una colección de archivos y su historial rastreados por Git. Los repositorios pueden existir localmente en su computadora o de forma remota en plataformas como GitHub. Normalmente, cada proyecto se almacena en su propio repositorio.
- **Remoto**: un repositorio remoto es una versión de tu proyecto alojada en una plataforma como GitHub. La versión local de su proyecto en su computadora está vinculada a este control remoto para que pueda cargar (empujar) y descargar (extraer) cambios.
- **Staging**: El proceso de preparación de archivos para una confirmación. Cuando preparas un archivo, le estás diciendo a Git que deseas incluirlo en la próxima confirmación. La preparación le permite elegir qué cambios incluir en una confirmación.
- **Push**: el acto de enviar los cambios confirmados desde su repositorio local a un repositorio remoto (por ejemplo, GitHub), para que otros puedan acceder a la versión actualizada de su código.
- **Pull**: El acto de buscar cambios desde un repositorio remoto para actualizar su copia local. Realiza cambios cuando desea sincronizar su repositorio local con la última versión desde el control remoto.
- **Origen**: el nombre predeterminado de un repositorio remoto cuando conecta por primera vez su repositorio local a un repositorio remoto. Normalmente se refiere a la URL principal de su proyecto en GitHub.