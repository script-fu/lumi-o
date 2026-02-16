---
title: "Usando Git en Linux"
type: docs
url: "hub/technical-guides/folder/Using-Git-on-Linux"
---
¡Bienvenido a esta guía para principiantes sobre el uso de Git en Linux! Esta guía está diseñada para ayudarlo a comenzar con Git y GitLab, y para brindarle una comprensión básica de cómo usar estas herramientas.

## Descripción general de Git

El código utilizado para crear aplicaciones se guarda en una colección de carpetas y archivos de su sistema. Git es una aplicación que nos permite realizar copias de seguridad, compartir y copiar esa colección. Git es conocido como un sistema de control de versiones que te permite realizar un seguimiento de los cambios en tu código y colaborar con otros. Es una herramienta poderosa que se usa ampliamente en la comunidad de código abierto. GitLab es una plataforma basada en web que le permite alojar y administrar sus repositorios Git en línea, lo que facilita la colaboración con otros y el seguimiento de los cambios en su código.

## ¿Qué es un repositorio?

Un _repo_, abreviatura de repositorio, es una carpeta local administrada por Git con una copia en línea. Un repositorio de Git Lab es una colección de archivos y carpetas que conforman un proyecto. Puede tener _ramas_ que sean copias independientes del mismo proyecto. Una rama es una versión separada de su proyecto que le permite realizar cambios sin afectar la versión principal. Esto es útil para probar nuevas funciones o corregir errores sin interrumpir el proyecto principal. Está su repositorio local, almacenado en su disco duro, y el repositorio remoto, almacenado en línea usando Git y GitLab.

## Usando Git

Necesitarás instalar Git en tu sistema. En sistemas basados ​​en Debian, puede utilizar el comando apt para instalar paquetes de software. En este caso, lo estamos usando para instalar Git, que es un paquete que proporciona el sistema de control de versiones de Git. El comando sudo le da permiso al instalador para instalar en su sistema.

```bash
 sudo apt install git
```

## Acceder a GitLab

Antes de poder usar [GitLab](https://gitlab.com/users/sign_up), deberá crear una cuenta visitando el sitio web de GitLab y completando el proceso de registro.

GitLab requiere _SSH_ para una comunicación segura y autenticada entre un cliente (usted, por ejemplo) y el servidor de GitLab al realizar operaciones de Git como _clonación_, _empuje_ y _obtención_ de repositorios. Clonar es hacer una copia local del repositorio, recuperar es traer cualquier cambio realizado en el repositorio a su copia local y enviar es enviar cambios y contenido al repositorio del servidor. SSH (Secure Shell) es un protocolo de red que permite el acceso remoto seguro y utiliza _pares de claves_ para autenticar y establecer conexiones seguras. Para generar un par de claves SSH, puede utilizar el comando ssh-keygen en su terminal.

```bash
 ssh-keygen
```

Especifique un nombre de archivo o use el predeterminado presionando Enter y, opcionalmente, una contraseña. En su directorio de inicio, en una carpeta oculta llamada .ssh, ahora hay dos archivos id_rsa, si eligió los nombres predeterminados. El archivo .pub es la clave pública y puedes ver su contenido con un editor de texto.

Inicie sesión en su cuenta de GitLab y navegue hasta su configuración de usuario. Haga clic en 'Claves SSH' en el menú de navegación de la izquierda. Copie y pegue su clave pública en el campo Clave y asígnele un título relevante, como PC@Home. Haga clic en el botón 'Agregar clave' para guardar la clave. Su clave pública SSH ahora está agregada a su cuenta de GitLab y puede usarla para autenticarse en los repositorios de GitLab. Pruebe si sus claves y su conexión funcionan con el comando ssh -T para ver un mensaje de bienvenida de GitLab.

```bash
 $ ssh -T git@ssh.gitlab.gnome.org
 Welcome to GitLab, @username!
```

## Comandos básicos de GitAhora que tiene Git instalado y configuró su clave SSH con GitLab, repasemos algunos comandos esenciales de Git para administrar repositorios. Estos comandos le ayudarán a trabajar con proyectos existentes, manteniéndolos actualizados y realizando cambios de forma segura.

### 1. **Clonación de un repositorio**

La clonación es el proceso de crear una copia local de un repositorio remoto. Esto es útil cuando deseas trabajar en un proyecto que ya existe en GitLab. Para clonar un repositorio, use el comando `git clone` seguido de la URL del repositorio:

```sh
git clone https://gitlab.com/username/repository.git
```

Reemplace `https://gitlab.com/username/repository.git` con la URL del repositorio que desea clonar. Este comando creará una copia local del repositorio en un nuevo directorio.

### 2. **Comprobación del estado del repositorio**

Para ver si su repositorio local tiene algún cambio o para ver su estado actual, use:

```sh
git status
```

Este comando le mostrará qué archivos se han modificado, agregado o eliminado en su copia local del repositorio.

### 3. **Repositorios remotos**

Los repositorios remotos son versiones de su proyecto alojadas en línea, como en GitLab. Sirven como ubicación central donde se almacena su código y otras personas pueden acceder a él. El repositorio remoto predeterminado que crea Git cuando clonas un proyecto se llama `origin`. Puede agregar, eliminar o enumerar repositorios remotos utilizando los siguientes comandos:

- **Listado de controles remotos:**

  Para ver qué repositorios remotos están vinculados a su proyecto local, use:

  ```sh
  git remote -v
  ```

  Este comando enumera todos los controles remotos y sus URL. Normalmente, verás `origin` en la lista.

- **Agregar un control remoto:**

  Si necesita agregar un nuevo repositorio remoto, puede hacerlo con:

  ```sh
  git remote add <name> <url>
  ```

  Reemplace `<name>` con un nombre para el control remoto y `<url>` con la URL del repositorio.

- **Extracción de un control remoto:**

  Para eliminar un repositorio remoto, utilice:

  ```sh
  git remote remove <name>
  ```

  Reemplace `<name>` con el nombre del control remoto que desea eliminar.

### 4. **Obteniendo cambios desde el repositorio remoto**

Si desea ver qué cambios se han realizado en el repositorio remoto sin aplicarlos a su copia local, use:

```sh
git fetch origin
```

Este comando recupera los últimos cambios del repositorio remoto pero no los fusiona en su sucursal local. Es una forma de comprobar si hay actualizaciones antes de decidir incorporarlas.

### 5. **Restableciendo su repositorio local**

Si desea restablecer su repositorio local para que coincida exactamente con el repositorio remoto, puede utilizar un restablecimiento completo. **Advertencia:** Esto sobrescribirá cualquier cambio local que haya realizado.

```sh
git reset --hard origin/branch-name
```

Reemplace `branch-name` con el nombre de la rama que desea restablecer. Este comando descartará cualquier cambio local y hará que su repositorio local sea idéntico al repositorio remoto.

### 6. **Ver el historial de confirmaciones**

Para ver una lista de los cambios realizados en el repositorio a lo largo del tiempo, utilice:

```sh
git log
```

Este comando muestra un historial de confirmaciones, incluido el autor, la fecha y el mensaje de cada cambio. Es útil para comprender qué cambios se han realizado y cuándo.

### Resumen

Estos comandos básicos de Git lo ayudarán a trabajar con repositorios, manteniendo actualizadas sus copias locales y garantizando que pueda administrar repositorios remotos de forma segura. Clonar repositorios, verificar el estado de su copia local y administrar repositorios remotos son habilidades clave para administrar proyectos usando Git.