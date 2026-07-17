---
title: "Usar Git en Linux"
type: docs
url: "hub/technical-guides/Using-Git-on-Linux"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 7054a9ff9efeb93b4f494197e09ed1fe34d5d6bde7bc305480693c3982d375ae
---

¡Bienvenido a esta guía para principiantes sobre el uso de Git en Linux! Está pensada para ayudarte a empezar con Git y GitLab, y para ofrecerte una comprensión básica de cómo usar estas herramientas.

## Introducción a Git

El código con el que se crean las aplicaciones se guarda en una colección de carpetas y archivos en tu sistema. Git es una aplicación que nos permite hacer copias de seguridad, compartir y duplicar esa colección. Git es un sistema de control de versiones que te permite seguir los cambios en tu código y colaborar con otras personas. Es una herramienta muy extendida en la comunidad de código abierto. GitLab es una plataforma web que te permite alojar y gestionar tus repositorios Git en línea, facilitando la colaboración y el seguimiento de los cambios en tu código.

## ¿Qué es un repositorio?

Un _repo_, abreviatura de repositorio, es una carpeta local gestionada por Git con una copia en línea. Un repositorio de GitLab es una colección de archivos y carpetas que componen un proyecto. Puede tener _ramas_ que son copias independientes del mismo proyecto. Una rama es una versión separada de tu proyecto que te permite hacer cambios sin afectar a la versión principal. Esto resulta útil para probar nuevas funciones o corregir errores sin interrumpir el proyecto principal. Existe tu repositorio local, almacenado en el disco duro, y el repositorio remoto, almacenado en línea mediante Git y GitLab.

## Usar Git

Necesitarás instalar Git en tu sistema. En sistemas basados en Debian, puedes usar el comando apt para instalar paquetes de software. En este caso, lo usamos para instalar Git, el paquete que proporciona el sistema de control de versiones Git. El comando sudo concede al instalador permiso para instalar en tu sistema.

```bash
 sudo apt install git
```

## Acceder a GitLab

Antes de poder usar [GitLab](https://gitlab.com/users/sign_up), tendrás que crear una cuenta visitando el sitio web de GitLab y completando el proceso de registro.

GitLab requiere _SSH_ para una comunicación segura y autenticada entre un cliente (tú, por ejemplo) y el servidor de GitLab al realizar operaciones de Git como _clonar_, _hacer push_ y _hacer fetch_ de repositorios. Clonar consiste en crear una copia local del repositorio; hacer fetch trae al repositorio local los cambios realizados en el remoto; y hacer push envía cambios y contenido al repositorio del servidor. SSH (Secure Shell) es un protocolo de red que permite el acceso remoto seguro y utiliza _pares de claves_ para autenticarse y establecer conexiones seguras. Para generar un par de claves SSH, puedes usar el comando ssh-keygen en tu terminal.

```bash
 ssh-keygen
```

Indica un nombre de archivo o usa el predeterminado pulsando Enter y, opcionalmente, una contraseña. En tu directorio personal, en una carpeta oculta llamada .ssh, aparecerán ahora dos archivos id_rsa si usaste los nombres predeterminados. El archivo .pub es la clave pública y puedes ver su contenido con un editor de texto.

Inicia sesión en tu cuenta de GitLab y ve a la configuración de usuario. Haz clic en «Claves SSH» en el menú de navegación izquierdo. Copia y pega tu clave pública en el campo Clave y asigna a la clave un título descriptivo, como PC@Home. Haz clic en el botón «Añadir clave» para guardarla. Tu clave pública SSH queda añadida a tu cuenta de GitLab y puedes usarla para autenticarte en los repositorios de GitLab. Comprueba que tus claves y la conexión funcionan con el comando ssh -T para ver un mensaje de bienvenida de GitLab.

```bash
 $ ssh -T git@ssh.gitlab.gnome.org
 Welcome to GitLab, @username!
```

## Comandos básicos de Git

Ahora que tienes Git instalado y has configurado tu clave SSH con GitLab, repasemos algunos comandos esenciales de Git para gestionar repositorios. Estos comandos te ayudarán a trabajar con proyectos existentes, mantenerlos actualizados y hacer cambios con seguridad.

### 1. **Clonar un repositorio**

Clonar es el proceso de crear una copia local de un repositorio remoto. Resulta útil cuando quieres trabajar en un proyecto que ya existe en GitLab. Para clonar un repositorio, usa el comando `git clone` seguido de la URL del repositorio:

```sh
git clone https://gitlab.com/username/repository.git
```

Sustituye `https://gitlab.com/username/repository.git` por la URL del repositorio que quieras clonar. Este comando creará una copia local del repositorio en un directorio nuevo.

### 2. **Comprobar el estado del repositorio**

Para ver si tu repositorio local tiene cambios o consultar su estado actual, usa:

```sh
git status
```

Este comando te mostrará qué archivos se han modificado, añadido o eliminado en tu copia local del repositorio.

### 3. **Repositorios remotos**

Los repositorios remotos son versiones de tu proyecto alojadas en línea, por ejemplo en GitLab. Sirven como ubicación central donde se almacena tu código y al que pueden acceder otras personas. El repositorio remoto predeterminado que Git crea al clonar un proyecto se llama `origin`. Puedes añadir, eliminar o listar repositorios remotos con los siguientes comandos:

- **Listar remotos:**

  Para ver qué repositorios remotos están vinculados a tu proyecto local, usa:

  ```sh
  git remote -v
  ```

  Este comando lista todos los remotos y sus URL. Normalmente verás `origin` en la lista.

- **Añadir un remoto:**

  Si necesitas añadir un nuevo repositorio remoto, puedes hacerlo con:

  ```sh
  git remote add <name> <url>
  ```

  Sustituye `<name>` por un nombre para el remoto y `<url>` por la URL del repositorio.

- **Eliminar un remoto:**

  Para eliminar un repositorio remoto, usa:

  ```sh
  git remote remove <name>
  ```

  Sustituye `<name>` por el nombre del remoto que quieras eliminar.

### 4. **Obtener cambios del repositorio remoto**

Si quieres ver qué cambios se han hecho en el repositorio remoto sin aplicarlos a tu copia local, usa:

```sh
git fetch origin
```

Este comando obtiene los últimos cambios del repositorio remoto, pero no los fusiona en tu rama local. Es una forma de comprobar actualizaciones antes de decidir incorporarlas.

### 5. **Restablecer tu repositorio local**

Si quieres restablecer tu repositorio local para que coincida exactamente con el repositorio remoto, puedes usar un restablecimiento «hard». **Advertencia:** esto sobrescribirá cualquier cambio local que hayas hecho.

```sh
git reset --hard origin/branch-name
```

Sustituye `branch-name` por el nombre de la rama que quieras restablecer. Este comando descartará cualquier cambio local y hará que tu repositorio local sea idéntico al remoto.

### 6. **Ver el historial de commits**

Para ver una lista de los cambios realizados en el repositorio a lo largo del tiempo, usa:

```sh
git log
```

Este comando muestra un historial de commits, incluidos el autor, la fecha y el mensaje de cada cambio. Resulta útil para entender qué cambios se han hecho y cuándo.

### Resumen

Estos comandos básicos de Git te ayudarán a trabajar con repositorios, mantener actualizadas tus copias locales y gestionar repositorios remotos con seguridad. Clonar repositorios, comprobar el estado de tu copia local y gestionar repositorios remotos son habilidades clave para administrar proyectos con Git.
