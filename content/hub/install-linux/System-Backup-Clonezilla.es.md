---
title: "Copia de seguridad del sistema con Clonezilla"
type: docs
url: "hub/install-linux/System-Backup-Clonezilla"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: b3453289d7da56bb4fc9039616edb73e537acd6a722f0eb8a000e4a398016863
---

Es habitual hacer copias de seguridad de los archivos importantes para volver a versiones anteriores o sustituir datos dañados. Sin embargo, otro tipo esencial de copia de seguridad es un **clon de disco**: una copia de seguridad completa del estado de tu sistema.

Una vez que tengas el sistema configurado y funcionando bien, crear una copia de seguridad completa es crucial para restaurar tu entorno si ocurre un desastre. Esta copia complementa el guardado habitual de tus datos de trabajo.

[Clonezilla](https://clonezilla.org/) es un software gratuito y de código abierto para crear imágenes y clonar discos. Permite crear y restaurar copias de seguridad completas del disco duro del equipo, por lo que es una herramienta popular tanto para profesionales de TI como para usuarios domésticos.

Siempre es mejor tener una copia de seguridad y no necesitarla que necesitarla y no tenerla.


## Características principales de Clonezilla

- **Imagen de disco**: Clonezilla crea una copia exacta de un disco duro, incluidos el sistema operativo, las aplicaciones y los datos.
- **Copia de seguridad y restauración**: te permite crear una imagen de copia de seguridad de un disco duro y restaurarla en caso de fallo o al migrar a una unidad nueva.
- **Gratuito y de código abierto**: Clonezilla es completamente gratuito y su código fuente está disponible para modificación y personalización.


## Copia de seguridad con Clonezilla

### Pasos de preparación

Necesitarás una unidad USB para Clonezilla y un disco duro externo más grande que la unidad interna que quieras clonar.

Estos pasos simplifican el proceso según la [guía oficial](https://clonezilla.org//fine-print-live-doc.php?path=./clonezilla-live/doc/01_Save_disk_image/00-boot-clonezilla-live-cd.doc#00-boot-clonezilla-live-cd.doc). Conviene revisar la guía completa, que incluye capturas de pantalla para mayor claridad.

1. **Crea un USB o CD/DVD de Clonezilla Live**: sigue las instrucciones detalladas en el [sitio web de Clonezilla](https://clonezilla.org/liveusb.php) para crear un USB o CD/DVD de arranque.

2. **Conecta la unidad externa de copia de seguridad**: conecta la unidad externa y asegúrate de que el sistema la reconozca. Será el destino de la copia de seguridad.

3. **Verifica el diseño de particiones**: usa el comando `lsblk` en una terminal para comprobar el diseño de particiones del disco principal. Anota el nombre del dispositivo principal.

4. **Arranca desde el USB de Clonezilla Live**: reinicia el equipo y arranca desde el medio Clonezilla que creaste. Puede que tengas que acceder a la configuración de BIOS/UEFI (normalmente pulsando F2, F12, ESC o DEL durante el arranque) y ajustar el orden de arranque para priorizar la unidad USB.



### Copia de seguridad con Clonezilla

1. **Selecciona el modo de copia de seguridad**: cuando Clonezilla arranque, elige el modo "device-device". Este modo te permite clonar directamente la unidad interna en un dispositivo externo.

2. **Selecciona el dispositivo de origen**: elige la unidad interna principal.

3. **Selecciona el dispositivo de destino**: elige la unidad externa de copia de seguridad como dispositivo de destino. Ten cuidado al seleccionar el dispositivo para no sobrescribir datos importantes. Asegúrate de que la unidad de destino tenga un tamaño igual o superior al de la unidad de origen.

4. **Inicia el proceso de copia de seguridad**: Clonezilla comenzará el proceso de copia de seguridad. Según el tamaño de la partición y la velocidad de las unidades, puede tardar desde varios minutos hasta unas horas.

5. **Etiqueta la copia de seguridad**: cuando termine, etiqueta la unidad USB y el disco duro externo con la fecha y el sistema del que hiciste la copia. Guárdalos en un lugar seguro.

---

### Restauración desde la copia de seguridad

Si necesitas restaurar tu sistema Debian desde la copia de seguridad, sigue estos pasos:

1. **Arranca desde el medio Clonezilla**: inserta el USB de Clonezilla y arranca desde él, siguiendo los mismos pasos que durante la copia de seguridad.

2. **Selecciona el modo de restauración**: elige de nuevo el modo "device-device", pero esta vez restaurarás desde la imagen de copia de seguridad. Esto copiará todos los datos de la unidad externa a la unidad interna.

3. **Selecciona el dispositivo de origen**: elige la unidad externa donde está almacenada la copia de seguridad.

4. **Selecciona el dispositivo de destino**: selecciona la unidad interna en la que quieres restaurar la copia de seguridad.

5. **Inicia el proceso de restauración**: Clonezilla comenzará el proceso de restauración. Al igual que con la copia de seguridad, el tiempo necesario dependerá del tamaño de la unidad y de la velocidad del hardware.

---

## Notas finales

Las copias de seguridad de disco con Clonezilla garantizan que se conserve todo el sistema —sistema operativo, ajustes y aplicaciones—. Con un esfuerzo mínimo, puedes proteger el sistema ante fallos catastróficos y reducir el tiempo de inactividad en caso de avería.

Recuerda: **las copias de seguridad son esenciales**. Actualiza tus copias de seguridad con regularidad y pruébalas periódicamente para asegurarte de poder restaurar el sistema cuando lo necesites.

Después de arrancar, puedes conectar la unidad externa de copia de seguridad e inspeccionar su estructura de particiones con la utilidad Discos en Linux. La unidad de copia de seguridad debería reflejar la estructura de la unidad interna, con las mismas particiones y algo de espacio sin usar si la unidad externa es más grande.
