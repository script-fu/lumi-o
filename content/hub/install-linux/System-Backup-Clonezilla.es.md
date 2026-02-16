---
title: "Copia de seguridad del sistema usando Clonezilla"
type: docs
url: "hub/install-linux/System-Backup-Clonezilla"
---
Es común hacer una copia de seguridad de sus archivos importantes para volver a versiones anteriores o reemplazar datos dañados. Sin embargo, otro tipo esencial de copia de seguridad es un **clon de disco**, una copia de seguridad completa del estado de su sistema.

Una vez que tenga su sistema configurado y funcionando bien, crear una copia de seguridad completa es crucial para restaurar su entorno en caso de que ocurra un desastre. Esta copia de seguridad complementa el almacenamiento periódico de sus datos de trabajo.

[Clonezilla](https://clonezilla.org/) es un software de clonación y creación de imágenes de discos gratuito y de código abierto. Permite a los usuarios crear y restaurar copias de seguridad completas del disco duro de su computadora, lo que la convierte en una herramienta popular tanto para profesionales de TI como para usuarios domésticos.

Siempre es mejor tener una copia de seguridad y no necesitarla que necesitarla y no tenerla.


## Características clave de Clonezilla

- **Imágenes de disco**: Clonezilla crea una copia exacta de un disco duro, incluido el sistema operativo, las aplicaciones y los datos.
- **Copia de seguridad y restauración**: Le permite crear una imagen de copia de seguridad de un disco duro y restaurarla en caso de falla o migración a una nueva unidad.
- **Gratis y de código abierto**: Clonezilla es de uso completamente gratuito y el código fuente está disponible para modificación y personalización.


## Usando Clonezilla para realizar copias de seguridad

### Pasos de preparación

Necesitará una unidad USB para Clonezilla y un disco duro externo que sea más grande que el disco interno que desea clonar.

Estos pasos simplifican el proceso basado en [official guide](https://clonezilla.org//fine-print-live-doc.php?path=./clonezilla-live/doc/01_Save_disk_image/00-boot-clonezilla-live-cd.doc#00-boot-clonezilla-live-cd.doc). Es una buena idea revisar la guía completa, que incluye capturas de pantalla para mayor claridad.

1. **Cree un USB o CD/DVD de Clonezilla Live**: siga las instrucciones detalladas en Clonezilla [website](https://clonezilla.org/liveusb.php) para crear un USB o CD/DVD de arranque.

2. **Conecte su unidad de respaldo externa**: conecte su unidad externa y asegúrese de que su sistema la reconozca. Este será el destino de su copia de seguridad.

3. **Verifique el diseño de su partición**: use el comando `lsblk` en una terminal para verificar el diseño de la partición de su disco duro principal. Tenga en cuenta el nombre del dispositivo principal.

4. **Arranque desde la unidad USB Clonezilla Live**: reinicie su computadora y arranque desde el medio Clonezilla que creó. Es posible que deba acceder a la configuración de BIOS/UEFI (generalmente presionando F2, F12, ESC o DEL durante el inicio) y ajustar el orden de inicio para priorizar la unidad USB.



### Copia de seguridad con Clonezilla

1. **Seleccione el modo de copia de seguridad**: una vez que se inicie Clonezilla, elija el modo "dispositivo-dispositivo". Este modo le permite clonar directamente su disco interno en un dispositivo externo.

2. **Seleccione el dispositivo de origen**: elija la unidad interna principal.

3. **Seleccione el dispositivo de destino**: elija su unidad de respaldo externa como dispositivo de destino. Tenga cuidado al seleccionar el dispositivo para evitar sobrescribir datos importantes. Asegúrese de que la unidad de destino sea igual o mayor en tamaño que la unidad de origen.

4. **Iniciar el proceso de copia de seguridad**: Clonezilla iniciará el proceso de copia de seguridad. Dependiendo del tamaño de su partición y la velocidad de sus unidades, esto podría llevar desde varios minutos hasta algunas horas.

5. **Etiquete su copia de seguridad**: una vez completada la copia de seguridad, etiquete la unidad USB y el disco duro externo con la fecha y el sistema del que realizó la copia de seguridad. Guárdelos en un lugar seguro.

---

### Restaurando desde la copia de seguridad

Si necesita restaurar su sistema Debian desde la copia de seguridad, siga estos pasos:

1. **Arranque desde Clonezilla Media**: Inserte el USB Clonezilla e inicie desde él, siguiendo los mismos pasos que durante el proceso de copia de seguridad.2. **Seleccione el modo de restauración**: elija el modo "dispositivo-dispositivo" nuevamente, pero esta vez restaurará desde la imagen de respaldo. Esto copiará todos los datos de su disco externo a su disco interno.

3. **Seleccione el dispositivo de origen**: elija su unidad externa donde se almacena la copia de seguridad.

4. **Seleccione el dispositivo de destino**: seleccione la unidad interna donde desea restaurar la copia de seguridad.

5. **Iniciar el proceso de restauración**: Clonezilla comenzará el proceso de restauración. Al igual que con la copia de seguridad, el tiempo necesario dependerá del tamaño de la unidad y de la velocidad de su hardware.

---

## Notas finales

Las copias de seguridad de disco con Clonezilla garantizan que se conserve todo su sistema (sistema operativo, configuración y aplicaciones). Con un mínimo esfuerzo, puede proteger su sistema contra fallas catastróficas y minimizar el tiempo de inactividad en caso de una falla.

Recuerde, **las copias de seguridad son esenciales**. Actualice periódicamente sus copias de seguridad y pruébelas periódicamente para asegurarse de que pueda restaurar su sistema cuando sea necesario.

Después de iniciar, puede conectar su unidad de respaldo externa e inspeccionar su estructura de partición usando la utilidad Discos en Linux. La unidad de respaldo debe reflejar la estructura de la unidad interna, con las mismas particiones y algo de espacio no utilizado si la unidad externa es más grande.