---
title: "Cópia de segurança do sistema com Clonezilla"
type: docs
url: "hub/install-linux/System-Backup-Clonezilla"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: b3453289d7da56bb4fc9039616edb73e537acd6a722f0eb8a000e4a398016863
---
É habitual fazer cópias de segurança dos ficheiros importantes para voltar a versões anteriores ou substituir dados corrompidos. No entanto, outro tipo essencial de cópia de segurança é um **clone de disco**, uma cópia completa do estado do sistema.

Depois de ter o sistema configurado e a funcionar bem, criar uma cópia de segurança completa é crucial para restaurar o ambiente em caso de desastre. Esta cópia complementa a gravação regular dos dados de trabalho.

[Clonezilla](https://clonezilla.org/) é software gratuito e de código aberto para imagens e clonagem de discos. Permite criar e restaurar cópias de segurança completas do disco rígido do computador, o que o torna popular tanto entre profissionais de TI como entre utilizadores domésticos.

É sempre preferível ter uma cópia de segurança e não precisar dela do que precisar de uma e não a ter.


## Principais funcionalidades do Clonezilla

- **Imagem de disco**: o Clonezilla cria uma cópia exacta de um disco rígido, incluindo o sistema operativo, aplicações e dados.
- **Cópia de segurança e restauro**: permite criar uma imagem de cópia de segurança de um disco rígido e restaurá-la em caso de falha ou migração para um novo disco.
- **Gratuito e de código aberto**: o Clonezilla é totalmente gratuito e o código-fonte está disponível para modificação e personalização.


## Usar o Clonezilla para cópias de segurança

### Passos de preparação

Será necessária uma pen USB para o Clonezilla e um disco rígido externo maior do que o disco interno que pretende clonar.

Estes passos simplificam o processo com base no [guia oficial](https://clonezilla.org//fine-print-live-doc.php?path=./clonezilla-live/doc/01_Save_disk_image/00-boot-clonezilla-live-cd.doc#00-boot-clonezilla-live-cd.doc). Vale a pena rever o guia completo, que inclui capturas de ecrã para maior clareza.

1. **Criar uma pen USB ou CD/DVD Clonezilla Live**: siga as instruções detalhadas no [site](https://clonezilla.org/liveusb.php) do Clonezilla para criar suporte de arranque USB ou CD/DVD.

2. **Ligar o disco externo de cópia de segurança**: ligue o disco externo e certifique-se de que o sistema o reconhece. Este será o destino da cópia de segurança.

3. **Verificar o esquema de partições**: use o comando `lsblk` num terminal para verificar o esquema de partições do disco principal. Anote o nome do dispositivo principal.

4. **Arrancar a partir da pen USB Clonezilla Live**: reinicie o computador e arranque a partir do suporte Clonezilla criado. Pode ser necessário aceder às definições BIOS/UEFI (normalmente premindo F2, F12, ESC ou DEL durante o arranque) e ajustar a ordem de arranque para priorizar a pen USB.



### Cópia de segurança com o Clonezilla

1. **Seleccionar o modo de cópia de segurança**: quando o Clonezilla arrancar, escolha o modo «device-device». Este modo permite clonar directamente o disco interno para um dispositivo externo.

2. **Seleccionar o dispositivo de origem**: escolha o disco interno principal.

3. **Seleccionar o dispositivo de destino**: escolha o disco externo de cópia de segurança como destino. Tenha cuidado ao seleccionar o dispositivo para evitar sobrescrever dados importantes. Certifique-se de que o disco de destino tem tamanho igual ou superior ao de origem.

4. **Iniciar o processo de cópia de segurança**: o Clonezilla iniciará o processo. Consoante o tamanho da partição e a velocidade dos discos, pode demorar desde vários minutos até algumas horas.

5. **Etiquetar a cópia de segurança**: após concluir, etiquete a pen USB e o disco externo com a data e o sistema copiado. Guarde-os num local seguro.

---

### Restaurar a partir da cópia de segurança

Se precisar de restaurar o sistema Debian a partir da cópia de segurança, siga estes passos:

1. **Arrancar a partir do suporte Clonezilla**: insira a pen USB Clonezilla e arranque a partir dela, seguindo os mesmos passos do processo de cópia de segurança.

2. **Seleccionar o modo de restauro**: escolha novamente o modo «device-device», mas desta vez restaurará a partir da imagem de cópia de segurança. Isto copiará todos os dados do disco externo de volta para o disco interno.

3. **Seleccionar o dispositivo de origem**: escolha o disco externo onde a cópia de segurança está armazenada.

4. **Seleccionar o dispositivo de destino**: seleccione o disco interno onde pretende restaurar a cópia de segurança.

5. **Iniciar o processo de restauro**: o Clonezilla iniciará o restauro. Tal como na cópia de segurança, o tempo necessário dependerá do tamanho do disco e da velocidade do hardware.

---

## Notas finais

As cópias de segurança de disco com o Clonezilla garantem que todo o sistema — sistema operativo, definições e aplicações — fica preservado. Com esforço mínimo, pode proteger o sistema contra falhas catastróficas e reduzir o tempo de inactividade em caso de avaria.

Lembre-se: **as cópias de segurança são essenciais**. Actualize-as regularmente e teste-as periodicamente para garantir que consegue restaurar o sistema quando necessário.

Após arrancar, pode ligar o disco externo de cópia de segurança e inspeccionar a estrutura de partições com a aplicação Discos no Linux. O disco de cópia de segurança deve reflectir a estrutura do disco interno, com as mesmas partições e algum espaço não utilizado se o disco externo for maior.

