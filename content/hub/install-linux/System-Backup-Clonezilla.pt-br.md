---
title: "Backup do sistema com Clonezilla"
type: docs
url: "hub/install-linux/System-Backup-Clonezilla"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: b3453289d7da56bb4fc9039616edb73e537acd6a722f0eb8a000e4a398016863
---

É comum fazer backup de arquivos importantes para voltar a versões anteriores ou substituir dados corrompidos. Outro tipo essencial de backup, porém, é o **clone de disco** — uma cópia completa do estado do sistema.

Depois que o sistema estiver configurado e funcionando bem, um backup completo é crucial para restaurar o ambiente em caso de desastre. Esse backup complementa o salvamento regular dos seus dados de trabalho.

[Clonezilla](https://clonezilla.org/) é um software gratuito e de código aberto para imagem e clonagem de discos. Permite criar e restaurar backups completos do disco rígido do computador — uma ferramenta popular tanto para profissionais de TI quanto para usuários domésticos.

Sempre é melhor ter um backup e não precisar dele do que precisar de um backup e não tê-lo.


## Principais recursos do Clonezilla

- **Imagem de disco**: o Clonezilla cria uma cópia exata de um disco rígido, incluindo sistema operacional, aplicativos e dados.
- **Backup e restauração**: permite criar uma imagem de backup de um disco e restaurá-la em caso de falha ou migração para um novo disco.
- **Gratuito e de código aberto**: o Clonezilla é totalmente gratuito; o código-fonte está disponível para modificação e personalização.


## Usar o Clonezilla para backup

### Etapas de preparação

Você precisará de uma unidade USB para o Clonezilla e de um disco rígido externo maior que a unidade interna que pretende clonar.

Estas etapas simplificam o processo com base no [guia oficial](https://clonezilla.org//fine-print-live-doc.php?path=./clonezilla-live/doc/01_Save_disk_image/00-boot-clonezilla-live-cd.doc#00-boot-clonezilla-live-cd.doc). Vale a pena revisar o guia completo, que inclui capturas de tela.

1. **Crie um Clonezilla Live USB ou CD/DVD**: siga as instruções detalhadas no [site](https://clonezilla.org/liveusb.php) do Clonezilla para criar um USB ou CD/DVD inicializável.

2. **Conecte o disco de backup externo**: conecte a unidade externa e verifique se o sistema a reconhece. Esse será o destino do backup.

3. **Verifique o layout de partições**: use o comando `lsblk` em um terminal para verificar o layout de partições do disco principal. Anote o nome do dispositivo principal.

4. **Inicialize a partir do Clonezilla Live USB**: reinicie o computador e inicialize a partir da mídia Clonezilla criada. Pode ser necessário acessar as configurações de BIOS/UEFI (geralmente pressionando F2, F12, ESC ou DEL durante a inicialização) e ajustar a ordem de boot para priorizar o USB.



### Backup com Clonezilla

1. **Selecione o modo de backup**: quando o Clonezilla iniciar, escolha o modo "device-device". Esse modo permite clonar diretamente a unidade interna para um dispositivo externo.

2. **Selecione o dispositivo de origem**: escolha a unidade interna principal.

3. **Selecione o dispositivo de destino**: escolha o disco de backup externo como destino. Tenha cuidado ao selecionar o dispositivo para não sobrescrever dados importantes. A unidade de destino deve ser igual ou maior que a de origem.

4. **Inicie o processo de backup**: o Clonezilla iniciará o backup. Dependendo do tamanho da partição e da velocidade dos discos, isso pode levar de alguns minutos a algumas horas.

5. **Identifique o backup**: após a conclusão, identifique a unidade USB e o disco externo com a data e o sistema que você copiou. Guarde-os em local seguro.

---

### Restaurar a partir do backup

Se precisar restaurar o sistema Debian a partir do backup, siga estas etapas:

1. **Inicialize a partir da mídia Clonezilla**: insira o USB do Clonezilla e inicialize a partir dele, seguindo os mesmos passos do processo de backup.

2. **Selecione o modo de restauração**: escolha novamente o modo "device-device", mas desta vez restaure a partir da imagem de backup. Todos os dados serão copiados da unidade externa de volta para a interna.

3. **Selecione o dispositivo de origem**: escolha a unidade externa onde o backup está armazenado.

4. **Selecione o dispositivo de destino**: selecione a unidade interna onde deseja restaurar o backup.

5. **Inicie o processo de restauração**: o Clonezilla iniciará a restauração. Como no backup, o tempo dependerá do tamanho do disco e da velocidade do hardware.

---

## Considerações finais

Backups de disco com Clonezilla preservam todo o sistema — sistema operacional, configurações e aplicativos. Com pouco esforço, você protege o sistema contra falhas graves e reduz o tempo de inatividade após uma queda.

Lembre-se: **backups são essenciais**. Atualize-os regularmente e teste periodicamente para garantir que poderá restaurar o sistema quando necessário.

Após a inicialização, você pode conectar o disco de backup externo e inspecionar a estrutura de partições com o utilitário Disks no Linux. O disco de backup deve espelhar a estrutura da unidade interna, com as mesmas partições e algum espaço não utilizado se a unidade externa for maior.

