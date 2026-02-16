---
title: "Backup do sistema usando Clonezilla"
type: docs
url: "hub/install-linux/System-Backup-Clonezilla"
---
É comum fazer backup de seus arquivos importantes para retornar a versões anteriores ou substituir dados corrompidos. No entanto, outro tipo essencial de backup é um **clone de disco**, um backup completo do estado do seu sistema.

Depois de ter seu sistema configurado e funcionando bem, criar um backup completo é crucial para restaurar seu ambiente em caso de desastre. Este backup complementa o salvamento regular de seus dados de trabalho.

[Clonezilla](https://clonezilla.org/) é um software de clonagem e imagem de disco gratuito e de código aberto. Ele permite aos usuários criar e restaurar backups completos do disco rígido de seus computadores, tornando-o uma ferramenta popular tanto para profissionais de TI quanto para usuários domésticos.

É sempre melhor ter um backup e não precisar dele do que precisar de um backup e não tê-lo.


## Principais recursos do Clonezilla

- **Imagem de disco**: o Clonezilla cria uma cópia exata de um disco rígido, incluindo o sistema operacional, aplicativos e dados.
- **Backup e Restauração**: permite criar uma imagem de backup de um disco rígido e restaurá-la em caso de falha ou migração para uma nova unidade.
- **Gratuito e de código aberto**: o uso do Clonezilla é totalmente gratuito e o código-fonte está disponível para modificação e personalização.


## Usando o Clonezilla para fazer backup

### Etapas de preparação

Você precisará de uma unidade USB para o Clonezilla e de um disco rígido externo maior que a unidade interna que você pretende clonar.

Essas etapas simplificam o processo com base no [official guide](https://clonezilla.org//fine-print-live-doc.php?path=./clonezilla-live/doc/01_Save_disk_image/00-boot-clonezilla-live-cd.doc#00-boot-clonezilla-live-cd.doc). É uma boa ideia revisar o guia completo, que inclui capturas de tela para maior clareza.

1. **Crie um Clonezilla Live USB ou CD/DVD**: Siga as instruções detalhadas no Clonezilla [website](https://clonezilla.org/liveusb.php) para criar um USB ou CD/DVD inicializável.

2. **Conecte sua unidade de backup externa**: Conecte sua unidade externa e certifique-se de que ela seja reconhecida pelo seu sistema. Este será o destino do seu backup.

3. **Verifique o layout da partição**: Use o comando `lsblk` em um terminal para verificar o layout da partição do seu disco rígido principal. Observe o nome do dispositivo principal.

4. **Inicializar a partir da unidade Clonezilla Live USB**: Reinicie o computador e inicialize a partir da mídia Clonezilla que você criou. Pode ser necessário acessar as configurações do BIOS/UEFI (geralmente pressionando F2, F12, ESC ou DEL durante a inicialização) e ajustar a ordem de inicialização para priorizar a unidade USB.



### Backup com Clonezilla

1. **Selecione o modo de backup**: Assim que o Clonezilla inicializar, escolha o modo "dispositivo-dispositivo". Este modo permite clonar diretamente sua unidade interna em um dispositivo externo.

2. **Selecione o dispositivo de origem**: Escolha a unidade interna principal.

3. **Selecione o dispositivo de destino**: Escolha sua unidade de backup externa como o dispositivo de destino. Tenha cuidado ao selecionar o dispositivo para evitar a substituição de dados importantes. Certifique-se de que a unidade de destino seja igual ou maior que a unidade de origem.

4. **Iniciar o processo de backup**: o Clonezilla iniciará o processo de backup. Dependendo do tamanho da sua partição e da velocidade das suas unidades, isso pode levar de vários minutos a algumas horas.

5. **Etiquete seu backup**: Após a conclusão do backup, etiquete a unidade USB e o disco rígido externo com a data e o sistema do qual você fez backup. Guarde-os em um local seguro.

---

### Restaurando do backup

Se você precisar restaurar seu sistema Debian a partir do backup, siga estas etapas:

1. **Inicialização a partir do Clonezilla Media**: Insira o Clonezilla USB e inicialize a partir dele, seguindo as mesmas etapas do processo de backup.2. **Selecione Modo de restauração**: Escolha o modo "dispositivo-dispositivo" novamente, mas desta vez, você restaurará a partir da imagem de backup. Isso copiará todos os dados da unidade externa de volta para a unidade interna.

3. **Selecione o dispositivo de origem**: Escolha sua unidade externa onde o backup está armazenado.

4. **Selecione o dispositivo de destino**: Selecione a unidade interna onde deseja restaurar o backup.

5. **Iniciar o processo de restauração**: o Clonezilla iniciará o processo de restauração. Tal como acontece com o backup, o tempo necessário dependerá do tamanho da unidade e da velocidade do seu hardware.

---

## Notas Finais

Os backups de disco com o Clonezilla garantem que todo o seu sistema – sistema operacional, configurações e aplicativos – seja preservado. Com esforço mínimo, você pode proteger seu sistema contra falhas catastróficas e minimizar o tempo de inatividade em caso de falha.

Lembre-se: **backups são essenciais**. Atualize regularmente seus backups e teste-os periodicamente para garantir que você possa restaurar seu sistema quando necessário.

Após a inicialização, você pode conectar sua unidade de backup externa e inspecionar sua estrutura de partição usando o utilitário Discos no Linux. A unidade de backup deve espelhar a estrutura da unidade interna, com as mesmas partições e algum espaço não utilizado se a unidade externa for maior.