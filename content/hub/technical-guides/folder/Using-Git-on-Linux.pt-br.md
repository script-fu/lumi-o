---
title: "Usando Git no Linux"
type: docs
url: "hub/technical-guides/folder/Using-Git-on-Linux"
---
Bem-vindo a este guia para iniciantes no uso do Git no Linux! Este guia foi projetado para ajudá-lo a começar a usar o Git e o GitLab e para fornecer uma compreensão básica de como usar essas ferramentas.

## Visão geral do Git

O código usado para criar aplicativos é mantido em uma coleção de pastas e arquivos em seu sistema. Git é um aplicativo que nos permite fazer backup, compartilhar e copiar essa coleção. Git é conhecido como um sistema de controle de versão que permite rastrear alterações em seu código e colaborar com outras pessoas. É uma ferramenta poderosa amplamente utilizada na comunidade de código aberto. GitLab é uma plataforma baseada na web que permite hospedar e gerenciar seus repositórios Git online, facilitando a colaboração com outras pessoas e o rastreamento de alterações em seu código.

## O que é um repositório?

Um _repo_, abreviação de repositório, é uma pasta local gerenciada pelo Git com uma cópia online. Um repositório Git Lab é uma coleção de arquivos e pastas que constituem um projeto. Pode ter _ramificações_ que são cópias independentes do mesmo projeto. Uma ramificação é uma versão separada do seu projeto que permite fazer alterações sem afetar a versão principal. Isso é útil para testar novos recursos ou corrigir bugs sem interromper o projeto principal. Existe o seu repositório local, armazenado em seu disco rígido, e o repositório remoto, armazenado online usando Git e GitLab.

## Usando Git

Você precisará instalar o Git em seu sistema. Em sistemas baseados em Debian, você pode usar o comando apt para instalar pacotes de software. Nesse caso, estamos usando-o para instalar o Git, que é um pacote que fornece o sistema de controle de versão do Git. O comando sudo dá permissão ao instalador para instalar em seu sistema.

```bash
 sudo apt install git
```

## Acesse o GitLab

Antes de poder usar [GitLab](https://gitlab.com/users/sign_up), você precisará criar uma conta visitando o site do GitLab e concluindo o processo de registro.

GitLab requer _SSH_ para comunicação segura e autenticada entre um cliente (você, por exemplo) e o servidor GitLab ao executar operações Git como _cloning_, _pushing_ e _fetching_ repositórios. Clonar é fazer uma cópia local do repositório, buscar é trazer quaisquer alterações feitas no repositório para sua cópia local e enviar é enviar alterações e conteúdo para o repositório do servidor. SSH (Secure Shell) é um protocolo de rede que permite acesso remoto seguro e usa _pares de chaves_ para autenticar e estabelecer conexões seguras. Para gerar um par de chaves SSH, você pode usar o comando ssh-keygen em seu terminal.

```bash
 ssh-keygen
```

Especifique um nome de arquivo ou use o padrão pressionando Enter e, opcionalmente, uma senha. No seu diretório inicial, em uma pasta oculta chamada .ssh, agora existem dois arquivos id_rsa, se você usar nomes padrão. O arquivo .pub é a chave pública e você pode ver seu conteúdo com um editor de texto.

Faça login em sua conta GitLab e navegue até as configurações de usuário. Clique em ‘Chaves SSH’ no menu de navegação esquerdo. Copie e cole sua chave pública no campo Chave e dê à chave um título relevante, como PC@Home. Clique no botão 'Adicionar chave' para salvar a chave. Sua chave pública SSH agora foi adicionada à sua conta GitLab e você pode usá-la para autenticar nos repositórios GitLab. Teste se suas chaves e conexão estão funcionando com o comando ssh -T para ver uma mensagem de boas-vindas do GitLab.

```bash
 $ ssh -T git@ssh.gitlab.gnome.org
 Welcome to GitLab, @username!
```

## Comandos básicos do GitAgora que você instalou o Git e configurou sua chave SSH com o GitLab, vamos examinar alguns comandos essenciais do Git para gerenciar repositórios. Esses comandos irão ajudá-lo a trabalhar com projetos existentes, mantendo-os atualizados e fazendo alterações com segurança.

### 1. **Clonando um repositório**

Clonagem é o processo de criação de uma cópia local de um repositório remoto. Isso é útil quando você deseja trabalhar em um projeto que já existe no GitLab. Para clonar um repositório, use o comando `git clone` seguido da URL do repositório:

```sh
git clone https://gitlab.com/username/repository.git
```

Substitua `https://gitlab.com/username/repository.git` pela URL do repositório que você deseja clonar. Este comando criará uma cópia local do repositório em um novo diretório.

### 2. **Verificando o status do repositório**

Para ver se o seu repositório local sofreu alguma alteração ou para visualizar seu estado atual, use:

```sh
git status
```

Este comando mostrará quais arquivos foram modificados, adicionados ou excluídos em sua cópia local do repositório.

### 3. **Repositórios remotos**

Repositórios remotos são versões do seu projeto hospedadas online, como no GitLab. Eles servem como local central onde seu código é armazenado e pode ser acessado por outras pessoas. O repositório remoto padrão que o Git cria quando você clona um projeto é chamado `origin`. Você pode adicionar, remover ou listar repositórios remotos usando os seguintes comandos:

- **Listando controles remotos:**

  Para ver quais repositórios remotos estão vinculados ao seu projeto local, use:

  ```sh
  git remote -v
  ```

  Este comando lista todos os controles remotos e seus URLs. Normalmente, você verá `origin` listado aqui.

- **Adicionando um controle remoto:**

  Se precisar adicionar um novo repositório remoto, você pode fazer isso com:

  ```sh
  git remote add <name> <url>
  ```

  Substitua `<name>` por um nome para o controle remoto e `<url>` pela URL do repositório.

- **Removendo um controle remoto:**

  Para remover um repositório remoto, use:

  ```sh
  git remote remove <name>
  ```

  Substitua `<name>` pelo nome do controle remoto que você deseja remover.

### 4. **Buscando alterações do repositório remoto**

Se você quiser ver quais alterações foram feitas no repositório remoto sem aplicá-las à sua cópia local, use:

```sh
git fetch origin
```

Este comando busca as alterações mais recentes do repositório remoto, mas não as mescla em sua ramificação local. É uma forma de verificar se há atualizações antes de decidir incorporá-las.

### 5. **Redefinindo seu repositório local**

Se quiser redefinir seu repositório local para corresponder exatamente ao repositório remoto, você pode usar uma reinicialização 'hard'. **Aviso:** isso substituirá quaisquer alterações locais feitas.

```sh
git reset --hard origin/branch-name
```

Substitua `branch-name` pelo nome da ramificação que você deseja redefinir. Este comando descartará quaisquer alterações locais e tornará seu repositório local idêntico ao repositório remoto.

### 6. **Visualizando histórico de commits**

Para ver uma lista de alterações feitas no repositório ao longo do tempo, use:

```sh
git log
```

Este comando exibe um histórico de commits, incluindo autor, data e mensagem para cada alteração. É útil para entender quais alterações foram feitas e quando.

### Resumo

Esses comandos básicos do Git ajudarão você a trabalhar com repositórios, mantendo suas cópias locais atualizadas e garantindo que você possa gerenciar repositórios remotos com segurança. Clonar repositórios, verificar o status de sua cópia local e gerenciar repositórios remotos são habilidades essenciais para gerenciar projetos usando Git.