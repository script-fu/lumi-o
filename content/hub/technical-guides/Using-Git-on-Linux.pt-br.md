---
title: "Usando Git no Linux"
type: docs
url: "hub/technical-guides/Using-Git-on-Linux"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 7054a9ff9efeb93b4f494197e09ed1fe34d5d6bde7bc305480693c3982d375ae
---

Bem-vindo a este guia para iniciantes sobre o uso do Git no Linux! Este guia foi feito para ajudá-lo a começar com Git e GitLab e para dar uma compreensão básica de como usar essas ferramentas.

## Visão geral do Git

O código usado para criar aplicativos fica em uma coleção de pastas e arquivos no seu sistema. Git é um aplicativo que permite fazer backup, compartilhar e copiar essa coleção. Git é um sistema de controle de versão que permite acompanhar alterações no código e colaborar com outras pessoas. É uma ferramenta poderosa, muito usada na comunidade open source. GitLab é uma plataforma web que permite hospedar e gerenciar seus repositórios Git online, facilitando a colaboração e o acompanhamento de alterações no código.

## O que é um repositório?

Um _repo_, abreviação de repositório, é uma pasta local gerenciada pelo Git com uma cópia online. Um repo do GitLab é uma coleção de arquivos e pastas que formam um projeto. Ele pode ter _branches_, cópias independentes do mesmo projeto. Um branch é uma versão separada do projeto que permite fazer alterações sem afetar a versão principal. Isso é útil para testar novos recursos ou corrigir bugs sem interromper o projeto principal. Existe o repo local, armazenado no disco rígido, e o repo remoto, armazenado online com Git e GitLab.

## Usando Git

Você precisa instalar o Git no seu sistema. Em sistemas baseados em Debian, use o comando apt para instalar pacotes de software. Neste caso, usamos o apt para instalar o Git, um pacote que fornece o sistema de controle de versão Git. O comando sudo dá permissão ao instalador para instalar no seu sistema.

```bash
 sudo apt install git
```

## Acesso ao GitLab

Antes de usar o [GitLab](https://gitlab.com/users/sign_up), você precisa criar uma conta visitando o site do GitLab e concluindo o processo de registro.

O GitLab exige _SSH_ para comunicação segura e autenticada entre um cliente (você, por exemplo) e o servidor GitLab ao executar operações Git como _clone_, _push_ e _fetch_ de repositórios. Clone é criar uma cópia local do repo, fetch traz alterações do repo para a cópia local, e push envia alterações e conteúdo para o repo no servidor. SSH (Secure Shell) é um protocolo de rede que permite acesso remoto seguro e usa _pares de chaves_ para autenticar e estabelecer conexões seguras. Para gerar um par de chaves SSH, use o comando ssh-keygen no terminal.

```bash
 ssh-keygen
```

Informe um nome de arquivo ou pressione Enter para usar o padrão e, opcionalmente, uma senha. No diretório home, em uma pasta oculta chamada .ssh, haverá dois arquivos id_rsa se você usou os nomes padrão. O arquivo .pub é a chave pública; você pode ver o conteúdo com um editor de texto.

Faça login na sua conta GitLab e vá às configurações de usuário. Clique em 'SSH Keys' no menu de navegação à esquerda. Copie e cole sua chave pública no campo Key e dê um título relevante à chave, como PC@Home. Clique em 'Add Key' para salvar a chave. Sua chave pública SSH agora está na conta GitLab e pode ser usada para autenticar em repositórios GitLab. Teste se as chaves e a conexão funcionam com o comando ssh -T; você deve ver uma mensagem de boas-vindas do GitLab.

```bash
 $ ssh -T git@ssh.gitlab.gnome.org
 Welcome to GitLab, @username!
```

## Comandos básicos do Git

Agora que você instalou o Git e configurou sua chave SSH no GitLab, vamos ver alguns comandos essenciais do Git para gerenciar repositórios. Esses comandos ajudam a trabalhar com projetos existentes, mantê-los atualizados e fazer alterações com segurança.

### 1. **Clonando um repositório**

Clonar é o processo de criar uma cópia local de um repositório remoto. Isso é útil quando você quer trabalhar em um projeto que já existe no GitLab. Para clonar um repositório, use o comando `git clone` seguido da URL do repositório:

```sh
git clone https://gitlab.com/username/repository.git
```

Substitua `https://gitlab.com/username/repository.git` pela URL do repositório que deseja clonar. Este comando criará uma cópia local do repositório em um novo diretório.

### 2. **Verificando o status do repositório**

Para ver se o repositório local tem alterações ou para visualizar o estado atual, use:

```sh
git status
```

Este comando mostra quais arquivos foram modificados, adicionados ou excluídos na cópia local do repositório.

### 3. **Repositórios remotos**

Repositórios remotos são versões do projeto hospedadas online, como no GitLab. Eles servem como o local central onde o código é armazenado e pode ser acessado por outras pessoas. O repositório remoto padrão que o Git cria ao clonar um projeto se chama `origin`. Você pode adicionar, remover ou listar repositórios remotos com os seguintes comandos:

- **Listando repositórios remotos:**

  Para ver quais repositórios remotos estão vinculados ao projeto local, use:

  ```sh
  git remote -v
  ```

  Este comando lista todos os repositórios remotos e suas URLs. Normalmente, você verá `origin` listado aqui.

- **Adicionando um repositório remoto:**

  Se precisar adicionar um novo repositório remoto, use:

  ```sh
  git remote add <name> <url>
  ```

  Substitua `<name>` por um nome para o repositório remoto e `<url>` pela URL do repositório.

- **Removendo um repositório remoto:**

  Para remover um repositório remoto, use:

  ```sh
  git remote remove <name>
  ```

  Substitua `<name>` pelo nome do repositório remoto que deseja remover.

### 4. **Buscando alterações do repositório remoto**

Se quiser ver quais alterações foram feitas no repositório remoto sem aplicá-las à cópia local, use:

```sh
git fetch origin
```

Este comando busca as alterações mais recentes do repositório remoto, mas não as mescla no branch local. É uma forma de verificar atualizações antes de decidir incorporá-las.

### 5. **Redefinindo o repositório local**

Se quiser redefinir o repositório local para corresponder exatamente ao repositório remoto, use um reset 'hard'. **Aviso:** isso sobrescreverá quaisquer alterações locais que você tenha feito.

```sh
git reset --hard origin/branch-name
```

Substitua `branch-name` pelo nome do branch que deseja redefinir. Este comando descartará todas as alterações locais e tornará o repositório local idêntico ao repositório remoto.

### 6. **Visualizando o histórico de commits**

Para ver uma lista de alterações feitas no repositório ao longo do tempo, use:

```sh
git log
```

Este comando exibe um histórico de commits, incluindo autor, data e mensagem de cada alteração. É útil para entender quais mudanças foram feitas e quando.

### Resumo

Esses comandos básicos do Git ajudam a trabalhar com repositórios, manter as cópias locais atualizadas e gerenciar repositórios remotos com segurança. Clonar repositórios, verificar o status da cópia local e gerenciar repositórios remotos são habilidades essenciais para gerenciar projetos com Git.
