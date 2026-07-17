---
title: "Usar Git no Linux"
type: docs
url: "hub/technical-guides/Using-Git-on-Linux"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 7054a9ff9efeb93b4f494197e09ed1fe34d5d6bde7bc305480693c3982d375ae
---
Bem-vindo a este guia introdutório sobre Git no Linux! Foi concebido para ajudar a começar com Git e GitLab e para fornecer uma compreensão básica de como usar estas ferramentas.

## Visão geral do Git

O código usado para criar aplicações mantém-se numa colecção de pastas e ficheiros no sistema. O Git é uma aplicação que permite fazer cópias de segurança, partilhar e copiar essa colecção. É um sistema de controlo de versões que permite acompanhar alterações ao código e colaborar com outras pessoas. É uma ferramenta poderosa, amplamente usada na comunidade de código aberto. O GitLab é uma plataforma web que permite alojar e gerir repositórios Git online, facilitando a colaboração e o acompanhamento de alterações ao código.

## O que é um repositório?

Um _repo_, abreviatura de repositório, é uma pasta local gerida pelo Git com uma cópia online. Um repositório GitLab é uma colecção de ficheiros e pastas que constituem um projecto. Pode ter _ramos_ que são cópias independentes do mesmo projecto. Um ramo é uma versão separada do projecto que permite fazer alterações sem afectar a versão principal. Isto é útil para testar novas funcionalidades ou corrigir erros sem interromper o projecto principal. Existe o repositório local, armazenado no disco rígido, e o repositório remoto, armazenado online com Git e GitLab.

## Usar o Git

Será necessário instalar o Git no sistema. Em sistemas baseados em Debian, pode usar o comando apt para instalar pacotes de software. Neste caso, usa-se para instalar o Git, um pacote que fornece o sistema de controlo de versões Git. O comando sudo dá permissão ao instalador para instalar no sistema.

```bash
 sudo apt install git
```

## Aceder ao GitLab

Antes de poder usar o [GitLab](https://gitlab.com/users/sign_up), será necessário criar uma conta visitando o site do GitLab e concluindo o registo.

O GitLab requer _SSH_ para comunicação segura e autenticada entre um cliente (por exemplo, o utilizador) e o servidor GitLab ao executar operações Git como _cloning_, _pushing_ e _fetching_ de repositórios. Clonar é fazer uma cópia local do repositório; fetch traz alterações feitas no repositório remoto para a cópia local; push envia alterações e conteúdo para o repositório no servidor. SSH (Secure Shell) é um protocolo de rede que permite acesso remoto seguro e usa _pares de chaves_ para autenticar e estabelecer ligações seguras. Para gerar um par de chaves SSH, pode usar o comando ssh-keygen no terminal.

```bash
 ssh-keygen
```

Especifique um nome de ficheiro ou use o predefinido premindo Enter e, opcionalmente, uma palavra-passe. Na pasta pessoal, numa pasta oculta chamada .ssh, existem agora dois ficheiros id_rsa, se usou os nomes predefinidos. O ficheiro .pub é a chave pública e pode ver o seu conteúdo com um editor de texto.

Inicie sessão na conta GitLab e navegue até às definições de utilizador. Clique em «Chaves SSH» no menu de navegação à esquerda. Copie e cole a chave pública no campo Chave e dê à chave um título relevante, como PC@Home. Clique no botão «Adicionar chave» para guardar a chave. A chave pública SSH fica então associada à conta GitLab e pode usá-la para autenticar repositórios GitLab. Teste se as chaves e a ligação funcionam com o comando ssh -T para ver uma mensagem de boas-vindas do GitLab.

```bash
 $ ssh -T git@ssh.gitlab.gnome.org
 Welcome to GitLab, @username!
```

## Comandos básicos do Git

Depois de instalar o Git e configurar a chave SSH com o GitLab, vejamos alguns comandos essenciais do Git para gerir repositórios. Estes comandos ajudam a trabalhar com projectos existentes, mantê-los actualizados e fazer alterações em segurança.

### 1. **Clonar um repositório**

Clonar é o processo de criar uma cópia local de um repositório remoto. Isto é útil quando se pretende trabalhar num projecto que já existe no GitLab. Para clonar um repositório, use o comando `git clone` seguido do URL do repositório:

```sh
git clone https://gitlab.com/username/repository.git
```

Substitua `https://gitlab.com/username/repository.git` pelo URL do repositório que pretende clonar. Este comando cria uma cópia local do repositório num novo directório.

### 2. **Verificar o estado do repositório**

Para ver se o repositório local tem alterações ou para visualizar o estado actual, use:

```sh
git status
```

Este comando mostra quais os ficheiros modificados, adicionados ou eliminados na cópia local do repositório.

### 3. **Repositórios remotos**

Repositórios remotos são versões do projecto alojadas online, como no GitLab. Servem como local central onde o código é armazenado e pode ser acedido por outras pessoas. O repositório remoto predefinido que o Git cria ao clonar um projecto chama-se `origin`. Pode adicionar, remover ou listar repositórios remotos com os seguintes comandos:

- **Listar remotos:**

  Para ver quais os repositórios remotos ligados ao projecto local, use:

  ```sh
  git remote -v
  ```

  Este comando lista todos os remotos e os respetivos URLs. Normalmente, verá `origin` listado aqui.

- **Adicionar um remoto:**

  Se precisar de adicionar um novo repositório remoto, pode fazê-lo com:

  ```sh
  git remote add <name> <url>
  ```

  Substitua `<name>` por um nome para o remoto e `<url>` pelo URL do repositório.

- **Remover um remoto:**

  Para remover um repositório remoto, use:

  ```sh
  git remote remove <name>
  ```

  Substitua `<name>` pelo nome do remoto que pretende remover.

### 4. **Obter alterações do repositório remoto**

Se quiser ver que alterações foram feitas no repositório remoto sem as aplicar à cópia local, use:

```sh
git fetch origin
```

Este comando obtém as alterações mais recentes do repositório remoto, mas não as integra no ramo local. É uma forma de verificar actualizações antes de decidir incorporá-las.

### 5. **Repor o repositório local**

Se quiser repor o repositório local para corresponder exactamente ao repositório remoto, pode usar um reset «hard». **Aviso:** isto sobrescreve quaisquer alterações locais.

```sh
git reset --hard origin/branch-name
```

Substitua `branch-name` pelo nome do ramo que pretende repor. Este comando descarta quaisquer alterações locais e torna o repositório local idêntico ao remoto.

### 6. **Ver o historial de commits**

Para ver uma lista de alterações feitas ao repositório ao longo do tempo, use:

```sh
git log
```

Este comando apresenta um historial de commits, incluindo autor, data e mensagem de cada alteração. É útil para perceber que alterações foram feitas e quando.

### Resumo

Estes comandos básicos do Git ajudam a trabalhar com repositórios, manter cópias locais actualizadas e gerir repositórios remotos em segurança. Clonar repositórios, verificar o estado da cópia local e gerir repositórios remotos são competências essenciais para gerir projectos com Git.
