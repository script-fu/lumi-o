---
title: "Git"
type: docs
---
Use o Git para rastrear alterações em seus plug-ins, reverter erros e compartilhar código entre máquinas.

## Por que organizar seu código?

Depois de ter mais de um script, uma estrutura de pastas consistente economiza tempo e simplifica o controle de versão.

## Configurando uma estrutura de pasta de código

Uma das maneiras mais simples de organizar seus projetos é criar uma **pasta de código** dedicada em sua máquina local. Dentro desta pasta, você pode criar subpastas para cada projeto ou repositório. Aqui está uma estrutura de pastas recomendada:

```plaintext
/home/your-username/code/
  ├── project1/
  ├── project2/
  └── project3/
```

Cada subpasta (por exemplo, `project1`) representa um **repositório**, onde você armazenará os arquivos e o código desse projeto.

## O que é um repositório?

Um **repositório** (ou **repo**) é essencialmente uma pasta com conteúdo que o Git rastreia. Ao criar um repositório localmente, você inicializa o Git dentro dessa pasta, permitindo salvar quaisquer alterações em um clone online.

### Repositórios locais e remotos

- **Repositório Local**: Este é o repositório armazenado no seu computador, em uma das pastas do seu projeto.
- **Remote Repo**: uma versão do repositório armazenada online (por exemplo, no GitLab ou GitHub).

## Usando Git e GitHub

Depois que sua estrutura de pastas estiver definida, você poderá inicializar o Git e conectar seus projetos locais ao GitHub. Siga estas etapas para começar:

### Etapas básicas para usar Git e GitHub

1. **Instale o Git**
2. **Crie uma conta GitHub**
3. **Crie um repositório em branco no GitHub**
4. **Inicialize o Git em seu projeto local**
5. **Conecte seu repositório local ao GitHub**
6. **Prepare seus arquivos**
7. **Comprometa suas alterações**
8. **Envie suas alterações para o GitHub**
9. **Veja seu repositório on-line**

### 1. Instale o Git

Se você ainda não instalou o Git, você pode fazer isso no Linux usando:

```sh
sudo apt install git
```

### 2. Crie uma conta GitHub

Se você ainda não possui uma conta, visite [GitHub](https://github.com/) para se inscrever. Uma vez registrado, você pode criar repositórios no GitHub para armazenar seu código online.

### 3. Crie um repositório em branco no GitHub

1. **Faça login no GitHub**: Vá para [GitHub](https://github.com/) e faça login em sua conta.
2. **Crie um novo repositório**:
   - Clique no ícone **+** no canto superior direito e selecione **Novo repositório**.
   - Insira um nome de repositório (por exemplo, `your-repository`).
   - Adicione uma descrição, se desejar.
   - Escolha visibilidade **Pública** ou **Privada**.
   - **Não** inicialize o repositório com um README, `.gitignore` ou licença (para evitar conflitos).
   - Clique em **Criar repositório**.

### 4. Inicialize o Git em seu projeto local

Para começar a rastrear uma pasta de projeto com Git, abra seu terminal, navegue até a pasta do projeto e execute:

```sh
cd code/your/project/folder
git init
```

Este comando inicializa um repositório Git vazio na pasta do seu projeto.

### 5. Conecte seu repositório local ao GitHub

Em seguida, você desejará conectar seu repositório local ao GitHub. Depois de criar um repositório em branco no GitHub, adicione-o como remoto ao seu projeto local:

```sh
cd code/your/project/folder
git remote add origin https://github.com/your-username/your-repository.git
```

Substitua `your-username` e `your-repository` pelo seu nome de usuário real do GitHub e o nome do repositório. Este comando vincula seu projeto local ao repositório remoto no GitHub.

### 6. Prepare seus arquivos

Antes de salvar suas alterações no Git, você precisa informar ao Git quais arquivos você alterou e deseja salvar. Isso é chamado de "preparação" de seus arquivos. Use o seguinte comando para preparar todos os arquivos novos ou modificados:

```sh
git add .
```Isso diz ao Git para rastrear as alterações feitas em todos os arquivos do seu projeto. Você também pode preparar arquivos específicos substituindo `.` pelo nome do arquivo.

### 7. Confirme suas alterações

Após a preparação, a próxima etapa é salvar (ou "confirmar") as alterações em seu repositório Git local. Ao confirmar, você deve sempre incluir uma mensagem que descreva as alterações que você fez. Por exemplo:

```sh
git commit -m "Add new feature"
```

O sinalizador `-m` permite que você escreva uma mensagem que resume as alterações feitas. Esta mensagem ajuda você e outras pessoas a entender o que foi modificado neste commit.

### 8. Envie suas alterações para o GitHub

Depois de confirmar as alterações localmente, você pode "enviá-las" para o GitHub para que seu repositório remoto seja atualizado. Execute o seguinte comando para fazer upload de suas alterações:

```sh
git push -u origin main
```

O `main` branch é o branch padrão no GitHub onde o código é armazenado, e este comando carrega suas alterações locais para o repositório remoto, tornando-as acessíveis online.

### 9. Visualize seu código no GitHub

Depois de enviar seu código para o GitHub, você poderá visualizar seu repositório na interface da web do GitHub. Você deverá ver os arquivos do seu repositório local, junto com um histórico de commits mostrando as alterações feitas.

## Conclusão

Ao organizar seu código em pastas dedicadas e usar o GitHub para gerenciar e fazer backup de seus repositórios, você manterá seus projetos bem estruturados e facilmente acessíveis. Depois de ter uma versão funcional do seu código, envie-o para o GitHub. Você pode então rastrear facilmente quaisquer alterações usando a interface da web do GitHub ou o Visual Studio Code, que destaca as linhas modificadas. Essa abordagem permite que você continue refinando e expandindo seu código sem perder o controle do progresso ou das alterações.

Git e plataformas como GitHub e GitLab são ferramentas poderosas e, embora possam ser complexas, existem vários recursos disponíveis online para ajudar você a entendê-los melhor. Um dos recursos mais valiosos que encontrei são ajudantes de IA como o ChatGPT. Você pode descrever o que precisa realizar e essas ferramentas o guiarão pacientemente pelo processo, passo a passo.

## Glossário

Aqui estão alguns termos comuns que você encontrará ao trabalhar com Git e GitHub:- **Commit**: um instantâneo de suas alterações no repositório. Cada commit inclui uma mensagem descrevendo o que foi alterado e cria um registro histórico que você pode consultar ou reverter posteriormente.
- **Repositório (Repo)**: Uma coleção de arquivos e seu histórico rastreado pelo Git. Os repositórios podem existir localmente no seu computador ou remotamente em plataformas como GitHub. Cada projeto normalmente é armazenado em seu próprio repositório.
- **Remoto**: um repositório remoto é uma versão do seu projeto hospedada em uma plataforma como o GitHub. A versão local do seu projeto no seu computador está vinculada a este controle remoto para que você possa fazer upload (push) e download (pull) de alterações.
- **Staging**: O processo de preparação de arquivos para um commit. Ao preparar um arquivo, você está dizendo ao Git que deseja incluí-lo no próximo commit. O teste permite que você escolha quais alterações incluir em um commit.
- **Push**: O ato de enviar suas alterações confirmadas do seu repositório local para um repositório remoto (por exemplo, GitHub), para que outros possam acessar a versão atualizada do seu código.
- **Pull**: O ato de buscar alterações de um repositório remoto para atualizar sua cópia local. Você obtém alterações quando deseja sincronizar seu repositório local com a versão mais recente do controle remoto.
- **Origem**: O nome padrão para um repositório remoto quando você conecta seu repositório local a um repositório remoto pela primeira vez. Normalmente refere-se ao URL principal do seu projeto no GitHub.