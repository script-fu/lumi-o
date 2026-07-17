---
title: "Git"
type: docs
translation_provenance: ai-reviewed
translation_source_sha256: c2c03721fbcc205a8c33d945786290712bc60e71beb18b9a1dda1a34d975051f
url: "hub/scripting/tools/git"
translation_lock: true
---
Use o Git para acompanhar alterações nos plug-ins, reverter erros e partilhar código entre máquinas.

## Por que organizar o código?

Depois de ter mais de um script, uma estrutura de pastas consistente poupa tempo e simplifica o controlo de versões.

## Configurando uma estrutura de pasta de código

Uma das formas mais simples de organizar os projetos é criar uma **pasta de código** dedicada na máquina local. Dentro desta pasta, pode criar subpastas para cada projeto ou repositório. Aqui está uma estrutura de pastas recomendada:

```plaintext
/home/your-username/code/
 ├── project1/
 ├── project2/
 └── project3/
```

Cada subpasta (por exemplo, `project1`) representa um **repositório**, onde armazenará os ficheiros e o código desse projeto.

## O que é um repositório?

Um **repositório** (ou **repo**) é essencialmente uma pasta com conteúdo que o Git rastreia. Ao criar um repositório localmente, inicializa o Git dentro dessa pasta, permitindo guardar quaisquer alterações em um clone online.

### Repositórios locais e remotos

- **Repositório Local**: Este é o repositório armazenado no computador, em uma das pastas do projeto.
- **Remote Repo**: uma versão do repositório armazenada online (por exemplo, no GitLab ou GitHub).

## Usando Git e GitHub

Depois que a estrutura de pastas estiver definida, poderá inicializar o Git e conectar os projetos locais ao GitHub. Siga estas etapas para começar:

### Etapas básicas para usar Git e GitHub

1. **Instale o Git**
2. **Crie uma conta GitHub**
3. **Crie um repositório em branco no GitHub**
4. **Inicialize o Git no projeto local**
5. **Conecte o repositório local ao GitHub**
6. **Prepare os ficheiros**
7. **Confirme as alterações**
8. **Envie as alterações para o GitHub**
9. **Veja o repositório on-line**

### 1. Instale o Git

Se ainda não instalou o Git, pode fazer isso no Linux usando:

```sh
sudo apt install git
```

### 2. Crie uma conta GitHub

Se ainda não possui uma conta, visite [GitHub](https://github.com/) para se registar. Uma vez registado, pode criar repositórios no GitHub para armazenar o código online.

### 3. Crie um repositório em branco no GitHub

1. **Faça login no GitHub**: Vá para [GitHub](https://github.com/) e faça login na conta.
2. **Crie um novo repositório**:
 - Clique no ícone **+** no canto superior direito e selecione **Novo repositório**.
 - Insira um nome de repositório (por exemplo, `your-repository`).
 - Adicione uma descrição, se desejar.
 - Escolha visibilidade **Pública** ou **Privada**.
 - **Não** inicialize o repositório com um README, `.gitignore` ou licença (para evitar conflitos).
 - Clique em **Criar repositório**.

### 4. Inicialize o Git no projeto local

Para começar a acompanhar uma pasta de projeto com Git, abra o terminal, navegue até a pasta do projeto e execute:

```sh
cd code/your/project/folder
git init
```

Este comando inicializa um repositório Git vazio na pasta do projeto.

### 5. Conecte o repositório local ao GitHub

Em seguida, ligue o repositório local ao GitHub. Depois de criar um repositório em branco no GitHub, adicione-o como remoto ao projeto local:

```sh
cd code/your/project/folder
git remote add origin https://github.com/your-username/your-repository.git
```

Substitua `your-username` e `your-repository` pelo nome de utilizador real do GitHub e pelo nome do repositório. Este comando vincula o projeto local ao repositório remoto no GitHub.

### 6. Prepare os ficheiros

Antes de guardar as alterações no Git, é preciso indicar ao Git quais ficheiros alterados deseja guardar. Isto chama-se «preparação» dos ficheiros. Use o seguinte comando para preparar todos os ficheiros novos ou modificados:

```sh
git add .
```

Isto diz ao Git para acompanhar as alterações feitas em todos os ficheiros do projeto. Também pode preparar ficheiros específicos substituindo `.` pelo nome do ficheiro.

### 7. Confirme as alterações

Após a preparação, a próxima etapa é guardar (ou "fazer commit") as alterações no repositório Git local. Ao fazer commit, deve sempre incluir uma mensagem que descreva as alterações efectuadas. Por exemplo:

```sh
git commit -m "Add new feature"
```

O sinalizador `-m` permite escrever uma mensagem que resume as alterações feitas. Esta mensagem ajuda-o a si e a outras pessoas a entender o que foi modificado neste commit.

### 8. Envie as alterações para o GitHub

Depois de fazer commit das alterações localmente, pode enviá-las para o GitHub para que o repositório remoto seja actualizado. Execute o seguinte comando para enviar as alterações:

```sh
git push -u origin main
```

O `main` branch é o branch padrão no GitHub onde o código é armazenado, e este comando carrega as alterações locais para o repositório remoto, tornando-as acessíveis online.

### 9. Visualize o código no GitHub

Depois de enviar o código para o GitHub, poderá visualizar o repositório na interface da web do GitHub. Deverá ver os ficheiros do repositório local, junto com um histórico de commits mostrando as alterações feitas.

## Conclusão

Ao organizar o código em pastas dedicadas e usar o GitHub para gerir e fazer backup de os repositórios, manterá os projetos bem estruturados e facilmente acessíveis. Depois de ter uma versão funcional do código, envie-o para o GitHub. Pode então acompanhar facilmente quaisquer alterações usando a interface da web do GitHub ou o Visual Studio Code, que destaca as linhas modificadas. Essa abordagem permite continuar refinando e expandindo o código sem perder o controlo do progresso ou das alterações.

Git e plataformas como GitHub e GitLab são ferramentas poderosas e, embora possam ser complexas, existem vários recursos disponíveis online para ajudar a entendê-los melhor. Um dos recursos mais valiosos que encontrei são ajudantes de IA como o ChatGPT. Pode descrever o que precisa realizar e essas ferramentas o guiarão pacientemente pelo processo, passo a passo.

## Glossário

Aqui estão alguns termos comuns que encontrará ao trabalhar com Git e GitHub:

- **Commit**: um instantâneo de as alterações no repositório. Cada commit inclui uma mensagem descrevendo o que foi alterado e cria um registo histórico que pode consultar ou reverter posteriormente.
- **Repositório (Repo)**: Uma coleção de ficheiros e o respetivo histórico rastreado pelo Git. Os repositórios podem existir localmente no computador ou remotamente em plataformas como GitHub. Cada projeto normalmente é armazenado no próprio repositório.
- **Remoto**: um repositório remoto é uma versão do projeto hospedada em uma plataforma como o GitHub. A versão local do projeto no computador está vinculada a este controlo remoto para que possa fazer envio (push) e download (pull) de alterações.
- **Preparação**: O processo de preparação de ficheiros para um commit. Ao preparar um ficheiro, está dizendo ao Git que deseja incluí-lo no próximo commit. A preparação permite escolher quais alterações incluir em um commit.
- **Push**: O ato de enviar as alterações confirmadas do repositório local para um repositório remoto (por exemplo, GitHub), para que outros possam acessar a versão atualizada do código.
- **Pull**: O ato de buscar alterações de um repositório remoto para atualizar a cópia local. Obtém alterações quando deseja sincronizar o repositório local com a versão mais recente do controlo remoto.
- **Origem**: O nome padrão para um repositório remoto quando conecta o repositório local a um repositório remoto pela primeira vez. Normalmente refere-se ao URL principal do projeto no GitHub.