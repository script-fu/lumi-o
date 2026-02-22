---
title: "Recuperação de arquivos"
type: docs
---
Lumi mantém dois sistemas de recuperação independentes (salvamentos automáticos em segundo plano e pontos de verificação incrementais manuais), ambos acessíveis em uma única caixa de diálogo.

## Acesso

**Arquivo** → **Recuperar imagem**

A caixa de diálogo é aberta pré-preenchida com estados de recuperação para o arquivo atualmente aberto. Use o seletor de arquivos na parte superior para alternar para um arquivo `.lum` diferente.

---

## Salvamento automático

Lumi salva um instantâneo de fundo do seu trabalho em intervalos regulares durante a edição. Os salvamentos automáticos são gravados em um **diretório de cache separado**, deixando o arquivo `.lum` funcional intacto:

```
~/.cache/lumi/autosave/~home~user~projects~my-painting.lum/
```

A codificação do caminho usa `~` como separador para criar um diretório de cache exclusivo por arquivo. Isso significa que os salvamentos automáticos estão disponíveis mesmo se o arquivo do projeto for perdido ou corrompido.

- **Frequência**: configurável em **Editar** → **Preferências** → **Desempenho** → Intervalo de salvamento automático.
- **Local de armazenamento**: Também definido em Preferências → Desempenho.
- **Objetivo**: Recuperação de falhas. A guia Salvamento automático na caixa de diálogo Recuperar imagem mostra os estados de salvamento automático disponíveis com carimbos de data e hora.

Quando você abre um arquivo que contém dados de salvamento automático mais recentes, o Lumi notifica você no momento da abertura.

---

## Salvamentos incrementais

O salvamento incremental é um sistema de ponto de verificação manual armazenado **dentro do arquivo do projeto** em `recovery/`. A estrutura é:

```
my-painting.lum/recovery/
  └── primary-01.lum/       (full baseline, created on first Ctrl+S)
      ├── delta-0001.lum/   (Ctrl+S checkpoint, only modified buffers)
      ├── delta-0002.lum/
      └── ...
```

Uma nova linha de base `primary-NN.lum/` é escrita após **Arquivo → Salvar**. Os pressionamentos subsequentes de Ctrl+S criam `delta-NNNN.lum/` subdiretórios contendo apenas os buffers que foram alterados desde a última linha de base. Deltas de salvamento automático e deltas de salvamento manual usam contadores separados para que não interfiram no histórico um do outro.

Os salvamentos incrementais estão **desabilitados por padrão** e devem ser habilitados por projeto:

1. **Arquivo** → **Salvar como** (Shift+Ctrl+S).
2. Na caixa de diálogo Salvar como, marque **Economia incremental** e, opcionalmente, defina um limite de **Economia máxima**.
3. A configuração é armazenada com o projeto e se aplica a todos os pressionamentos subsequentes de Ctrl+S.

Quando você abre um arquivo `.lum` que possui salvamentos incrementais mais recentes do que o salvamento primário, o Lumi mostra um prompt **Salvamento incremental detectado** oferecendo para carregar o ponto de verificação mais recente.

---

## Caixa de diálogo Recuperar imagem

A caixa de diálogo possui três guias e dois botões de ação.

### Guia de salvamento automático

Lista todos os estados de salvamento automático disponíveis para o arquivo selecionado com carimbos de data/hora e miniaturas (quando disponíveis). Selecione um estado e clique em **Recuperar** para abri-lo.

Use esta guia para:
- Recupere após um acidente.
- Reverter para um estado anterior da mesma sessão.

### Guia Incremental

Lista todos os estados dos pontos de verificação armazenados no arquivo do projeto. Cada entrada mostra o carimbo de data/hora do ponto de verificação. Selecione um ponto de verificação e clique em **Recuperar** para abri-lo.

Use esta guia para:
- Regressar a um ponto anterior de uma sessão sem ter guardado ficheiros separados.
- Navegue pelo histórico de versões de um projeto.

### Última guia

A guia padrão quando a caixa de diálogo é aberta. Identifica automaticamente o estado de recuperação mais recente disponível em salvamentos automáticos e pontos de verificação incrementais e mostra seu carimbo de data/hora. Clique em **Recuperar** para carregá-lo imediatamente, sem navegar pelos estados individuais.

---

## Botões

| Botão | Ação |
|--------|--------|
| **Recuperar** | Abre o estado de recuperação selecionado como uma nova imagem. |
| **Fechar** | Ignora o diálogo sem se recuperar. |
| **Limpar estados antigos…** | Abre um prompt de limpeza (veja abaixo). |

---

## Limpar estados antigosO acúmulo de estados de recuperação ao longo do tempo pode consumir espaço em disco significativo. O botão **Limpar estados antigos…** (canto inferior esquerdo da caixa de diálogo) abre um prompt de limpeza para a guia ativa (Salvamento automático ou Incremental).

O prompt mostra:
- Quantos salvamentos completos existem para o arquivo.
- O espaço total em disco que ocupam.
- Um botão giratório **Manter mais recente** para selecionar quantos salvamentos serão mantidos.

Definir **Keep most recent** como `0` exclui todos os estados de recuperação. O próximo Ctrl+S após uma limpeza completa gravará um novo salvamento primário.

---

## Recuperação de inicialização

Na inicialização, se o Lumi detectar que o arquivo aberto mais recentemente contém dados de salvamento automático mais recentes do que o último salvamento completo, ele apresentará um prompt de recuperação antes de carregar. Você pode aceitar (carregar o salvamento automático) ou descartar (abrir o salvamento principal normalmente).