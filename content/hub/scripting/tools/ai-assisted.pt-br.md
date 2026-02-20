---
title: "Desenvolvimento Assistido por IA"
type: docs
---
As ferramentas modernas de IA podem acelerar significativamente o desenvolvimento do plug-in Lumi, agindo como um parceiro de codificação colaborativa.

## Código VS no modo Agente

Usar o Visual Studio Code com um assistente de IA no **modo Agente** (como o modo Agente do GitHub Copilot ou outros assistentes habilitados para ferramentas) permite que você execute tarefas complexas de várias etapas usando linguagem natural.

Em vez de apenas completar uma única linha de código, um Agente pode:
- Leia todo o seu espaço de trabalho para entender o contexto.
- Crie novos arquivos e diretórios.
- Execute comandos de terminal para testar ou validar scripts.
- Pesquise padrões existentes em sua base de código.

## Acesso ao repositório

A assistência de IA é mais eficaz quando o Agente tem acesso ao **lumi-dev** ou ao repositório específico do seu projeto. Com visibilidade da base de código existente, o Agente pode:
- Use **[Utility Libraries](@@LUMI_TOKEN_4@@)** como referência para funções auxiliares.
- Siga os padrões existentes para operações GEGL e gerenciamento de camadas.
- Reutilize código padrão de plug-ins estabelecidos.

## Exemplo de fluxo de trabalho

Você pode pedir diretamente ao Agente para gerar um plug-in completo descrevendo o resultado funcional desejado:

> "Usando os utilitários de esquema disponíveis e exemplos na área de trabalho, escreva um novo plug-in que crie uma guia horizontal de 50% na imagem ativa e o nomeie 'Guia Central'."

O Agente procurará como criar guias, identificar a função de utilidade correta (como `lumi-image-add-hguide-percent` de `common.scm`) e gerar o arquivo `.scm` completo com o padrão de registro correto.

## Melhores práticas

- **Seja específico**: descreva exatamente o que você deseja que o plug-in faça.
- **Utilitários de referência**: incentive o agente a consultar o diretório `share/lumi/scripts/` para ajudantes de alto nível.
- **Revisar e testar**: sempre teste o plug-in gerado pela IA, geralmente é um processo iterativo e criativo.