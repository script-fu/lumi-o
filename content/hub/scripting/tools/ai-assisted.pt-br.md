---
title: "Desenvolvimento Assistido por IA"
type: docs
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 7867639dd5e951131133f23635a10898d35de3c275f48b78b7ed7091c73e15c4
url: "hub/scripting/tools/ai-assisted"
---
As ferramentas modernas de IA podem acelerar significativamente o desenvolvimento do plug-in Lumi, agindo como um parceiro de codificação colaborativa.

## VS Code no modo Agent

Usar o Visual Studio Code com um assistente de IA no **modo Agente** (como o modo Agente do GitHub Copilot ou outros assistentes habilitados para ferramentas) permite que você execute tarefas complexas de várias etapas usando linguagem natural.

Em vez de apenas completar uma única linha de código, um Agente pode:
- ler todo o workspace para entender o contexto
- criar novos arquivos e diretórios
- executar comandos de terminal para testar ou validar scripts
- buscar padrões existentes na base de código

## Acesso ao repositório

A assistência de IA é mais eficaz quando o Agente tem acesso ao **lumi-dev** ou ao repositório específico do seu projeto. Com visibilidade da base de código existente, o Agente pode:
- usar as **[Bibliotecas de utilitários]({{< ref "/hub/scripting/reference/utility-browser" >}})** como referência para funções auxiliares
- seguir padrões existentes para operações GEGL e gerenciamento de camadas
- reutilizar boilerplate de plug-ins estabelecidos

## Exemplo de fluxo de trabalho

Você pode pedir diretamente ao Agente para gerar um plug-in completo descrevendo o resultado funcional desejado:

> "Usando os utilitários de Scheme disponíveis e exemplos na área de trabalho, escreva um novo plug-in que crie uma guia horizontal de 50% na imagem ativa e o nomeie 'Guia Central'."

O Agente procurará como criar guias, identificar a função de utilidade correta (como `lumi-image-add-hguide-percent` de `common.scm`) e gerar o arquivo `.scm` completo com o padrão de registro correto.

## Melhores práticas

- **Seja específico**: descreva exatamente o que você deseja que o plug-in faça.
- **Utilitários de referência**: incentive o agente a consultar o diretório `share/lumi/scripts/` para ajudantes de alto nível.
- **Revisar e testar**: sempre teste o plug-in gerado pela IA, geralmente é um processo iterativo e criativo.
