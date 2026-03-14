---
title: "Navegador de plug-ins"
type: docs
---
O Plug-In Browser permite explorar o sistema de menus e ver onde plug-ins específicos estão instalados.

## Abrindo o navegador de plug-ins

Vá para **Ajuda → Programação → Navegador de plug-ins**.

## O que mostra

Enquanto o Navegador de Procedimento se concentra nas *funções* brutas do PDB, o Navegador de Plug-In é uma visualização de subconjunto focada na descoberta da interface do usuário. Ele filtra especificamente o PDB para mostrar "coisas que parecem plug-ins instalados em menus".

Internamente, utiliza uma consulta que retorna apenas procedimentos que possuem um arquivo associado em disco e um caminho de menu registrado.

- **Árvore de menus**: Mostra uma representação em árvore da estrutura de menus do Lumi.
- **Locais de plug-ins**: ajuda você a descobrir onde um plug-in recém-instalado se aninhou nos menus.
- **Metadados**: Mostra informações sobre autor, versão e data do plug-in.

## Uso

Use o Navegador de plug-ins quando souber que existe um recurso, mas não conseguir encontrá-lo nos menus, ou quando estiver projetando seu próprio plug-in e quiser ver onde ferramentas semelhantes estão localizadas.