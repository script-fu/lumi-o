---
title: "Início rápido"
type: docs
---
O Lumi ainda não foi lançado, está disponível em versão de desenvolvimento.

Se você já usa Linux e deseja executar o Lumi rapidamente, use o **AppImage de desenvolvimento** mais recente dos artefatos do GitLab:

https://gitlab.gnome.org/pixelmixer/lumi-dev/-/artifacts

1. Baixe o zip do artefato AppImage de desenvolvimento mais recente.
2. Extraia o zip.
3. Clique duas vezes no arquivo `Lumi*.AppImage` para executá-lo.

O AppImage já deve estar executável. Caso contrário, habilite **Permitir execução de arquivo como programa** nas permissões do arquivo ou use o método de terminal abaixo.

```bash
chmod +x Lumi*.AppImage
./Lumi*.AppImage
```

## Configuração Wacom no Linux

Para pintura digital no Lumi, uma simples **configuração de pressão linear** geralmente é melhor:

- Mantenha a curva de pressão do driver do tablet linear.
- Manter curvas de pressão/entrada em Lumi lineares.
- Molde a sensação com o próprio pincel, pois a dinâmica do pincel pode ser não linear.

Você pode verificar e redefinir a curva do driver Linux com:

```bash
xsetwacom --get "Wacom Intuos Pro L Pen stylus" PressureCurve
xsetwacom --set "Wacom Intuos Pro L Pen stylus" PressureCurve 0 0 100 100
```

Dicas práticas:

- Lumi atualmente bloqueia a entrada problemática do pad/touch-ring Wacom para evitar falhas no X11. Em vez disso, mapeie os botões do tablet para o tamanho do pincel **relativo** para cima/para baixo.
- Se arrastar o tamanho do pincel com `Alt` não funcionar, sua área de trabalho pode estar usando `Alt` para mover janelas. Altere o atalho do gerenciador de janelas para `Super` ou desative-o.

Se você quiser trabalhar a partir do código-fonte, vá para [Technical Guides](/hub/technical-guides/) e [Installation](/hub/technical-guides/Installation/).