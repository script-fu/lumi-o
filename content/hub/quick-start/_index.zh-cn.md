---
title: "快速入门"
type: docs
---
Lumi 尚未发布，它作为开发版本提供。

如果您已经使用 Linux 并且想要快速运行 Lumi，请使用 GitLab 工件中最新的 **开发 AppImage**：

https://gitlab.gnome.org/pixelmixer/lumi-dev/-/artifacts

1.下载最新开发AppImage神器zip。
2. 拉开拉链。
3. 双击`Lumi*.AppImage` 文件运行它。

AppImage 应该已经可以运行。如果不是，请在文件的权限中启用**允许将文件作为程序执行**，或使用下面的终端方法。

```bash
chmod +x Lumi*.AppImage
./Lumi*.AppImage
```

## Linux 上的 Wacom 设置

对于 Lumi 中的数字绘画，简单的**线性压力设置**通常是最好的：

- 保持平板电脑驱动器压力曲线线性。
- 保持 Lumi 中的压力/输入曲线呈线性。
- 使用画笔本身塑造感觉，因为画笔动力学可能是非线性的。

您可以使用以下命令检查并重置 Linux 驱动程序曲线：

```bash
xsetwacom --get "Wacom Intuos Pro L Pen stylus" PressureCurve
xsetwacom --set "Wacom Intuos Pro L Pen stylus" PressureCurve 0 0 100 100
```

实用技巧：

- Lumi 目前会阻止有问题的 Wacom 板/触摸环输入，以避免 X11 故障。将平板电脑按钮映射到**相对**画笔大小向上/向下。
- 如果使用 `Alt` 进行画笔大小拖动不起作用，则您的桌面可能正在使用 `Alt` 来移动窗口。将该窗口管理器快捷方式更改为 `Super` 或禁用它。

如果您想使用源代码，请转到[Technical Guides](/hub/technical-guides/) 和[Installation](/hub/technical-guides/Installation/)。