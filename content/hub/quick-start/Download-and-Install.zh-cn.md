---
title: "下载并安装"
type: docs
url: "hub/quick-start/Download-and-Install"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 5f17d7e9009aeeacf256152bef94386ccc5a8eea87cf0feebef073488fb59283
---
如果您已经使用 Linux 并且想要快速运行 Lumi，请使用 GitLab 工件中最新的 **开发 AppImage**：

https://gitlab.gnome.org/pixelmixer/lumi-dev/-/artifacts

1. 下载最新的开发版 AppImage 构件 zip。
2. 解压 zip 文件。
3. 双击`Lumi*.AppImage` 文件运行它。

AppImage 应该已经可以运行。如果不是，请在文件的权限中启用**允许将文件作为程序执行**，或使用下面的终端方法。

```bash
chmod +x Lumi*.AppImage
./Lumi*.AppImage
```