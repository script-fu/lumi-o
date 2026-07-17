---
title: "AI辅助开发"
type: docs
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 7867639dd5e951131133f23635a10898d35de3c275f48b78b7ed7091c73e15c4
---
现代人工智能工具可以通过充当协作编码合作伙伴来显着加速 Lumi 插件的开发。

## VS Code 的 Agent 模式

在 **代理模式** 下将 Visual Studio Code 与 AI 助手（例如 GitHub Copilot 的代理模式或其他支持工具的助手）结合使用，您可以使用自然语言执行复杂的多步骤任务。

代理不仅可以完成一行代码，还可以：
- 读取整个工作区以了解上下文
- 创建新文件和目录
- 运行终端命令以测试或验证脚本
- 在代码库中搜索现有模式

## 存储库访问

当代理有权访问 **lumi-dev** 或您的特定项目存储库时，AI 协助最为有效。通过了解现有代码库，代理可以：
- 使用 **[实用工具库]({{< ref "/hub/scripting/reference/utility-browser" >}})** 作为辅助函数的参考
- 遵循 GEGL 操作与图层管理的现有模式
- 复用既有插件中的样板代码

## 工作流程示例

您可以通过描述所需的功能结果直接要求 Agent 生成完整的插件：

>“使用工作区中可用的 Scheme 实用程序和示例，编写一个新插件，在活动图像上创建 50% 的水平参考线，并将其命名为 'Center Guide'。”

代理将搜索如何创建指南，识别正确的实用函数（例如`common.scm`中的`lumi-image-add-hguide-percent`），并生成具有正确注册样板的完整`.scm`文件。

## 最佳实践

- **具体**：准确描述您希望插件执行的操作。
- **参考实用程序**：鼓励代理查看 `share/lumi/scripts/` 目录以获取高级帮助程序。
- **审查和测试**：始终测试人工智能生成的插件，这通常是一个迭代和创造性的过程。
