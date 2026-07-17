---
title: "AI輔助開發"
type: docs
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 7867639dd5e951131133f23635a10898d35de3c275f48b78b7ed7091c73e15c4
url: "hub/scripting/tools/ai-assisted"
---
現代人工智慧工具可以透過充當協作編碼合作夥伴來顯著加速 Lumi 插件的開發。

## VS Code 的 Agent 模式

在 **代理模式** 下將 Visual Studio Code 與 AI 助手（例如 GitHub Copilot 的代理模式或其他支援工具的助手）結合使用，您可以使用自然語言執行複雜的多步驟任務。

代理不僅可以完成一行程式碼，還可以：
- 讀取整個工作區以了解脈絡
- 建立新檔案與目錄
- 執行終端命令以測試或驗證指令碼
- 在程式碼庫中搜尋現有模式

## 儲存庫訪問

當代理有權存取 **lumi-dev** 或您的特定專案儲存庫時，AI 協助最為有效。透過了解現有程式碼庫，代理可以：
- 使用 **[實用工具程式庫]({{< ref "/hub/scripting/reference/utility-browser" >}})** 作為輔助函數的參考
- 遵循 GEGL 操作與圖層管理的現有模式
- 重用既有外掛中的樣板程式碼

## 工作流程範例

您可以透過描述所需的功能結果直接要求 Agent 產生完整的插件：

>“使用工作區中可用的 Scheme 實用程式和範例，編寫一個新外掛，在活動圖像上建立 50% 的水平參考線，並將其命名為 'Center Guide'。”

代理程式將搜尋如何建立指南，識別正確的實用函數（例如`common.scm`中的`lumi-image-add-hguide-percent`），並產生具有正確註冊樣板的完整`.scm`檔案。

## 最佳實踐

- **具體**：準確描述您希望外掛程式執行的操作。
- **參考實用程式**：鼓勵代理商查看 `share/lumi/scripts/` 目錄以取得進階幫助程式。
- **審查和測試**：始終測試人工智慧生成的插件，這通常是一個迭代和創造性的過程。
