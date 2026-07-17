---
title: "Quy trình hàng loạt"
type: docs
translation_provenance: ai-reviewed
translation_source_sha256: 141b5ee23e77ecfc8ef5e8112706cfaebc1a2a528518218296dffda5b9dee6d1
url: "hub/scripting/plug-ins/batch-process"
translation_lock: true
---
Một ví dụ thực tế, toàn diện để xử lý nhiều tệp trong một lần.

## Nơi nó sống

- [Xem nguồn](https://gitlab.gnome.org/pixelmixer/lumi-dev/-/blob/main/plug-ins/lumi/batch-process/batch-process.scm)

## Nơi nó xuất hiện trong Lumi

- **Tệp → Xử lý hàng loạt**

## Nó thể hiện điều gì

- Tham số `SF-DIRNAME` cho thư mục nguồn/đích
- Xác thực đường dẫn GUI có dự phòng (`validate-path-and-dir`)
- Quét và lặp lại thư mục đệ quy
- Báo cáo tiến độ cho các hoạt động dài hạn