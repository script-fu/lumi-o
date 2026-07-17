---
title: "Phát triển được hỗ trợ bởi AI"
type: docs
translation_provenance: ai-reviewed
translation_source_sha256: 7867639dd5e951131133f23635a10898d35de3c275f48b78b7ed7091c73e15c4
url: "hub/scripting/tools/ai-assisted"
translation_lock: true
---
Các công cụ AI hiện đại có thể tăng tốc đáng kể việc phát triển plug-in Lumi bằng cách đóng vai trò là đối tác mã hóa hợp tác.

## Mã VS trong Chế độ Đại lý

Việc sử dụng Visual Studio Code với trợ lý AI trong **Chế độ tác nhân** (chẳng hạn như chế độ Tác nhân của GitHub Copilot hoặc các trợ lý hỗ trợ công cụ khác) cho phép bạn thực hiện các tác vụ phức tạp, gồm nhiều bước bằng ngôn ngữ tự nhiên.

Thay vì chỉ hoàn thành một dòng mã, Đại lý có thể:
- Đọc toàn bộ không gian làm việc của bạn để hiểu ngữ cảnh.
- Tạo tập tin và thư mục mới.
- Chạy các lệnh đầu cuối để kiểm tra hoặc xác thực các tập lệnh.
- Tìm kiếm các mẫu hiện có trong cơ sở mã của bạn.

## Truy cập kho lưu trữ

Hỗ trợ AI hiệu quả nhất khi Đại lý có quyền truy cập vào **lumi-dev** hoặc kho lưu trữ dự án cụ thể của bạn. Với khả năng hiển thị trong cơ sở mã hiện có, Đại lý có thể:
- Sử dụng **[Thư viện tiện ích]({{< ref "/hub/scripting/reference/utility-browser" >}})** làm tài liệu tham khảo cho các hàm trợ giúp.
- Thực hiện theo các mẫu hiện có cho hoạt động GEGL và quản lý lớp.
- Tái sử dụng mã soạn sẵn từ các plug-in đã thiết lập.

## Ví dụ về quy trình làm việc

Bạn có thể trực tiếp yêu cầu Tác nhân tạo một plug-in đầy đủ bằng cách mô tả kết quả chức năng mong muốn:

> "Sử dụng các tiện ích và ví dụ Scheme có sẵn trong không gian làm việc, viết một plug-in mới tạo hướng dẫn ngang 50% trên hình ảnh đang hoạt động và đặt tên là 'Hướng dẫn Trung tâm'."

Tác nhân sẽ tìm kiếm cách tạo hướng dẫn, xác định chức năng tiện ích chính xác (như `lumi-image-add-hguide-percent` từ `common.scm`) và tạo tệp `.scm` hoàn chỉnh với bản mẫu đăng ký chính xác.

## Các phương pháp hay nhất

- **Hãy cụ thể**: Mô tả chính xác những gì bạn muốn plugin thực hiện.
- **Tiện ích tham khảo**: Khuyến khích Đại lý xem thư mục `share/lumi/scripts/` dành cho người trợ giúp cấp cao.
- **Đánh giá và kiểm tra**: Luôn kiểm tra plugin do AI tạo ra, đây thường là một quá trình lặp đi lặp lại và sáng tạo.