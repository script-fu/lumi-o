---
title: "Định dạng tệp (.lum)"
type: docs
url: "hub/features/file-format"
translation_provenance: ai-reviewed
translation_source_sha256: f26bd5ecb0cb647cd3180b1ab39402ee6943085b7ad899518a406ee6ae98c4c9
translation_lock: true
---

Định dạng gốc của Lumi dành cho dự án vẽ nhiều lớp cần đáng tin cậy, có thể kiểm tra và khôi phục theo thời gian. Nó được thiết kế quanh thực tế minh họa: nhiều lớp, canvas lớn, thông tin màu nhúng, mặt nạ, hiệu ứng và dữ liệu khôi phục.

Thay vì coi dự án là một khối mờ duy nhất, định dạng giữ cấu trúc tác phẩm hiển thị với ứng dụng. Điều này giúp Lumi lưu, tải và khôi phục hình lớn thông minh hơn trong khi giữ tổ chức mà nghệ sĩ phụ thuộc.

## Cấu trúc dự án mở

Dự án Lumi tách các phần tác phẩm: cấu trúc hình ảnh, nội dung lớp, mặt nạ, dữ liệu màu, metadata và thông tin khôi phục — mỗi phần có vai trò rõ ràng. Điều này giúp định dạng dễ lý giải và phù hợp truy cập lâu dài hơn vùng chứa đóng, nguyên khối.

Mục tiêu không chỉ lưu pixel mà lưu trạng thái làm việc của minh họa. Lớp vẫn là lớp, mặt nạ vẫn là mặt nạ, và tệp phản ánh cách tác phẩm được xây dựng.

## Thiết kế cho tranh lớn

Hình nhiều lớp lớn nhanh chóng trở nên nặng. Định dạng Lumi hỗ trợ quy trình mà không phải mọi phần dữ liệu hình ảnh đều cần vào bộ nhớ cùng lúc. Dự án có thể phản hồi nhanh bằng cách tải phần hình ảnh thực sự cần để xem, chỉnh sửa, tổng hợp hoặc xuất.

Cách tiếp cận này giúp tệp phức tạp dễ quản lý, đặc biệt khi tác phẩm có nhiều lớp ẩn, lưu trữ, thử nghiệm hoặc được nhóm.

## Lưu mà không gián đoạn dòng công việc

Định dạng hỗ trợ cả lưu dự án thông thường và ảnh chụp khôi phục nhẹ. Nghệ sĩ có thể bảo vệ tác phẩm thường xuyên mà không biến mọi điểm kiểm tra thành bản sao đầy đủ toàn bộ hình.

Vì thông tin khôi phục thuộc cấu trúc dự án, Lumi giữ lịch sử hữu ích gần tác phẩm trong khi vẫn cho phép lưu an toàn tự động tách khỏi tệp đang làm việc.

## Trao đổi và xuất

Định dạng gốc dành cho công việc Lumi đang tiến hành; định dạng xuất dùng để chia sẻ kết quả phẳng hoặc tập trung tương thích. Hỗ trợ nhập đưa tác phẩm hiện có vào môi trường nhiều lớp của Lumi; hỗ trợ xuất cho phép tác phẩm hoàn thiện rời định dạng dự án khi sẵn sàng xuất bản, giao hàng hoặc xử lý thêm.

Sự phân biệt này giữ tệp làm việc phong phú, có thể chỉnh sửa, đồng thời cho phép tạo hình cuối ở định dạng bên ngoài phổ biến.

## Độ tin cậy lâu dài

Tóm lại, định dạng `.lum` là vùng chứa thực tế cho công việc vẽ nghiêm túc: đủ mở để kiểm tra, đủ cấu trúc để khôi phục và đủ linh hoạt để xử lý hình nhiều lớp phức tạp một cách tiết kiệm.
