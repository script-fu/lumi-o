---
title: "Lumi-o"
type: docs
url: "hub/about/lumi-o"
weight: 1
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 1bf50df22bdb2af7931727f82bc3c90eee5242be66847535aec8e41c47087e53
---
Lumi là ứng dụng nhanh, hiệu quả, chỉ dành cho Linux để tạo ảnh raster, được phát triển công khai. Quyết định thiết kế, tài liệu kiến trúc và lịch sử phát triển đều công khai để người dùng hiểu cách nó tiến hóa.

Lumi thiên về vẽ tranh và minh họa kỹ thuật số, đồng thời vẫn có thể chỉnh sửa ảnh có cấu trúc và hiệu chỉnh ảnh chụp. Các điều chỉnh hiệu chỉnh, hiệu ứng kiểu ống kính, phân cấp màu theo tông và lớp bộ lọc không phá hủy hỗ trợ nhiều quy trình chỉnh sửa.

Lumi được xây dựng quanh ý tưởng rằng phần mềm vẽ kỹ thuật số nên hoạt động như một công cụ studio đáng tin cậy: dự đoán được, minh bạch và tập trung vào việc tạo ra hình ảnh.

## Mục đích

Lumi hỗ trợ phương pháp tạo ảnh có cấu trúc, không phá hủy, dù nguồn là tranh vẽ, hình vẽ hay ảnh chụp. Đây là lựa chọn thay thế tập trung và rõ ràng so với cả trình chỉnh sửa ảnh đa dụng lẫn phần mềm vẽ chuyên dụng: miễn phí và mã nguồn mở, không đăng ký, không bị khóa hệ sinh thái, không phụ thuộc đám mây hay tạo ảnh bằng AI.

Độ tin cậy và khả năng truy cập lâu dài là tính năng cốt lõi. Định dạng tệp mở dựa trên thư mục vẫn đọc được mà không cần phần mềm độc quyền, với nhập và xuất XCF và PSD.

## Nền tảng nghệ thuật

Lumi do một nghệ sĩ độc lập phát triển, có kinh nghiệm về pixel art, vẽ và hội họa truyền thống, phát triển game, technical art, minh họa và hoạt hình 3D. Nền tảng đó định hình cách tiếp cận màu sắc, nét vẽ, lớp, hiệu năng, phục hồi dữ liệu, scripting và trải nghiệm người dùng.

## Triết lý

Lumi kết hợp hệ thống màu dựa trên sắc tố với quy trình làm việc theo lớp, phản hồi nhanh và không phá hủy. Hệ thống màu cố ý khác biệt so với thanh trượt HSV thông thường và bộ chọn RGB tùy ý.

- **Màu lấy sắc tố làm trung tâm**: Hồ sơ sắc tố thực (mã Colour Index) được trộn theo phổ để bảng màu hoạt động giống sơn thật hơn.
- **Quy trình theo bảng màu**: Các bảng màu đã lưu, có thể chuyển đổi, sắp xếp sắc tố, pha màu, dải giá trị và gradient, giữ quyết định màu nhất quán trong một bức tranh hoặc dự án.
- **Công cụ xúc giác, tập trung**: Cọ tích hợp lực nặng, góc nghiêng và tốc độ bút để điều khiển trực tiếp, tinh tế; các điều khiển hỗ trợ quyết định có chủ đích mà không phức tạp thừa.
- **Độ tin cậy không phá hủy**: Lớp và bộ lọc có thể chỉnh sửa mở rộng cho dự án phức tạp nhưng vẫn dự đoán được. Tự động lưu, lưu nhanh, lưu tăng dần và phục hồi bảo vệ các phiên vẽ dài và dự án lớn.
- **Workspace trực tiếp**: Hồ sơ đặt tên giữ dock, công cụ, preset, bảng màu và liên kết thiết bị, rồi chuyển đổi chúng nguyên tử khi chạy.
- **Scripting Scheme**: Lumi mở rộng truyền thống Script-Fu bằng ngôn ngữ plug-in dựa trên Scheme và các hàm tiện ích bổ sung để xây dựng plug-in và tự động hóa quy trình.

[Bộ lọc](/hub/features/filters/), gồm làm mờ ống kính bokeh có hình dạng, tilt shift, phân cấp tông màu, làm sắc nét và giảm nhiễu, có thể vẫn chỉnh sửa được cùng lúc với công việc cọ vẽ.

## Ranh giới

- **Tập trung, không bao quát**: Lumi không nhắm tới thiết kế web, xuất bản desktop hay mọi ngách mà một trình biên tập rộng như GIMP cố gắng phủ.
- **Chỉ Linux**: Lumi được tối ưu riêng cho Linux và không hỗ trợ Windows hay macOS.

## Lời cảm ơn

Lumi được xây dựng trên nền GNU Image Manipulation Program (GIMP). Lumi ghi nhận và biết ơn sâu sắc nhiều năm lao động của các nhà phát triển, nghệ sĩ và cộng tác viên.

![Lumi logo placeholder](/images/lumi.png)
