---
title: "Vẽ tranh kỹ thuật số mã nguồn mở cho Linux"
toc: false
type: docs
url: ""
translation_provenance: ai-reviewed
translation_source_sha256: 4f453bf4100140e9a2779d4b9806a0ef8f27e68af8a9b6f680d01a6a5d2f2341
translation_lock: true
---

[![gui-01](/images/screens/gui-00.jpg)](/images/screens/gui-01.jpg)

Lumi-o được thiết kế cho vẽ raster nhiều lớp — gọn gàng và ổn định một cách có chủ ý. Hỗ trợ bảng vẽ Wacom mang lại đầu vào mượt mà, phản hồi nhanh như minh họa chuyên nghiệp yêu cầu.

Hệ thống màu dựa trên bảng màu của Lumi pha trộn hồ sơ sắc tố thực tế theo phương pháp quang phổ. Xây dựng bảng màu quanh các mối quan hệ màu được trực quan hóa, kiểm soát giá trị tông màu và sắp xếp màu thành các mục có tên. Palette Map và Palette Mixer giúp các quyết định màu nhất quán trong suốt một bức tranh hoặc dự án.

Xây dựng trên kiến trúc lõi của GIMP, Lumi-o được thiết kế và tinh chỉnh bởi một nghệ sĩ. Công cụ vẫn dễ tiếp cận trong khi cung cấp khả năng tùy chỉnh cần thiết cho quy trình nâng cao. Tải và lưu nhanh, cùng hệ thống tự động lưu tích hợp đầy đủ, bảo vệ tác phẩm mà không làm gián đoạn dòng công việc.

Nét cọ có thể nhận kết cấu bề mặt với hạt và độ nhám, kể cả nét thư pháp. Nét mới, hoặc một chỗ ngoặt trong nét, có thể dịch hạt đó, nên khi tô bóng qua lại mỗi lần đổi chiều đều lấy một pha mới. Cọ lông cứng định hình được rất chi tiết, và lông có thể dịch chuyển rồi mọc lại dọc nét. OpenGL vẽ chồng lớp trong cửa sổ hình ảnh và có thể vẽ nét cho từng cài đặt sẵn công cụ. Airbrush lớn và stamp kết cấu lớn nhanh hơn nhiều, và việc di chuyển trên canvas vẫn mượt.

Tải trì hoãn cho phép Lumi mở các dự án lớn, phức tạp với ít RAM hơn nhiều. Khi một nhóm được thu gọn, Lumi tải ảnh composite đã lưu của nhóm đó, trong khi các lớp, mặt nạ và nhóm lồng nhau vẫn là các placeholder nhẹ cho đến khi được mở rộng.

Lumi-o được phát triển trên Debian 13 với Cinnamon trên X11. GNOME trên Wayland đã được thử cho diện mạo chung của ứng dụng và cho kết xuất OpenGL. Nếu bạn đến từ Windows, cài đặt Debian là bước đầu tiên. Xem [Cách cài đặt Linux](/hub/install-linux/).

{{< cards >}}
  {{< card link="hub/about/" title="Giới thiệu" icon="information-circle" >}}
  {{< card link="hub/quick-start/" title="Bắt đầu nhanh" icon="play" >}}
  {{< card link="hub/features/" title="Tính năng" icon="book-open" >}}
{{< /cards >}}
