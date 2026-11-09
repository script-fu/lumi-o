---
title: "OpenGL"
type: docs
url: "hub/features/opengl"
translation_provenance: ai-reviewed
translation_source_sha256: af2a57a988eced14801b7c4673253fe79e9a6b6ab4fcf53a075b62d4a9035351
translation_lock: true
---

Lumi có thể vẽ chồng lớp bằng OpenGL, và vẽ nét cọ theo cách đó. Việc di chuyển trên canvas vẫn mượt. Airbrush lớn và stamp kết cấu lớn nhanh hơn nhiều. Một công cụ có vẽ theo cách này hay không được lưu cùng cài đặt sẵn của nó.

## Chồng lớp

Cửa sổ hình ảnh hiện chồng lớp đã tổng hợp. Với OpenGL, hình đó được giữ thành các tile trên bộ xử lý đồ họa, đã chuẩn bị sẵn cho màn hình. Kéo khung, thu phóng, xoay và lật đổi góc nhìn của những tile đó. Trên canvas lớn, cử chỉ vẫn mượt, vì di chuyển chỉ là đổi góc nhìn trên một hình đã có sẵn.

Các chỉnh sửa cập nhật những tile mà chúng chạm. Đường dẫn hướng, con trỏ cọ và viền vùng chọn được vẽ lên trên hình đó.

Khi bút còn hạ, một nét dùng OpenGL có thể hiện đúng chỗ giữa các lớp xung quanh, kể cả lớp phía trên lớp đang vẽ. Dấu được ghi vào lớp khi nhấc bút.

## Nét theo từng cài đặt sẵn

Vẽ bằng OpenGL là một lựa chọn trên cài đặt sẵn công cụ, cạnh cọ và dynamics. Một cài đặt sẵn có thể vẽ trên bộ xử lý đồ họa. Cài đặt khác giữ đường nét thường. Khi trở lại một công cụ, lựa chọn đó trở về cùng nó.

Lợi thấy rõ nhất ở chỗ stamp lớn. Trên đường thường, airbrush rộng tụt lại sau bút khi dấu lớn lên: các dấu chấm xếp hàng, và nét phát sinh khoảng trống, kể cả khi bút vẫn giữ và cọ tiếp tục phun. Với OpenGL, airbrush đó vẫn đặt dấu cùng tay.

Stamp kết cấu lớn cho thấy cùng một thay đổi. Đường thường tốn lâu hơn trên mỗi dấu chấm khi stamp lớn, đến khi nét khựng. Nét OpenGL ở lại với cử chỉ. Stamp mang hạt bề mặt cũng vẽ bằng OpenGL được.

Dấu lông cũng vẽ theo cách này. Một stamp lông đầu tròn có thể vẽ bằng OpenGL trong khi lông đổi dọc nét. Nét thư pháp có thể vẽ thành một dải liên tục, và kết cấu bề mặt có thể che dải đó. [Kết cấu bề mặt](/hub/features/paintbrush/#kết-cấu-bề-mặt) và [lông theo nét vẽ](/hub/features/paintbrush/#lông-theo-nét-vẽ) thuộc về chính cọ.

Một số nét ở lại trên đường thường. Pha trộn quang phổ, một bộ lọc trên lớp đang vẽ, và một số chế độ hòa trộn nằm trong số đó. Những dấu ấy vẫn được vẽ.

## Khi canvas thường còn lại

OpenGL được dùng khi phần cứng đồ họa chạy được và hình vừa bộ nhớ dành cho canvas. Nếu không, Lumi vẽ cửa sổ như trước. AppImage đã phát hành mang thư viện đồ họa riêng. Trên driver mà các thư viện đó không dùng được, kể cả driver độc quyền của NVIDIA, canvas ở lại trên đường thường đó.

## Cinnamon trên X11 và GNOME trên Wayland

Lumi được phát triển trên Debian với Cinnamon và X11. Đó vẫn là môi trường phát triển. GNOME trên Wayland đã được thử cho diện mạo chung của ứng dụng và cho kết xuất OpenGL này. Việc di chuyển trên canvas ở đó cũng mượt.
