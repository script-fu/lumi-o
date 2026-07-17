---
title: "Công cụ cọ"
type: docs
url: "hub/features/paintbrush"
translation_provenance: ai-reviewed
translation_source_sha256: a37df7a3325c5a6028907f9584d45fd23746dd345b2d649f0a3ff5c1e03ed657
translation_lock: true
---

Công cụ cọ là công cụ vẽ cốt lõi của Lumi: cách đáp ứng, biểu cảm để vẽ, tô màu, tạo bóng, thêm kết cấu và đặt nét trực tiếp trên canvas. Thiết kế mang lại cảm giác tức thì trong khi vẫn cho nghệ sĩ không gian định hình hành vi nét vẽ.

Thay vì một cọ cố định, nó hoạt động như hệ thống vẽ. Hình dạng, kết cấu, chuyển động, áp lực, thời gian và màu cọ đều góp phần tạo nét cuối — phù hợp nét sạch, tô mềm, hiệu ứng media khô, nét thư pháp, kết cấu rải rác và đội hình cọ nhiều đầu.

![brush-tool](/images/screens/brush-tool.jpg)

## Nét cọ biểu cảm

Cọ có thể dựa trên stamp bitmap, hình dạng procedural hoặc nguồn hoạt hình theo khung. Nét có thể từ dấu tròn mềm đơn giản đến đầu cọ giàu kết cú hoặc phát triển. Cùng engine vẽ hỗ trợ vẽ chính xác, xây tông, nét trang trí và chia nhỏ kiểu media tự nhiên.

Khi cọ phức tạp về mặt thị giác, xem trước có thể đơn giản hóa để vẽ vẫn phản hồi nhanh và dễ đọc.

![tool-setup](/images/screens/tool-setup.jpg)


## Dynamics và phản hồi đầu vào

Công cụ Cọ phản hồi đầu vào trực tiếp như áp lực bút, tốc độ, hướng, độ nghiêng và giá trị điều khiển khác. Tín hiệu này ảnh hưởng nét theo nhiều cách: độ dày, độ mờ, góc, phản ứng kết cú, hành vi màu, khoảng cách và đặc tính khác thay đổi khi tay di chuyển.

Cọ cảm giác như dụng cụ vẽ vật lý hơn mẫu đóng dấu lặp. Chạm nhẹ tạo nét tinh tế; chuyển động nhanh mở kết cú hoặc hình dạng; hành vi nhạy hướng giúp nét theo cử chỉ tay.

![dynamics](/images/screens/dynamics.jpg)

## Hành vi nét vẽ

Nét có thể trực tiếp, tức thì, hoặc được hỗ trợ bằng làm mịn và ổn định. Các tính năng này giảm rung không mong muốn, làm dịu thay đổi đột ngột và giúp chuyển động dài kiểm soát hơn mà không mất đặc tính đầu vào nghệ sĩ.

Cọ cũng hỗ trợ nhiều cách tích lũy mực/sơn. Nó có thể như nét liên tục, tích lũy dấu chấm lặp, hoặc phát nét theo thời gian khi con trỏ giữ yên. Linh hoạt cho cả nét có chủ ý lẫn xây tông chậm.

Với nét thư pháp hoặc kiểu mực, Cọ tạo nét có hình dạng liên tục hơn thay vì chỉ dựa stamp lặp — hình uyển chuyển, giống dải ruy băng, phản hồi cử chỉ và tốc độ.

![stroke](/images/screens/stroke.jpg)

## Ghi mẫu nét và render mô phỏng

Cọ ghi mẫu nhỏ cách cài đặt sẵn thường được vẽ tay, rồi dùng hồ sơ đó khi render nét xác định bằng hình học thay vì chuyển động trực tiếp. Đường Shift-click thẳng, nét đường dẫn và nét vùng chọn đều có thể dùng mẫu áp lực và vận tốc của cài đặt sẵn đang hoạt động thay vì như đường cơ học phẳng.

Nét xây dựng gần đặc tính cọ hơn. Đường từ path có thể bắt đầu nhẹ, tăng áp lực, taper đi hoặc đổi phản hồi tốc độ như nét tay mẫu, vẫn theo đúng hình path, cạnh chọn hoặc cử chỉ đường thẳng.

## Xử lý hậu kỳ

Cọ ghi nét khi bạn vẽ, rồi phát lại cử chỉ đã ghi khi nhấc bút — tinh chỉnh đường trước khi đặt nét cuối. Phác thảo tự do vẫn đạt hướng sạch hơn, góc sắc hơn hoặc cấu trúc có chủ ý hơn mà không cần vẽ cơ học.

Mở ra gạch bóng và nét construction bám góc sạch trong khi giữ chiều dài và đặc tính vẽ tay; nét ruy băng ổn định theo nghiêng; phát lại nhận biết góc xử lý khúc cong và đoạn thẳng khác nhau. Cọ nhiều đầu chia sẻ path đã sửa trong khi mỗi đầu giữ biến thể riêng; dynamics vẫn định hình nét dọc đường cong cuối khi phát lại. Xử lý hậu kỳ áp dụng nét vẽ, không phải phun airbrush liên tục.

## Màu sắc và kết cú

Nét dùng màu vẽ đang hoạt động, phản hồi gradient hoặc đổi màu qua dynamics. Xử lý kết cú cho phép cọ chuyển giữa phủ đặc và vết đứt, lướt bề mặt — hữu ích cho cọ khô, hạt và bóng biểu cảm.

Màu và kết cú là phần cùng hệ dynamics với hình dạng và độ mờ — một nét phát triển khi di chuyển trên canvas thay vì đồng nhất trực quan.

## Đầu cọ và đội hình

Cọ vẽ bằng nhiều đầu cùng lúc. Nhiều đầu sắp quanh path tạo vết ngòi, nét quạt, hành vi lông cứng, phun, đội hình kết cú hoặc gạch có cấu trúc.

Các đầu theo hướng di chuyển, khác nhau và phân tán khiến nét hữu cơ hơn lặp máy móc. Đặc biệt hữu ích cho cọ media tự nhiên, nét trang trí, lá, lông, gạch bóng và nét cần bất quy tắc có kiểm soát.

![brush-heads](/images/screens/brush-heads.jpg)

## Tải mực và nhặt sơn

Cọ mô phỏng lượng mực/sơn đang mang. Khi nét tiếp tục, tải giảm dần — nét nhẹ hơn, khô hơn, mỏng hơn, thô hơn hoặc vỡ nhiều hơn tùy dynamics.

Tải có thể nạp lại giữa các nét, giữ ở mức chọn hoặc dùng làm tín hiệu điều khiển cho hành vi cọ khác. Xây cọ giống media thật: ướt lúc bắt đầu, cạn dần theo quãng đường, nhúng lại cho lần vẽ sau.

![material-state](/images/screens/material-state.jpg)

## Tiếp xúc bề mặt cọ

Cọ mô phỏng mất tiếp xúc không liên tục với bề mặt — vết đứt khi bút chì, than, cọ khô hoặc marker cạn chỉ bám một phần giấy.

Khi bật mô phỏng tiếp xúc, cọ đang chạm hoặc đang nhấc. Khi chạm, nét đặt bình thường. Khi nhấc, không đặt vật liệu và nét để khoảng trống có chiều dài ngẫu nhiên giữa min và max. Chuyển đổi nhị phân: hiệu ứng không đổi độ mờ, kích thước, độ cứng, khoảng cách hay flow — chỉ có hay không đặt mực.

Mức dễ mất tiếp xúc do ngưỡng tiếp xúc, áp lực bút và tùy chọn tải cọ. Ngưỡng cao hơn làm đứt nét thường xuyên hơn. Áp lực ổn định: nhẹ tăng khả năng mất tiếp xúc; mạnh giúp nét giữ chạm. Khi bật tải cọ, tải thấp làm nét vỡ hơn; tải cao giúp duy trì tiếp xúc — như dụng cụ còn đủ vật liệu bám bề mặt.

Mất tiếp xúc đánh giá theo quãng đường nét, không theo số dab — cọ dày hoặc thưa khoảng cách đều nhất quán. Tính năng hoạt động với render stamp và calligraphic, tạo khoảng trống mạch lạc dọc nét thay vì dab bị bỏ rời rạc.

## Hoạt hình và biến thể

Nguồn cọ hoạt hình đổi khung khi nét tiến triển — cọ có cảm giác chuyển động và đa dạng. Ngẫu nhiên và biến thể theo nét giữ nét lặp không giống hệt; seed ổn định giữ đặc tính khi cần lặp lại.

Hữu ích cho cọ sống động: lông dịch chuyển trong nét, stamp kết cú đổi tinh tế theo thời gian, hoặc công cụ nhiều đầu mỗi đầu một cá tính.

## Quy trình tập trung nghệ sĩ

Cọ được tổ chức để quyết định vẽ thường gặp luôn gần tay; thiết lập ít dùng hơn ở ngoài tầm. Công cụ dễ tiếp cận khi vẽ vẫn hỗ trợ tùy chỉnh sâu cho thiết kế cọ.

Cọ bao quát vẽ hàng ngày lẫn tạo nét chuyên biệt: phác nhanh, minh họa bóng bẩy, render kết cú, mực biểu cảm và hiệu ứng cọ procedural phức tạp — cùng nền tảng linh hoạt.
