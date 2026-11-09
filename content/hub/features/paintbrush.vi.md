---
title: "Công cụ cọ"
type: docs
url: "hub/features/paintbrush"
translation_provenance: ai-reviewed
translation_source_sha256: 467281fdb592ef90257d2dc951c59516e623a5ae34360c4be446a8d487529330
translation_lock: true
---

Công cụ cọ là công cụ vẽ cốt lõi của Lumi: cách đáp ứng, biểu cảm để vẽ, tô màu, tạo bóng, thêm kết cấu và đặt nét trực tiếp trên canvas. Thiết kế mang lại cảm giác tức thì trong khi vẫn cho nghệ sĩ không gian định hình hành vi nét vẽ.

Thay vì một cọ cố định, nó hoạt động như hệ thống vẽ. Hình dạng cọ, kết cấu, hạt bề mặt, chuyển động, áp lực, thời gian và màu đều góp phần tạo nét cuối — phù hợp nét sạch, tô mềm, hiệu ứng media khô, nét thư pháp, kết cấu rải, chùm lông và đội hình cọ nhiều đầu.

## Nét cọ biểu cảm

Cọ có thể dựa trên stamp bitmap, hình dạng procedural hoặc nguồn hoạt hình theo khung. Nét có thể từ dấu tròn mềm đơn giản đến đầu cọ giàu kết cú hoặc phát triển. Cùng engine vẽ hỗ trợ vẽ chính xác, xây tông, nét trang trí và chia nhỏ kiểu media tự nhiên.

Khi cọ phức tạp về mặt thị giác, xem trước có thể đơn giản hóa để vẽ vẫn phản hồi nhanh và dễ đọc.

## Dynamics và phản hồi đầu vào

Công cụ Cọ phản hồi đầu vào trực tiếp như áp lực bút, tốc độ, hướng, độ nghiêng và giá trị điều khiển khác. Tín hiệu này ảnh hưởng nét theo nhiều cách: độ dày, độ mờ, góc, phản ứng kết cú, hành vi màu, khoảng cách và đặc tính khác thay đổi khi tay di chuyển.

Cọ cảm giác như dụng cụ vẽ vật lý hơn mẫu đóng dấu lặp. Chạm nhẹ tạo nét tinh tế; chuyển động nhanh mở kết cấu hoặc hình dạng; hành vi nhạy hướng giúp nét theo cử chỉ tay.

## Hành vi nét vẽ

Nét có thể trực tiếp, tức thì, hoặc được hỗ trợ bằng làm mịn và ổn định. Các tính năng này giảm rung không mong muốn, làm dịu thay đổi đột ngột và giúp chuyển động dài kiểm soát hơn mà không mất đặc tính đầu vào nghệ sĩ.

Cọ cũng hỗ trợ nhiều cách tích lũy mực/sơn. Nó có thể như nét liên tục, tích lũy dấu chấm lặp, hoặc phát nét theo thời gian khi con trỏ giữ yên. Linh hoạt cho cả nét có chủ ý lẫn xây tông chậm.

Với nét thư pháp hoặc kiểu mực, Cọ tạo nét có hình dạng liên tục hơn thay vì chỉ dựa stamp lặp — hình uyển chuyển, giống dải ruy băng, phản hồi cử chỉ và tốc độ.

## Ghi mẫu nét và render mô phỏng

Cọ ghi mẫu nhỏ cách cài đặt sẵn thường được vẽ tay, rồi dùng hồ sơ đó khi render nét xác định bằng hình học thay vì chuyển động trực tiếp. Đường Shift-click thẳng, nét đường dẫn và nét vùng chọn đều có thể dùng mẫu áp lực và vận tốc của cài đặt sẵn đang hoạt động thay vì như đường cơ học phẳng.

Nét xây dựng gần đặc tính cọ hơn. Đường từ path có thể bắt đầu nhẹ, tăng áp lực, taper đi hoặc đổi phản hồi tốc độ như nét tay mẫu, vẫn theo đúng hình path, cạnh chọn hoặc cử chỉ đường thẳng.

## Xử lý hậu kỳ

Cọ ghi nét khi bạn vẽ, rồi phát lại cử chỉ đã ghi khi nhấc bút — tinh chỉnh đường trước khi đặt nét cuối. Phác thảo tự do vẫn đạt hướng sạch hơn, góc sắc hơn hoặc cấu trúc có chủ ý hơn mà không cần vẽ cơ học.

Mở ra gạch bóng và nét construction bám góc sạch trong khi giữ chiều dài và đặc tính vẽ tay; nét ruy băng ổn định theo nghiêng; phát lại nhận biết góc xử lý khúc cong và đoạn thẳng khác nhau. Cọ nhiều đầu chia sẻ path đã sửa trong khi mỗi đầu giữ biến thể riêng; dynamics vẫn định hình nét dọc đường cong cuối khi phát lại. Xử lý hậu kỳ áp dụng nét vẽ, không phải phun airbrush liên tục.

## Màu sắc và kết cấu

Nét dùng màu vẽ đang hoạt động, phản hồi gradient hoặc đổi màu qua dynamics. Xử lý kết cấu cho phép cọ chuyển giữa phủ đặc và vết đứt, lướt bề mặt — hữu ích cho cọ khô, hạt và bóng biểu cảm.

Màu và kết cấu là phần cùng hệ dynamics với hình dạng và độ mờ — một nét phát triển khi di chuyển trên canvas thay vì đồng nhất trực quan.

Kết cấu ở đầu cọ làm vỡ chính stamp. Kết cấu bề mặt, phần ngay sau, là hạt và độ nhám của bề mặt mà nét gặp.

## Kết cấu bề mặt

Kết cấu bề mặt cho nét hạt và độ nhám của một bề mặt: giấy, canvas, hoặc một mẫu khác. Lumi có những hạt hợp với việc này. Vùng sáng để sơn đi qua. Vùng tối giữ lại, nên vết vỡ theo cách media khô vỡ trên độ nhám.

Chỗ hạt nằm có thể đổi theo nét. Mỗi nét có thể bắt đầu ở chỗ riêng trong mẫu, và một chỗ ngoặt có thể lăn một độ lệch mới. Tô bóng qua lại là nơi điều này hiện ra: mỗi lần đổi chiều lấy một pha nhám mới, và những lượt chồng lên nhau dựng một bóng vỡ, sát thực. Đường cong từ từ có thể giữ pha đang nằm dưới nét, nên độ nhám liền mạch suốt nhịp quét.

Cọ đóng dấu và nét thư pháp đều nhận hạt này. Một hàng dấu chấm và một dải mực liên tục có thể vỡ trên cùng kiểu độ nhám. Trên nét thư pháp, đường vẫn là một dấu có hình, và hạt quyết định dải đó mở ở đâu, bị giữ ở đâu.

Độ nhám mở đến mức nào có thể đổi dọc nét. Áp lực và đầu vào khác dịch giữa một cách đọc khô hơn và một cách đọc đầy hơn. Ở cách đọc khô, các lỗ vẫn trống. Khi nét mở ra, các đảo đã tô có thể lớn vào những lỗ đó, nên hạt được lấp từ mép. Ở cách đọc đầy, nét khớp mẫu mở hơn.

Các dấu chấm chồng trong một nét có thể dựng độ đục trong các đảo. Chúng cũng có thể đặt một lớp phủ duy nhất mà vòng sau của nét đó viết lại, nên hạt không chồng lên chính nó.

## Đầu cọ và đội hình

Cọ vẽ bằng nhiều đầu cùng lúc. Nhiều đầu sắp quanh path tạo vết ngòi, nét quạt, hành vi lông cứng, phun, đội hình kết cú hoặc gạch có cấu trúc.

Các đầu theo hướng di chuyển, khác nhau và phân tán khiến nét hữu cơ hơn lặp máy móc. Đặc biệt hữu ích cho cọ media tự nhiên, nét trang trí, lá, lông, gạch bóng và nét cần bất quy tắc có kiểm soát.

Cọ tạo được cũng có thể dựng thành một trường lông. Hình dạng và chuyển động trong nét của trường đó được mô tả bên dưới.

## Hình dạng lông

Cọ lông cứng là một trường các dấu riêng nằm trong hình được tạo. Đường viền của hình quyết định lông nằm ở đâu: tròn, vuông, thoi, tam giác, gai, góc mềm, hoặc tỷ lệ bị kéo. Mỗi sợi lông giữ dấu của chính nó.

Khoảng cách và kích thước đặt độ khít và cỡ mỗi sợi. Biến đổi kích thước làm lông kề nhau khác nhau, từ một đầu đều đến đầu có lông chạy từ cỡ đầy xuống gần như trống. Độ đều dịch trường từ lưới đều sang phân tán lỏng hơn. Sự tụ kéo lông về ít tâm hơn, nên đầu đọc như những chùm. Trường cũng có thể được làm thưa.

Đầu có thể tròn. Chúng cũng có thể thành đa giác nhỏ nằm trong từng sợi. Đầu xếp đều quanh vòng, hoặc để rách, và có thể nằm ra mép hoặc kéo vào trong. Chỗ các đầu chồng, dấu mạnh hơn là dấu hiện ra.

Mỗi đầu của cọ nhiều đầu có cách sắp riêng, nên một quạt đầu là một bộ chùm cùng họ.

Kích thước lông có thể theo áp lực bút và các đầu vào khác định hình nét. Khi kích thước đổi, đầu có thể được vẽ lại, nên chạm nhẹ và nhấn chắc khác nhau ở chính những sợi lông.

## Lông theo nét vẽ

Cọ lông cứng có thể đổi trong khi nét vẫn đang vẽ. Từng sợi lớn lên và nhỏ lại. Một số phai đi, số khác mọc ở chỗ khác, nên tính cách của đầu đi dọc dấu. Mật độ chùm giữ đều trong khi lông sắp lại.

Hoạt hình tiến cùng các dấu chấm của nét. Nó có thể bắt đầu sau khi dấu đã đi một quãng, hoặc khi tay ngoặt. Các trạng thái ở giữa diễn ra dọc đường, và cùng một cử chỉ cho cùng một chuỗi.

Airbrush là một chuyển động khác. Nó có thể đặt một sắp xếp lông mới khi phun, nên nét giữ yên vẫn làm mới tia. Nét thư pháp giữ ngòi liên tục, và ngòi đó vẫn nhận kết cấu bề mặt. Chuyển động của lông thuộc về cọ lông cứng đóng dấu.

## Tải mực và nhặt sơn

Cọ mô phỏng lượng mực/sơn đang mang. Khi nét tiếp tục, tải giảm dần — nét nhẹ hơn, khô hơn, mỏng hơn, thô hơn hoặc vỡ nhiều hơn tùy dynamics.

Tải có thể nạp lại giữa các nét, giữ ở mức chọn hoặc dùng làm tín hiệu điều khiển cho hành vi cọ khác. Xây cọ giống media thật: ướt lúc bắt đầu, cạn dần theo quãng đường, nhúng lại cho lần vẽ sau.

## Tiếp xúc bề mặt cọ

Cọ mô phỏng mất tiếp xúc không liên tục với bề mặt — vết đứt khi bút chì, than, cọ khô hoặc marker cạn chỉ bám một phần giấy.

Khi bật mô phỏng tiếp xúc, cọ đang chạm hoặc đang nhấc. Khi chạm, nét đặt bình thường. Khi nhấc, không đặt vật liệu và nét để khoảng trống có chiều dài ngẫu nhiên giữa min và max. Chuyển đổi nhị phân: hiệu ứng không đổi độ mờ, kích thước, độ cứng, khoảng cách hay flow — chỉ có hay không đặt mực.

Mức dễ mất tiếp xúc do ngưỡng tiếp xúc, áp lực bút và tùy chọn tải cọ. Ngưỡng cao hơn làm đứt nét thường xuyên hơn. Áp lực ổn định: nhẹ tăng khả năng mất tiếp xúc; mạnh giúp nét giữ chạm. Khi bật tải cọ, tải thấp làm nét vỡ hơn; tải cao giúp duy trì tiếp xúc — như dụng cụ còn đủ vật liệu bám bề mặt.

Mất tiếp xúc đánh giá theo quãng đường nét, không theo số dab — cọ dày hoặc thưa khoảng cách đều nhất quán. Tính năng hoạt động với render stamp và calligraphic, tạo khoảng trống mạch lạc dọc nét thay vì dab bị bỏ rời rạc.

## Hoạt hình và biến thể

Nguồn cọ hoạt hình đổi khung khi nét tiến triển — cọ có cảm giác chuyển động và đa dạng. Ngẫu nhiên và biến thể theo nét giữ nét lặp không giống hệt; seed ổn định giữ đặc tính khi cần lặp lại.

Những hành vi này hữu ích cho cọ cần cảm giác sống: stamp có kết cấu đổi theo thời gian, hoặc công cụ nhiều đầu mỗi đầu một cá tính. Cọ lông cứng có chuyển động trong nét riêng, mô tả ở trên.

## Nét OpenGL

Một cài đặt sẵn công cụ có thể vẽ nét bằng OpenGL. Khác biệt thấy rõ nhất ở airbrush lớn và stamp kết cấu lớn, chúng đi cùng tay. Cửa sổ hình ảnh cũng có thể vẽ chồng lớp bằng OpenGL, nhờ đó việc di chuyển trên canvas vẫn mượt. Cả hai được mô tả trong [OpenGL](/hub/features/opengl/).

## Quy trình tập trung nghệ sĩ

Cọ được tổ chức để quyết định vẽ thường gặp luôn gần tay; thiết lập ít dùng hơn ở ngoài tầm. Công cụ dễ tiếp cận khi vẽ vẫn hỗ trợ tùy chỉnh sâu cho thiết kế cọ.

Cọ bao quát vẽ hàng ngày lẫn tạo nét chuyên biệt: phác nhanh, minh họa bóng bẩy, render kết cú, mực biểu cảm và hiệu ứng cọ procedural phức tạp — cùng nền tảng linh hoạt.
