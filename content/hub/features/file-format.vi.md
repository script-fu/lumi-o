---
title: "Định dạng tệp (.lum)"
type: docs
url: "hub/features/file-format"
translation_provenance: ai-reviewed
translation_source_sha256: c5e414a0d2870f1b111751c8c462e7ac5a4530103d70cb9be52a9fbd11417028
translation_lock: true
---

Định dạng `.lum` gốc của Lumi là thư mục dự án, không phải một tệp khép kín duy nhất. Nó được thiết kế cho minh họa nhiều lớp: cây lớp sâu, canvas lớn, mặt nạ, hiệu ứng không phá hủy, và điểm kiểm tra không cần sao chép toàn bộ bức tranh.

Nhiệm vụ của định dạng là giữ nguyên cấu trúc làm việc đó — để mở lại dự án đúng như cũ, kiểm tra khi có sự cố, và khôi phục từ điểm kiểm tra gần đây mà không coi tác phẩm là một khối mờ.

## Tách thành phần, có chủ đích

Dự án `.lum` là một thư mục. Cây lớp và thuộc tính ảnh nằm trong XML đọc được. Mỗi lớp và mặt nạ giữ bộ đệm pixel riêng, đặt tên theo tác phẩm chứ không theo ID nội bộ. Đường vector lưu dưới dạng SVG thông thường. Cài đặt bộ lọc nặng nằm ở tệp riêng cạnh ảnh. Hồ sơ ICC lưu một lần ở gốc dự án, để ảnh chụp khôi phục tham chiếu thay vì sao chép.

Sự tách đó mới làm phần còn lại của định dạng khả thi. Lớp không đổi có thể để yên trên đĩa. Bộ đệm hỏng thì hỏng một mình, không kéo cả tệp theo. Pixel lớp bị thiếu trở thành lớp trống vẫn còn tên, vị trí và cài đặt pha trộn; bản tổng hợp nhóm bị thiếu được dựng lại từ lớp con. Dự án vẫn là bản đồ cách bức tranh được xây.

Bảng màu sắc tố thuộc công cụ màu của Lumi. Dự án có thể nhớ bảng nào gắn với ảnh, nhưng thư viện bảng màu nằm ngoài `.lum`.

## Trạng thái chỉnh sửa, không phải bản làm phẳng

Tệp lưu bức tranh đang làm. Lớp vẫn là lớp, nhóm lớp vẫn là nhóm, mặt nạ vẫn là mặt nạ — gồm độ lệch vị trí, khóa, hành vi pha trộn và ngăn bộ lọc. Bộ lọc không phá hủy được lưu thành thao tác và tham số, không phải pixel đã áp sẵn. Lớp một màu phẳng không cần tệp pixel.

Nhóm đã thu gọn cũng giữ một khung nhìn đã tổng hợp. Bản tổng hợp đã lưu đó hiện trên canvas khi nhóm đóng, nên không phải dựng lại lớp con chỉ để nhìn tranh. Chế độ kiểm tra chỉ để xem thì không vào bộ nhớ đệm đó: hiện mặt nạ hoặc alpha để chỉnh được khôi phục như metadata, không bị ghi cứng vào nhóm đã lưu.

## Tệp lớn có thể để một phần trên đĩa

Mở `.lum` không bắt buộc tải mọi pixel. Nội dung trong nhóm đã thu gọn có thể ở lại trên đĩa trong khi bản tổng hợp đã lưu của nhóm hiện ngay. Mở rộng nhóm mới là lúc các lớp, mặt nạ và nhóm lồng được nạp vào bộ nhớ. Nhóm nào vẫn đóng thì vẫn nhẹ.

Tệp cũng ghi nhóm nào thực sự đang dùng. Nhóm trên đường chọn hiện tại có thể mở lại ở trạng thái mở rộng; thư mục khác được lưu dạng thu gọn dù phiên trước có mở. Nhờ đó tệp có cấu trúc sâu không nạp mọi nhánh không dùng vào bộ nhớ ngay lúc mở.

Nhóm lớp vì thế vừa là lựa chọn hiệu năng vừa là cách tổ chức. Tấm nền lớn, thử nghiệm đã lưu trữ và biến thể không dùng có thể nằm trong nhóm đóng mà không chiếm cùng bộ nhớ với lớp đang vẽ. Lưu theo cùng quy tắc: bộ đệm vẫn ẩn được sao chép hoặc bỏ qua như tệp, không nạp lại vào bộ nhớ chỉ để ghi ra lần nữa.

## Điểm kiểm tra chỉ ghi những gì đã đổi

Tập tin → Lưu cập nhật dự án đang làm. Lưu tăng dần và tự động lưu ghi vào cây khôi phục, và chỉ ghi dữ liệu đã thay đổi — bộ đệm lớp đã đổi, không phải bản sao thứ hai của cả ảnh. Mỗi điểm kiểm tra vẫn mang mô tả đầy đủ cây lớp, nên bất kỳ điểm nào trên đường đó đều mở được bằng cách điền pixel không đổi từ điểm kiểm tra cũ hơn và, nếu cần, từ chính tệp đang làm.

Tự động lưu dùng cùng kiểu trong bộ nhớ đệm riêng, nên bảo vệ tự động không phải ghi lại tệp trên đĩa. Nếu mở dự án khi có điểm kiểm tra mới hơn lần lưu đầy đủ cuối, Lumi có thể đề xuất chúng thay vì lặng lẽ bỏ tác phẩm mới hơn. Ảnh khôi phục mở với tên riêng để lần lưu nhanh không ghi đè bản gốc.

## Định dạng để làm việc

`.lum` dùng để tiếp tục vẽ trong Lumi. Định dạng đã làm phẳng hay hướng tương thích dùng để xuất bản, giao nộp và dùng với ứng dụng khác. Vì dự án là thư mục nhiều tệp, nên đóng gói lưu trữ nếu cần mang đi.

Tệp làm việc vẫn phong phú, chỉnh sửa được. Xuất là cách ảnh hoàn thiện hoặc chia sẻ rời khỏi cấu trúc đó.
