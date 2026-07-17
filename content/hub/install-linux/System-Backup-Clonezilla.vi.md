---
title: "Sao lưu hệ thống bằng Clonezilla"
type: docs
translation_provenance: ai-reviewed
translation_source_sha256: b3453289d7da56bb4fc9039616edb73e537acd6a722f0eb8a000e4a398016863
url: "hub/install-linux/System-Backup-Clonezilla"
translation_lock: true
---
Việc sao lưu các tệp quan trọng của bạn để quay lại phiên bản cũ hơn hoặc thay thế dữ liệu bị hỏng là điều bình thường. Tuy nhiên, một loại bản sao lưu thiết yếu khác là **bản sao đĩa**, bản sao lưu hoàn chỉnh về trạng thái hệ thống của bạn.

Khi hệ thống của bạn đã được thiết lập và hoạt động tốt, việc tạo bản sao lưu đầy đủ là rất quan trọng để khôi phục môi trường của bạn trong trường hợp thảm họa xảy ra. Bản sao lưu này bổ sung cho việc thường xuyên lưu dữ liệu làm việc của bạn.

[Clonezilla](https://clonezilla.org/) là phần mềm tạo ảnh đĩa và nhân bản ổ mã nguồn mở, miễn phí. Nó cho phép tạo và khôi phục bản sao lưu đầy đủ ổ cứng máy tính, nên được dùng rộng rãi cả trong CNTT chuyên nghiệp lẫn ở nhà.

Tốt hơn là có một bản sao lưu và không cần đến nó hơn là cần một bản sao lưu và không có nó.


## Tính năng chính của Clonezilla

- **Tạo ảnh đĩa**: Clonezilla tạo bản sao chính xác của ổ cứng, gồm hệ điều hành, ứng dụng và dữ liệu.
- **Sao lưu và khôi phục**: Nó cho phép bạn tạo ảnh sao lưu của ổ cứng và khôi phục nó trong trường hợp bị lỗi hoặc di chuyển sang ổ đĩa mới.
- **Nguồn mở và miễn phí**: Clonezilla hoàn toàn miễn phí sử dụng và mã nguồn có sẵn để sửa đổi và tùy chỉnh.


## Sử dụng Clonezilla để sao lưu

### Các bước chuẩn bị

Bạn sẽ cần một ổ USB cho Clonezilla và một ổ cứng ngoài lớn hơn ổ đĩa trong mà bạn định sao chép.

Các bước này giúp đơn giản hóa quy trình dựa trên [hướng dẫn chính thức](https://clonezilla.org//fine-print-live-doc.php?path=./clonezilla-live/doc/01_Save_disk_image/00-boot-clonezilla-live-cd.doc#00-boot-clonezilla-live-cd.doc). Bạn nên xem lại hướng dẫn đầy đủ, bao gồm ảnh chụp màn hình để hiểu rõ hơn.

1. **Tạo Clonezilla Live USB hoặc CD/DVD**: Làm theo hướng dẫn chi tiết trên Clonezilla [trang web](https://clonezilla.org/liveusb.php) để tạo USB hoặc CD/DVD có khả năng khởi động.

2. **Kết nối ổ đĩa dự phòng bên ngoài của bạn**: Cắm ổ đĩa ngoài của bạn và đảm bảo nó được hệ thống của bạn nhận ra. Đây sẽ là đích đến cho bản sao lưu của bạn.

3. **Xác minh bố cục phân vùng của bạn**: Sử dụng lệnh `lsblk` trong thiết bị đầu cuối để xác minh bố cục phân vùng của ổ cứng chính của bạn. Lưu ý tên thiết bị chính.

4. **Khởi động từ Clonezilla Live USB Drive**: Khởi động lại máy tính của bạn và khởi động từ phương tiện Clonezilla bạn đã tạo. Bạn có thể cần truy cập cài đặt BIOS/UEFI của mình (thường bằng cách nhấn F2, F12, ESC hoặc DEL trong khi khởi động) và điều chỉnh thứ tự khởi động để ưu tiên ổ USB.



### Sao lưu bằng Clonezilla

1. **Chọn chế độ sao lưu**: Khi Clonezilla khởi động, chọn chế độ "device-device". Chế độ này cho phép sao chép trực tiếp ổ trong sang thiết bị ngoài.

2. **Chọn Thiết bị nguồn**: Chọn ổ đĩa trong chính.

3. **Chọn Thiết bị đích**: Chọn ổ đĩa sao lưu bên ngoài của bạn làm thiết bị đích. Hãy cẩn thận khi chọn thiết bị để tránh ghi đè lên dữ liệu quan trọng. Đảm bảo rằng ổ đĩa đích có kích thước bằng hoặc lớn hơn ổ đĩa nguồn.

4. **Bắt đầu quá trình sao lưu**: Clonezilla sẽ bắt đầu quá trình sao lưu. Tùy kích thước phân vùng và tốc độ ổ, quá trình này có thể mất từ vài phút đến vài giờ.

5. **Gắn nhãn cho bản sao lưu của bạn**: Sau khi sao lưu xong, hãy gắn nhãn ổ USB và ổ cứng ngoài với ngày tháng và hệ thống bạn đã sao lưu. Hãy cất giữ chúng ở nơi an toàn.

---

### Khôi phục từ bản sao lưu

Nếu bạn cần khôi phục hệ thống Debian của mình từ bản sao lưu, hãy làm theo các bước sau:

1. **Khởi động từ phương tiện Clonezilla**: Cắm USB Clonezilla và khởi động từ đó, làm theo các bước tương tự như khi sao lưu.

2. **Chọn chế độ khôi phục**: Chọn lại chế độ "device-device", nhưng lần này bạn khôi phục từ ảnh sao lưu. Thao tác này sao chép mọi dữ liệu từ ổ ngoài trở lại ổ trong.

3. **Chọn Thiết bị nguồn**: Chọn ổ đĩa ngoài nơi lưu trữ bản sao lưu.

4. **Chọn Thiết bị đích**: Chọn ổ đĩa trong nơi bạn muốn khôi phục bản sao lưu.

5. **Bắt đầu quá trình khôi phục**: Clonezilla sẽ bắt đầu quá trình khôi phục. Giống như việc sao lưu, thời gian cần thiết sẽ phụ thuộc vào kích thước ổ đĩa và tốc độ phần cứng của bạn.

---

## Ghi chú cuối cùng

Sao lưu đĩa bằng Clonezilla đảm bảo rằng toàn bộ hệ thống của bạn—hệ điều hành, cài đặt và ứng dụng—được bảo toàn. Với nỗ lực tối thiểu, bạn có thể bảo vệ hệ thống của mình khỏi sự cố nghiêm trọng và giảm thiểu thời gian ngừng hoạt động trong trường hợp xảy ra sự cố.

Hãy nhớ rằng **sao lưu là cần thiết**. Thường xuyên cập nhật các bản sao lưu của bạn và kiểm tra chúng định kỳ để đảm bảo bạn có thể khôi phục hệ thống của mình khi cần.

Sau khi khởi động, bạn có thể cắm ổ đĩa sao lưu ngoài và kiểm tra cấu trúc phân vùng của nó bằng tiện ích Disks trong Linux. Ổ đĩa sao lưu phải phản ánh cấu trúc của ổ đĩa trong, có cùng phân vùng và một số dung lượng chưa sử dụng nếu ổ đĩa ngoài lớn hơn.