---
title: "Khôi phục tệp"
type: docs
url: "hub/features/recovery"
translation_provenance: ai-reviewed
translation_source_sha256: 59495d24302cb3493b90bc61a6dd1ffb9bb9c30b179f7be388882fe4f45a5075
translation_lock: true
---

Hệ thống khôi phục của Lumi bảo vệ tác phẩm khỏi sự cố, sai sót và phiên bị gián đoạn. Dự án có lưới an toàn mà không buộc nghệ sĩ sao chép tệp thủ công liên tục.

Khôi phục dựa trên hai ý tưởng: bảo vệ nền tự động và điểm kiểm tra có chủ ý. Chúng cùng giúp giữ tác phẩm gần đây trong khi vẫn cho phép quay lại các thời điểm trước trong dự án.

![recover](/images/screens/recover.jpg)

## Bảo vệ tự động

Khi hình đang được chỉnh sửa, Lumi có thể tách dữ liệu khôi phục khỏi tệp làm việc chính. Dự án không cần ghi lại mỗi khi tạo ảnh chụp an toàn.

Nếu có sự cố, trạng thái khôi phục tự động có thể cung cấp phiên bản gần đây của tác phẩm — có thể mới hơn lần lưu có chủ ý cuối. Mục tiêu đơn giản: giảm công việc mất khi phiên kết thúc bất ngờ.

## Điểm kiểm tra có chủ ý

Một số thời điểm trong bức tranh đáng lưu có chủ ý: trước thay đổi màu lớn, sau phác thảo thành công, trước quyết định làm phẳng hoặc khi thử hướng mạo hiểm.

Lumi hỗ trợ điểm kiểm tra cấp dự án cho những lúc đó. Chúng nhẹ hơn giữ bản sao đầy đủ riêng cho mỗi thử nghiệm, nhưng vẫn giúp nghệ sĩ quay lại điểm có ý nghĩa trong lịch sử tác phẩm.

## Khôi phục có ngữ cảnh

Trạng thái khôi phục được trình bày như phiên bản tác phẩm thay vì tệp thô phải tìm thủ công. Nghệ sĩ so sánh lần lưu tự động gần đây và điểm kiểm tra có chủ ý, rồi mở trạng thái phù hợp nhất để tiếp tục.

Hình khôi phục mở như tài liệu làm việc, cho phép kiểm tra trước khi quyết định cách lưu hoặc tiếp tục.

## Giữ khôi phục thực tế

Hệ thống khôi phục hữu ích cũng phải quản lý được. Lumi được thiết kế để sắp xếp dữ liệu khôi phục và xóa trạng thái cũ khi không còn cần.

Điều này giữ an toàn không trở nên lộn xộn. Khôi phục vẫn chạy nền trong khi nghệ sĩ kiểm soát lượng lịch sử được giữ theo thời gian.

## Tự tin khi làm việc

Mục đích khôi phục tệp không phải thay thế lưu mà làm tác phẩm sáng tạo bớt dễ vỡ. Nghệ sĩ có thể vẽ, thử nghiệm và chấp nhận rủi ro khi biết Lumi duy trì cách bổ sung để quay lại khi phiên, tệp hoặc quyết định gặp trục trặc.
