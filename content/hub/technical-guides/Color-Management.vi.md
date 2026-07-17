---
title: "Quản lý màu sắc"
type: docs
weight: 15
translation_provenance: ai-reviewed
translation_source_sha256: 60e00f1b5e0b4a7bb3034ca99dd3f8f51f6bc52b1629a9ab717d2ac2166393ee
translation_lock: true
url: "hub/technical-guides/Color-Management"
---
Lumi-o được cấu hình để dùng ngay. Miễn là bạn làm việc trên ảnh có **độ chính xác 16 bit trở lên**, phần mềm đã thiết lập sẵn hồ sơ soft-proof CMYK mặc định đi kèm và hồ sơ sRGB tích hợp; mọi thứ hoạt động mà không cần cấu hình thêm.

Với người cần kiểm soát sâu hơn, hướng dẫn này giải thích mô hình quản lý màu cốt lõi của Lumi, sự khác biệt giữa hồ sơ ảnh và hồ sơ soft-proof, vị trí các điều khiển và cách các hồ sơ mặc định đi kèm ứng dụng.

## Tóm tắt nhanh

Lumi dùng ba vai trò hồ sơ khác nhau:

1. **Hồ sơ làm việc của ảnh**
   - Xác định ý nghĩa các giá trị RGB hoặc thang độ xám của ảnh.
   - Dùng cho thao tác gán/chuyển đổi.
   - Ví dụ điển hình: sRGB tích hợp, Adobe RGB.

2. **Hồ sơ hiển thị**
   - Mô tả màn hình của bạn.
   - Dùng để hiển thị ảnh chính xác trên màn hình.
   - Thường do hệ thống cung cấp hoặc chọn trong Tùy chọn.

3. **Hồ sơ soft-proof**
   - Mô phỏng thiết bị đầu ra hoặc điều kiện in khác.
   - **Không** thay đổi giá trị pixel của ảnh.
   - Ví dụ điển hình: hồ sơ máy in CMYK như `CoatedFOGRA39`.

## Hồ sơ ảnh và hồ sơ soft-proof

### Hồ sơ ảnh

Dùng khi bạn muốn cho Lumi biết ảnh thực sự nằm trong không gian màu nào.

Hai thao tác phổ biến:

- **Gán hồ sơ**
  - Đổi nhãn hồ sơ gắn với ảnh.
  - **Không** chuyển đổi giá trị pixel.
  - Chỉ dùng khi số pixel đã thuộc không gian của hồ sơ đó.

- **Chuyển sang hồ sơ**
  - Chuyển đổi giá trị pixel từ hồ sơ ảnh hiện tại sang hồ sơ mới.
  - Dùng khi bạn muốn ảnh thực sự chuyển sang không gian làm việc khác.

**Vị trí menu:**
- Hình ảnh > Quản lý màu > Gán hồ sơ màu...
- Hình ảnh > Quản lý màu > Chuyển sang hồ sơ màu...

### Hồ sơ soft-proof

Dùng khi bạn muốn xem trước ảnh sẽ được tái tạo thế nào trên thiết bị đích hoặc điều kiện in.

Soft-proof:
- giữ nguyên không gian làm việc của ảnh
- thay đổi pipeline xem trước
- có thể đánh dấu màu ngoài gam
- nhằm xem trước, không gán lại dữ liệu ảnh

**Vị trí menu:**
- Hình ảnh > Quản lý màu > Cài đặt Soft-Proof > Chọn hồ sơ Soft-Proof...
- Hình ảnh > Quản lý màu > Cài đặt Soft-Proof > Ý định hiển thị
- Hình ảnh > Quản lý màu > Cài đặt Soft-Proof > Bù điểm đen
- Xem > Quản lý màu > Bật xem trước Soft-Proof
- Xem > Quản lý màu > Đánh dấu màu ngoài gam

## Cách xem xem trước soft-proof

Có hai điểm vào chính để bật/tắt soft-proof.

### 1. Menu Xem

Dùng:
- Xem > Quản lý màu > Bật xem trước Soft-Proof

Thao tác này bật hoặc tắt mô phỏng xem trước trên màn hình hiện tại.

### 2. Nút trên thanh trạng thái

Lumi cũng đặt điều khiển soft-proof trực tiếp trên thanh trạng thái phía dưới.

- **Nhấp chuột trái** (bật/tắt): bật hoặc tắt màu soft-proof
- **Nhấp chuột phải**: mở popover soft-proof để chỉnh:
  - hồ sơ hiện tại
  - bộ chọn hồ sơ
  - ý định hiển thị
  - bù điểm đen
  - đánh dấu ngoài gam

{{< callout type="warning" >}}
**Lưu ý quan trọng về độ chính xác**
Xem trước soft-proof chỉ bật cho ảnh **16 bit và 32 bit**.
Với ảnh **8 bit**, nút bị tắt và Lumi sẽ nhắc bạn chuyển sang độ sâu cao hơn trước khi xem trước màu chính xác.
{{< /callout >}}

## Tùy chọn và mặc định

Mặc định toàn cục nằm tại:
- Chỉnh sửa > Tùy chọn > Quản lý màu

Các phần liên quan:
- **Hồ sơ màn hình thủ công**
- **Hồ sơ RGB ưu tiên**
- **Hồ sơ thang độ xám ưu tiên**
- **Soft-Proofing**

### Mặc định hiện tại của Lumi

#### Không gian làm việc

Các ICC không gian làm việc đi kèm hiện có trong thư mục dữ liệu dùng chung:
- `AdobeRGB1998.icc`
- `AppleRGB.icc`

Với công việc sRGB tiêu chuẩn, Lumi cũng cung cấp **hồ sơ làm việc sRGB tích hợp sẵn**.

#### Mặc định soft-proof

Các hồ sơ soft-proof đi kèm hiện được cài:
- `CoatedFOGRA39.icc`
- `USWebCoatedSWOP.icc`
- `JapanColor2001Coated.icc`

Khi có sẵn, `CoatedFOGRA39.icc` được dùng làm hồ sơ tham chiếu soft-proof/CMYK mặc định đi kèm.

## Quy trình thực tế

### Vẽ và làm việc trên màn hình bình thường

- Giữ ảnh trong sRGB tích hợp hoặc không gian làm việc RGB hợp lệ khác.
- Để Lumi dùng hồ sơ màn hình hệ thống nếu có.

### Xem trước bản in

- Giữ ảnh trong không gian làm việc RGB tiêu chuẩn.
- Chọn hồ sơ soft-proof khớp điều kiện in mục tiêu (ví dụ FOGRA39).
- Bật xem trước soft-proof.
- Tùy chọn bật cảnh báo gam để thấy ý định hiển thị bị cắt.
