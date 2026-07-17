---
title: "Git"
type: docs
translation_provenance: ai-reviewed
translation_source_sha256: c2c03721fbcc205a8c33d945786290712bc60e71beb18b9a1dda1a34d975051f
url: "hub/scripting/tools/git"
translation_lock: true
---
Sử dụng Git để theo dõi các thay đổi đối với plugin của bạn, khôi phục lỗi và chia sẻ mã giữa các máy.

## Tại sao phải sắp xếp mã của bạn?

Khi bạn có nhiều tập lệnh, cấu trúc thư mục nhất quán sẽ tiết kiệm thời gian và giúp việc kiểm soát phiên bản trở nên đơn giản.

## Thiết lập cấu trúc thư mục mã

Một trong những cách đơn giản nhất để sắp xếp các dự án của bạn là tạo một **thư mục mã** chuyên dụng trên máy cục bộ của bạn. Bên trong thư mục này, bạn có thể tạo các thư mục con cho từng dự án hoặc kho lưu trữ. Đây là cấu trúc thư mục được đề xuất:

```plaintext
/home/your-username/code/
  ├── project1/
  ├── project2/
  └── project3/
```

Mỗi thư mục con (ví dụ: `project1`) đại diện cho một **kho lưu trữ**, đây là nơi bạn sẽ lưu trữ các tệp và mã cho dự án đó.

## Kho lưu trữ là gì?

**kho lưu trữ** (hoặc **repo**) về cơ bản là một thư mục chứa nội dung mà Git theo dõi. Khi bạn tạo một kho lưu trữ cục bộ, bạn khởi tạo Git trong thư mục đó, cho phép bạn lưu mọi thay đổi đối với bản sao trực tuyến.

### Kho lưu trữ cục bộ và từ xa

- **Local Repo**: Đây là kho lưu trữ được lưu trữ trên máy tính của bạn, ở một trong các thư mục dự án của bạn.
- **Repo Repo**: Phiên bản của kho lưu trữ được lưu trữ trực tuyến (ví dụ: trên GitLab hoặc GitHub).

## Sử dụng Git và GitHub

Khi đã có cấu trúc thư mục, bạn có thể khởi tạo Git và kết nối các dự án cục bộ của mình với GitHub. Hãy làm theo các bước sau để bắt đầu:

### Các bước cơ bản để sử dụng Git và GitHub

1. **Cài đặt Git**
2. **Tạo tài khoản GitHub**
3. **Tạo kho lưu trữ trống trên GitHub**
4. **Khởi tạo Git trong dự án địa phương của bạn**
5. **Kết nối kho lưu trữ cục bộ của bạn với GitHub**
6. **Sắp xếp các tập tin của bạn**
7. **Cam kết thay đổi của bạn**
8. **Đẩy các thay đổi của bạn lên GitHub**
9. **Xem kho lưu trữ của bạn trực tuyến**

### 1. Cài đặt Git

Nếu bạn chưa cài đặt Git, bạn có thể cài đặt trên Linux bằng cách sử dụng:

```sh
sudo apt install git
```

### 2. Tạo tài khoản GitHub

Nếu bạn chưa có tài khoản, hãy truy cập [GitHub](https://github.com/) để đăng ký. Sau khi đăng ký, bạn có thể tạo kho lưu trữ trên GitHub để lưu trữ mã của mình trực tuyến.

### 3. Tạo Kho lưu trữ trống trên GitHub

1. **Đăng nhập vào GitHub**: Truy cập [GitHub](https://github.com/) và đăng nhập vào tài khoản của bạn.
2. **Tạo kho lưu trữ mới**:
   - Nhấp vào biểu tượng **++** ở góc trên bên phải và chọn **Kho lưu trữ mới**.
   - Nhập tên kho lưu trữ (ví dụ: `your-repository`).
   - Thêm mô tả nếu muốn.
   - Chọn chế độ hiển thị **Công khai** hoặc **Riêng tư**.
   - **Không** khởi tạo kho lưu trữ bằng README, `.gitignore` hoặc giấy phép (để tránh xung đột).
   - Nhấp vào **Tạo kho lưu trữ**.

### 4. Khởi tạo Git trong dự án cục bộ của bạn

Để bắt đầu theo dõi thư mục dự án bằng Git, hãy mở terminal của bạn, điều hướng đến thư mục dự án và chạy:

```sh
cd code/your/project/folder
git init
```

Lệnh này khởi tạo kho lưu trữ Git trống trong thư mục dự án của bạn.

### 5. Kết nối Kho lưu trữ cục bộ của bạn với GitHub

Tiếp theo, bạn sẽ muốn kết nối kho lưu trữ cục bộ của mình với GitHub. Sau khi tạo một kho lưu trữ trống trên GitHub, hãy thêm nó làm điều khiển từ xa cho dự án cục bộ của bạn:

```sh
cd code/your/project/folder
git remote add origin https://github.com/your-username/your-repository.git
```

Thay thế `your-username` và `your-repository` bằng tên người dùng GitHub thực tế của bạn và tên kho lưu trữ. Lệnh này liên kết dự án cục bộ của bạn với kho lưu trữ từ xa trên GitHub.

### 6. Sắp xếp các tập tin của bạn

Trước khi có thể lưu các thay đổi của mình trong Git, bạn cần cho Git biết những tệp bạn đã thay đổi và muốn lưu. Điều này được gọi là "dàn dựng" các tập tin của bạn. Sử dụng lệnh sau để xử lý tất cả các tệp đã sửa đổi hoặc mới:

```sh
git add .
```Điều này yêu cầu Git theo dõi những thay đổi bạn đã thực hiện đối với tất cả các tệp trong dự án của mình. Bạn cũng có thể sắp xếp các tệp cụ thể bằng cách thay thế `.` bằng tên tệp.

### 7. Cam kết thay đổi của bạn

Sau khi dàn dựng, bước tiếp theo là lưu (hoặc "xác nhận") các thay đổi đối với kho lưu trữ Git cục bộ của bạn. Khi cam kết, bạn phải luôn kèm theo thông báo mô tả những thay đổi bạn đã thực hiện. Ví dụ:

```sh
git commit -m "Add new feature"
```

Cờ `-m` cho phép bạn viết thông báo tóm tắt những thay đổi bạn đã thực hiện. Thông báo này giúp bạn và những người khác hiểu những gì đã được sửa đổi trong cam kết này.

### 8. Đẩy các thay đổi của bạn lên GitHub

Khi bạn đã thực hiện các thay đổi cục bộ, giờ đây bạn có thể "đẩy" chúng lên GitHub để kho lưu trữ từ xa của bạn được cập nhật. Chạy lệnh sau để tải lên các thay đổi của bạn:

```sh
git push -u origin main
```

Nhánh `main` là nhánh mặc định trong GitHub nơi mã được lưu trữ và lệnh này tải các thay đổi cục bộ của bạn lên kho lưu trữ từ xa, giúp chúng có thể truy cập trực tuyến.

### 9. Xem mã của bạn trên GitHub

Sau khi đẩy mã của mình lên GitHub, bạn có thể xem kho lưu trữ của mình trong giao diện web GitHub. Bạn sẽ thấy các tệp từ kho lưu trữ cục bộ của mình, cùng với lịch sử cam kết hiển thị những thay đổi bạn đã thực hiện.

## Kết luận

Bằng cách sắp xếp mã của bạn vào các thư mục chuyên dụng và sử dụng GitHub để quản lý cũng như sao lưu kho lưu trữ, bạn sẽ giữ cho dự án của mình có cấu trúc tốt và dễ dàng truy cập. Sau khi bạn có phiên bản mã đang hoạt động, hãy đẩy mã đó lên GitHub. Sau đó, bạn có thể dễ dàng theo dõi mọi thay đổi bằng giao diện web GitHub hoặc Visual Studio Code, trong đó làm nổi bật các dòng đã sửa đổi. Cách tiếp cận này cho phép bạn tiếp tục tinh chỉnh và mở rộng mã của mình mà không mất dấu tiến trình hoặc thay đổi.

Git và các nền tảng như GitHub và GitLab là những công cụ mạnh mẽ và mặc dù chúng có thể phức tạp nhưng có rất nhiều tài nguyên trực tuyến có sẵn để giúp bạn hiểu rõ hơn về chúng. Một trong những tài nguyên có giá trị nhất mà tôi tìm thấy là những công cụ trợ giúp AI như ChatGPT. Bạn có thể mô tả những gì bạn cần hoàn thành và những công cụ này sẽ kiên nhẫn hướng dẫn bạn thực hiện từng bước quy trình.

## Bảng thuật ngữ

Dưới đây là một số thuật ngữ phổ biến bạn sẽ gặp khi làm việc với Git và GitHub:- **Cam kết**: Ảnh chụp nhanh các thay đổi của bạn trong kho lưu trữ. Mỗi cam kết bao gồm một thông báo mô tả những gì đã được thay đổi và tạo ra một bản ghi lịch sử mà bạn có thể tham khảo hoặc hoàn nguyên về sau.
- **Kho lưu trữ (Repo)**: Tập hợp các tệp và lịch sử của chúng được Git theo dõi. Kho lưu trữ có thể tồn tại cục bộ trên máy tính của bạn hoặc từ xa trên các nền tảng như GitHub. Mỗi dự án thường được lưu trữ trong kho lưu trữ riêng của nó.
- **Từ xa**: Kho lưu trữ từ xa là phiên bản dự án của bạn được lưu trữ trên nền tảng như GitHub. Phiên bản cục bộ của dự án trên máy tính của bạn được liên kết với điều khiển từ xa này để bạn có thể tải lên (đẩy) và tải xuống (kéo) các thay đổi.
- **Staging**: Quá trình chuẩn bị file cho một commit. Khi bạn sắp xếp một tệp, bạn đang nói với Git rằng bạn muốn đưa nó vào lần xác nhận tiếp theo. Giai đoạn cho phép bạn chọn những thay đổi nào sẽ được đưa vào một cam kết.
- **Đẩy**: Hành động gửi các thay đổi đã cam kết của bạn từ kho lưu trữ cục bộ đến kho lưu trữ từ xa (ví dụ: GitHub), để những người khác có thể truy cập phiên bản cập nhật mã của bạn.
- **Kéo**: Hành động tìm nạp các thay đổi từ kho lưu trữ từ xa để cập nhật bản sao cục bộ của bạn. Bạn lấy các thay đổi khi muốn đồng bộ hóa kho lưu trữ cục bộ của mình với phiên bản mới nhất từ xa.
- **Xuất xứ**: Tên mặc định cho kho lưu trữ từ xa khi bạn kết nối kho lưu trữ cục bộ của mình với điều khiển từ xa lần đầu tiên. Thường đề cập đến URL chính của dự án của bạn trên GitHub.