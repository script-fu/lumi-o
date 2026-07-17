---
title: "Sử dụng Git trên Linux"
type: docs
translation_provenance: ai-reviewed
translation_source_sha256: 7054a9ff9efeb93b4f494197e09ed1fe34d5d6bde7bc305480693c3982d375ae
url: "hub/technical-guides/Using-Git-on-Linux"
translation_lock: true
---
Chào mừng bạn đến với hướng dẫn sử dụng Git trên Linux dành cho người mới bắt đầu! Hướng dẫn này được thiết kế để giúp bạn bắt đầu với Git và GitLab, đồng thời cung cấp hiểu biết cơ bản về cách sử dụng các công cụ này.

## Tổng quan về Git

Mã được sử dụng để tạo ứng dụng được lưu giữ trong một tập hợp các thư mục và tệp trên hệ thống của bạn. Git là một ứng dụng cho phép chúng ta sao lưu, chia sẻ và sao chép bộ sưu tập đó. Git được biết đến như một hệ thống kiểm soát phiên bản cho phép bạn theo dõi các thay đổi đối với mã của mình và cộng tác với những người khác. Đó là một công cụ mạnh mẽ được sử dụng rộng rãi trong cộng đồng nguồn mở. GitLab là một nền tảng dựa trên web cho phép bạn lưu trữ và quản lý kho lưu trữ Git trực tuyến, giúp bạn dễ dàng cộng tác với những người khác và theo dõi các thay đổi đối với mã của mình.

## Kho lưu trữ là gì?

_repo_, viết tắt của repository, là thư mục cục bộ do Git quản lý với bản sao trực tuyến. Kho GitLab là tập hợp tệp và thư mục tạo nên dự án. Kho có thể có _branch_ — bản sao độc lập của cùng dự án. Nhánh là phiên bản riêng cho phép thay đổi mà không ảnh hưởng phiên bản chính — hữu ích khi thử tính năng mới hoặc sửa lỗi mà không làm gián đoạn dự án chính. Có kho cục bộ trên ổ cứng và kho remote trực tuyến qua Git và GitLab.

## Sử dụng Git

Bạn sẽ cần cài đặt Git trên hệ thống của mình. Trên các hệ thống dựa trên Debian, bạn có thể sử dụng lệnh apt để cài đặt các gói phần mềm. Trong trường hợp này, lệnh này dùng để cài đặt Git — gói cung cấp hệ thống kiểm soát phiên bản Git. Lệnh sudo cấp cho trình cài đặt quyền cài đặt trên hệ thống của bạn.

```bash
 sudo apt install git
```

## Truy cập GitLab

Trước khi có thể sử dụng [GitLab](https://gitlab.com/users/sign_up), bạn cần tạo một tài khoản bằng cách truy cập trang web GitLab và hoàn tất quy trình đăng ký.

GitLab yêu cầu _SSH_ để liên lạc an toàn và được xác thực giữa máy khách (ví dụ: bạn) và máy chủ GitLab khi thực hiện các thao tác Git như kho lưu trữ _cloning_, _pushing_ và _fetching_. Nhân bản là tạo một bản sao cục bộ của kho lưu trữ, tìm nạp là đưa mọi thay đổi được thực hiện trong kho lưu trữ sang bản sao cục bộ của bạn và đẩy là gửi các thay đổi và nội dung đến kho lưu trữ của máy chủ. SSH (Secure Shell) là giao thức mạng cho phép truy cập từ xa an toàn và sử dụng _key pair_ để xác thực và thiết lập kết nối an toàn. Để tạo cặp khóa SSH, bạn có thể sử dụng lệnh ssh-keygen trong thiết bị đầu cuối của mình.

```bash
 ssh-keygen
```

Chỉ định tên tệp hoặc sử dụng tên mặc định bằng cách nhấn Enter và tùy chọn mật khẩu. Trong thư mục chính của bạn, trong một thư mục ẩn có tên .ssh, hiện có hai tệp id_rsa, nếu bạn sử dụng tên mặc định. Tệp .pub là khóa chung và bạn có thể xem nội dung của nó bằng trình soạn thảo văn bản.

Đăng nhập vào tài khoản GitLab của bạn và điều hướng đến cài đặt người dùng của bạn. Nhấp vào 'Khóa SSH' trong menu điều hướng bên trái. Sao chép và dán khóa công khai của bạn vào trường Khóa và đặt tiêu đề phù hợp cho khóa, chẳng hạn như PC@Home. Nhấp vào nút 'Thêm khóa' để lưu khóa. Khóa công khai SSH của bạn hiện đã được thêm vào tài khoản GitLab của bạn và bạn có thể sử dụng nó để xác thực bằng kho lưu trữ GitLab. Kiểm tra xem khóa và kết nối của bạn có hoạt động với lệnh ssh -T hay không để xem thông báo chào mừng từ GitLab.

```bash
 $ ssh -T git@ssh.gitlab.gnome.org
 Welcome to GitLab, @username!
```

## Các lệnh Git cơ bản

Sau khi cài Git và thiết lập khóa SSH với GitLab, hãy xem qua một số lệnh cần thiết để quản lý kho.

### 1. **Clone kho**

Clone là tạo bản sao cục bộ của kho remote. Hữu ích khi bạn muốn làm việc trên dự án đã có trên GitLab. Dùng lệnh `git clone` theo sau URL kho:

```sh
git clone https://gitlab.com/username/repository.git
```

Thay thế `https://gitlab.com/username/repository.git` bằng URL của kho lưu trữ mà bạn muốn sao chép. Lệnh này sẽ tạo một bản sao cục bộ của kho lưu trữ trong một thư mục mới.

### 2. **Kiểm tra trạng thái kho lưu trữ**

Để xem kho lưu trữ cục bộ của bạn có bất kỳ thay đổi nào hoặc để xem trạng thái hiện tại của nó hay không, hãy sử dụng:

```sh
git status
```

Lệnh này sẽ cho bạn biết những tập tin nào đã được sửa đổi, thêm hoặc xóa trong bản sao kho lưu trữ cục bộ của bạn.

### 3. **Kho lưu trữ từ xa**

Kho lưu trữ từ xa là phiên bản dự án của bạn được lưu trữ trực tuyến, chẳng hạn như trên GitLab. Chúng đóng vai trò là vị trí trung tâm nơi mã của bạn được lưu trữ và những người khác có thể truy cập. Kho lưu trữ từ xa mặc định mà Git tạo khi bạn sao chép một dự án có tên là `origin`. Bạn có thể thêm, xóa hoặc liệt kê các kho lưu trữ từ xa bằng các lệnh sau:

- **Liệt kê remote:**

  Để xem kho lưu trữ từ xa nào được liên kết với dự án cục bộ của bạn, hãy sử dụng:

  ```sh
  git remote -v
  ```

  Lệnh này liệt kê tất cả các điều khiển từ xa và URL của chúng. Thông thường, bạn sẽ thấy `origin` được liệt kê ở đây.

- **Thêm remote:**

  Nếu bạn cần thêm kho lưu trữ từ xa mới, bạn có thể làm như vậy với:

  ```sh
  git remote add <name> <url>
  ```

  Thay `<name>` bằng tên remote và `<url>` bằng URL kho.

- **Xóa remote:**

  Để xóa kho lưu trữ từ xa, hãy sử dụng:

  ```sh
  git remote remove <name>
  ```

  Thay `<name>` bằng tên remote cần xóa.

### 4. **Fetch thay đổi từ kho remote**

Nếu bạn muốn xem những thay đổi nào đã được thực hiện đối với kho lưu trữ từ xa mà không áp dụng chúng cho bản sao cục bộ của bạn, hãy sử dụng:

```sh
git fetch origin
```

Lệnh này fetch thay đổi mới nhất từ remote nhưng không merge vào nhánh cục bộ — cách kiểm tra cập nhật trước khi quyết định gộp.

### 5. **Đặt lại kho lưu trữ cục bộ của bạn**

Nếu bạn muốn reset kho cục bộ cho khớp chính xác với remote, dùng reset `hard`. **Cảnh báo:** Thao tác này ghi đè mọi thay đổi cục bộ.

```sh
git reset --hard origin/branch-name
```

Thay thế `branch-name` bằng tên của nhánh bạn muốn đặt lại. Lệnh này sẽ loại bỏ mọi thay đổi cục bộ và làm cho kho lưu trữ cục bộ của bạn giống hệt với kho lưu trữ từ xa.

### 6. **Xem lịch sử commit**

Để xem danh sách các thay đổi được thực hiện đối với kho lưu trữ theo thời gian, hãy sử dụng:

```sh
git log
```

Lệnh này hiển thị lịch sử commit, gồm tác giả, ngày và thông điệp cho mỗi thay đổi.

### Tóm tắt

Các lệnh Git cơ bản này giúp bạn làm việc với kho, giữ bản sao cục bộ cập nhật và quản lý remote an toàn. Clone, kiểm tra trạng thái và quản lý remote là kỹ năng cốt lõi khi dùng Git.