---
title: "Bọc lệnh"
type: docs
weight: 4
translation_provenance: ai-reviewed
translation_source_sha256: 7b176d9b546b2566812e825fb2e10da5dd4e86f0e79be2c362a4775546110ac6
translation_lock: true
url: "hub/scripting/tutorials/Wrapping/wrapping"
---
Các lệnh Scheme hoạt động ở mức độ thấp, nghĩa là ngay cả những tác vụ đơn giản cũng có thể yêu cầu nhiều bước. Tuy nhiên, mức độ chi tiết này mang lại sự linh hoạt, chúng ta có thể gói các lệnh thành các hàm nhỏ, có thể tái sử dụng để thực hiện chính xác những gì chúng ta cần. Bao bọc không phải là một khái niệm đen trắng; nó có thể bao gồm từ các bí danh đơn giản cho các lệnh được sử dụng thường xuyên đến các hàm phức tạp hơn quản lý toàn bộ quy trình công việc. Đôi khi, trình bao bọc chỉ là một chức năng tiện lợi để cải thiện khả năng đọc, trong khi trong các trường hợp khác, nó phát triển thành một tiện ích đầy đủ tính năng, đóng gói nhiều thao tác.

### Tại sao phải gói chức năng?

Có một số lợi ích chính của chức năng gói:

- **Đơn giản hóa các tác vụ lặp đi lặp lại** – Thay vì lặp lại các lệnh cấp thấp, hãy gói chúng trong một hàm trợ giúp và tái sử dụng nó.
- **Cải thiện khả năng đọc** – Đặt tên mô tả rõ ràng cho các hàm được bao bọc của chúng ta giúp mã của chúng ta dễ hiểu hơn trong nháy mắt.
- **Đóng gói sự phức tạp** – Thay vì xử lý các danh sách lệnh dài, khó hiểu, các vòng lặp được lồng sâu hoặc các câu lệnh thông báo phức tạp, chúng ta có thể chia chúng thành các hàm trợ giúp có cấu trúc tốt, nhỏ hơn.
- **Nâng cao khả năng bảo trì** – Nếu chức năng cốt lõi của lệnh thay đổi, chúng ta chỉ cần cập nhật chức năng được bao bọc của mình một lần, cách ly các plug-in của chúng ta khỏi các chi tiết về những thay đổi đó.
- **Khuyến khích sử dụng lại mã** – Mỗi trình trợ giúp trở thành một phần trong thư viện của bạn, giúp viết và gỡ lỗi các tập lệnh trong tương lai nhanh hơn.

Khi các plug-in của bạn phát triển, trình bao bọc giúp bạn giữ cho logic cốt lõi có thể đọc được và tách biệt các chi tiết lặp lại.

Một ưu điểm khác của việc gói các hàm là tích hợp chúng vào một công cụ đánh dấu cú pháp như Visual Studio Code. Điều này cải thiện khả năng đọc và điều hướng, làm cho tập lệnh rõ ràng hơn. Trong một plug-in sử dụng các hàm tùy chỉnh, mọi hàm được đánh dấu màu xanh lục đều xác nhận rằng hàm đó được tham chiếu chính xác từ thư viện của chúng ta.

Nếu bạn duy trì thư viện trợ giúp của riêng mình, hãy cân nhắc việc thêm tên hàm của dự án vào phần tô sáng cú pháp của trình soạn thảo. Nó làm cho việc điều hướng và tái cấu trúc nhanh hơn.

Ví dụ:

### Seed ngẫu nhiên

```scheme
;; Mục đích: Trả về số nguyên ngẫu nhiên để khởi tạo seed cho bộ lọc
(define (random-seed)
  (msrg-rand))
```

Mặc dù chúng ta có thể sử dụng ***msrg-Rand*** trực tiếp trong mã của mình nhưng việc gói nó bên trong một hàm có tên ***random-seed*** sẽ cải thiện khả năng đọc. Bằng cách đặt cho hàm một cái tên rõ ràng và mang tính mô tả, bạn sẽ dễ dàng hiểu được mục đích của nó ngay lập tức.

Ngoài ra, việc xác định ***random-seed*** là một hàm độc lập cho phép chúng ta sử dụng nó ở bất kỳ đâu trong các plugin của mình trong khi tập trung việc triển khai ở một vị trí duy nhất. Nếu cần thay đổi cách tạo hạt giống, chúng ta chỉ cần cập nhật hàm này, giữ nguyên phần còn lại của mã.

Ví dụ: nếu chúng ta quyết định chuyển sang ***random*** thay thế:

```scheme
;; Mục đích: Trả về số nguyên ngẫu nhiên để khởi tạo seed cho bộ lọc
(define (random-seed)
  (random 1000))
```

Tên hàm vẫn được giữ nguyên, đảm bảo rằng tập lệnh của chúng ta tiếp tục hoạt động mà không cần sửa đổi. Cách tiếp cận này giữ cho mã của chúng ta linh hoạt, có thể bảo trì và dễ đọc.

### Xuất JPEG

Chức năng xuất JPEG trong Scheme đi kèm với nhiều tham số, cung cấp khả năng kiểm soát tốt cách lưu hình ảnh. Tuy nhiên, trong hầu hết các trường hợp, chúng ta chỉ quan tâm đến một số cài đặt chính, chẳng hạn như tên tệp và chất lượng. Để đơn giản hóa quy trình, chúng ta có thể gói hàm.

```scheme
;; Mục đích: Lưu hình ảnh dạng JPEG với chất lượng chỉ định
(define (file-jpg-save image file quality)
  (let ((export-file (if (has-substring? file ".jpg")
                         file
                         (string-append file ".jpg")))) ;; Tránh jpg.jpg
    (debug-message "Exporting: " export-file)
    (file-jpeg-export #:run-mode RUN-NONINTERACTIVE
                      #:image image
                      #:file export-file
                      #:options -1
                      #:quality (* 0.01 quality)
                      #:smoothing 0.0
                      #:optimize 1
                      #:progressive 1
                      #:cmyk 0
                      #:sub-sampling "sub-sampling-1x1"
                      #:baseline 1
                      #:restart 0
                      #:dct "integer")))
```

Trong hàm bao bọc này, hầu hết các tùy chọn xuất đều được mã hóa cứng, chỉ hiển thị các tham số mà chúng ta có thể điều chỉnh: tên tệp và chất lượng. Cách tiếp cận này cải thiện khả năng đọc và làm cho việc lưu hình ảnh trở nên đơn giản hơn. Ngoài ra, nếu trình xuất của Lumi thay đổi trong tương lai, chúng ta chỉ cần cập nhật một chức năng này thay vì sửa đổi mọi tập lệnh xuất JPEG.

### Sử dụng Trình bao bọc

Để xuất JPEG trong các plugin của chúng ta, chúng ta chỉ cần đưa thư viện vào và gọi hàm tùy chỉnh của mình:

```scheme
(file-jpg-save image "/home/mark/pictures/my-picture" 85)
```

Điều này giúp mã của chúng ta sạch sẽ, dễ đọc và dễ điều chỉnh đồng thời cho phép chúng ta xuất ảnh JPEG một cách hiệu quả mà không tốn nhiều công sức.

### Thay thế `car`

Hàm ***car*** có thể khó hiểu và dễ mắc lỗi tập lệnh. Rất dễ áp dụng nhầm ***car*** cho một vector hoặc một mục không có trong danh sách, dẫn đến hành vi không mong muốn. Để làm cho mã của chúng ta mạnh mẽ hơn và dễ đọc hơn, chúng ta có thể gói chức năng này trong một chức năng an toàn hơn.

```scheme
;; Mục đích: Trả về phần tử đầu tiên của danh sách hoặc vector.
;;          Cảnh báo nếu đầu vào không hợp lệ hoặc rỗng.
(define (first-item collection)
  (cond
    ;; Xử lý danh sách không rỗng
    ((and (list? collection) (not (null? collection)))
     (list-ref collection 0))
    ;; Xử lý vector không rỗng
    ((and (vector? collection) (> (vector-length collection) 0))
     (vector-ref collection 0))
    ;; Đầu vào không hợp lệ hoặc rỗng
    (else
     (begin
       (warning-message "first-item: Expected a non-empty list or vector, but received: " collection)
       #f))))
```

Hàm này truy xuất mục đầu tiên của danh sách hoặc vector một cách an toàn đồng thời đưa ra các cảnh báo hữu ích khi gặp phải dữ liệu đầu vào không hợp lệ hoặc trống. Bằng cách sử dụng ***first-item*** thay vì ***car***, chúng ta giảm nguy cơ xảy ra lỗi vô tình và cải thiện độ rõ ràng của tập lệnh.

#### Tại sao nên sử dụng trình bao bọc này?

- **Ngăn chặn sự cố tập lệnh** – Tránh các lỗi gây ra do áp dụng ***car*** cho các mục không có trong danh sách.
- **Hỗ trợ cả danh sách và vector** – Mở rộng khả năng sử dụng ngoài danh sách.
- **Cung cấp các cảnh báo có ý nghĩa** – Giúp gỡ lỗi các sự cố đầu vào không mong muốn.
- **Cải thiện khả năng đọc** – Tên hàm truyền tải rõ ràng mục đích của nó.

Bằng cách gói gọn logic này trong mục đầu tiên, chúng ta làm cho các plug-in của mình trở nên mạnh mẽ hơn và dễ bảo trì hơn. Tất nhiên, điều này tùy thuộc vào sở thích cá nhân, bạn có thể hoàn toàn thoải mái khi sử dụng trực tiếp các chức năng `car`, `cadr`, `caddr` và các chương trình tương tự.

### Gói một hàm được gói

Việc gói một hàm đã được gói sẵn có thể cải thiện hơn nữa khả năng đọc và bảo trì. Ví dụ: khi làm việc với các cặp tọa độ như ***pixel-coords (danh sách 100 200)***, chúng ta có thể sử dụng:

```scheme
(first-item pixel-coords)
```

để truy xuất tọa độ ****x***. Tuy nhiên, trong khi có chức năng, điều này không mang tính biểu cảm cho lắm. Thay vào đó, chúng ta có thể gói ***first-item*** theo một định nghĩa phù hợp hơn để làm cho ý định của chúng ta rõ ràng hơn.

```scheme
;; Mục đích: Trả về tọa độ x, để dễ đọc
(define (x-coord pixel-coords)
  (first-item pixel-coords))

;; Mục đích: Trả về tọa độ y, để dễ đọc
(define (y-coord pixel-coords)
  (second-item pixel-coords))
```

### Tại sao nên sử dụng phương pháp này?

- **Nâng cao độ rõ ràng của mã** – Thay vì sử dụng các hàm truy cập danh sách chung chung, chúng ta xác định rõ ràng các hàm mô tả mục đích của chúng.
- **Cải thiện khả năng bảo trì** – Nếu biểu diễn tọa độ của chúng ta thay đổi (ví dụ: sử dụng vector thay vì danh sách), chúng ta chỉ cần cập nhật các hàm nhỏ này.
- **Khuyến khích tính nhất quán** – Sử dụng ***x-coord*** và ***y-coord*** giúp tập lệnh dễ đọc và dễ hiểu hơn trong nháy mắt.

Bây giờ, thay vì viết bằng Scheme nói chung:

```scheme
(car pixel-coords) ;; Lấy tọa độ x
(cadr pixel-coords) ;; Lấy tọa độ y
```

Chúng ta có thể viết trong sơ đồ _our_:

```scheme
(x-coord pixel-coords)
(y-coord pixel-coords)
```

Bằng cách gói các hàm cấp thấp vào các tên có ý nghĩa, chúng ta tạo ra cách làm việc với dữ liệu trực quan hơn, giảm sự nhầm lẫn và các lỗi tiềm ẩn.

### Trình bao bọc được vận chuyển: Stdlib tiện ích

Lumi gửi một bộ trình bao bọc làm sẵn được tải tự động khi khởi động, vì vậy chúng có sẵn trong bất kỳ plug-in nào hoặc trong Bảng điều khiển Scheme mà không cần bất kỳ lệnh gọi `(load ...)` nào. Các thư viện này (`common.scm`, `files.scm`, `gegl.scm`, `images.scm`, `layers.scm`, `parasites.scm` và `paths.scm`) được xây dựng theo nguyên tắc giống hệt như các ví dụ trên: chúng đặt tên rõ ràng cho các hoạt động cấp thấp, ẩn bản soạn sẵn lặp đi lặp lại và cung cấp một nơi duy nhất để cập nhật nếu lệnh cơ bản thay đổi. Ví dụ: `images.scm` cung cấp `image-get-open-list` dưới dạng trình bao bọc có thể đọc được xung quanh lệnh gọi PDB thô và `files.scm` hiển thị các trình trợ giúp xây dựng đường dẫn mà nếu không sẽ yêu cầu các chuỗi `string-append` lặp lại.

Bạn có thể duyệt mọi tên đã xuất, đọc chuỗi tài liệu của nó và xem nó đến từ thư viện nào trong **[Trình duyệt tiện ích]({{< ref "/hub/scripting/reference/utility-browser" >}})** (Trợ giúp → Lập trình → Trình duyệt tiện ích). Đây là minh chứng thực tế về việc gói theo quy mô lớn và là nguồn mẫu hữu ích để mượn khi xây dựng thư viện trợ giúp của riêng bạn.

### Kết luận

Chức năng gói là một cách mạnh mẽ để đơn giản hóa việc phát triển Scheme, làm cho các tập lệnh dễ đọc hơn, dễ bảo trì hơn và mạnh mẽ hơn. Bằng cách gói gọn sự phức tạp và chỉ hiển thị những chi tiết cần thiết, chúng ta tạo ra một cách tiếp cận có cấu trúc hơn để viết các plug-in.

Những điểm chính rút ra từ phương pháp này:

- **Đơn giản hóa các tác vụ lặp đi lặp lại** – Thay vì lặp lại các lệnh cấp thấp theo cách thủ công, chúng ta tạo ra các hàm có thể sử dụng lại.
- **Cải thiện khả năng đọc mã** – Trình bao bọc được đặt tên phù hợp giúp tập lệnh dễ hiểu hơn.
- **Đóng gói sự phức tạp** – Các chi tiết cấp thấp được xử lý bên trong trình bao bọc, giữ cho tập lệnh chính luôn rõ ràng.
- **Nâng cao khả năng bảo trì** – Nếu chức năng cốt lõi thay đổi, chúng ta chỉ cần cập nhật trình bao bọc chứ không phải mọi tập lệnh dựa trên đó.
- **Khuyến khích tái sử dụng và nhất quán** – Thư viện chức năng cá nhân của chúng ta phát triển theo thời gian, giúp quá trình phát triển nhanh hơn và hiệu quả hơn.

Bằng cách sử dụng gói chức năng một cách nhất quán, chúng ta có thể biến đổi cách chúng ta viết các plug-in Scheme, tạo ra một môi trường tập lệnh mang tính mô-đun và biểu cảm hơn. Với những nguyên tắc này, chúng ta có thể tiếp tục cải tiến cách tiếp cận của mình, phát triển một bản Scheme phù hợp và hiệu quả hơn, đáp ứng các nhu cầu cụ thể của chúng ta.

Các bước tiếp theo: xác định các khối lặp lại trong tập lệnh của bạn và trích xuất các trợ giúp nhỏ có tên rõ ràng.