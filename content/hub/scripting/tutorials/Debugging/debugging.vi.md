---
title: "Gỡ lỗi"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_source_sha256: bd5eaf8ed491a7a74b7e4bcd130ed5177cfb15be41526bb6aefdfa0fb2a2428f
url: "hub/scripting/tutorials/debugging"
translation_lock: true
---
Trong kịch bản, không có chức năng nào là không thể sai lầm. Ngay cả những lệnh đáng tin cậy nhất cũng có thể bị lỗi khi gặp phải các đầu vào hoặc điều kiện không mong muốn. Để bảo vệ khỏi điều này, chúng ta có thể triển khai hệ thống gỡ lỗi tùy chỉnh và áp dụng các kỹ thuật lập trình phòng thủ. Bằng cách kết hợp các hàm tiêu chuẩn với cơ chế xử lý lỗi và cung cấp phản hồi mang tính thông tin, chúng ta có thể làm cho tập lệnh của mình mạnh mẽ hơn và dễ khắc phục sự cố hơn.

Một phần quan trọng của chiến lược này là sử dụng cờ gỡ lỗi chung để kiểm soát đầu ra dài dòng, cho phép chúng ta kích hoạt thông tin gỡ lỗi chi tiết khi cần trong khi vẫn giữ đầu ra sạch sẽ trong quá trình thực thi bình thường.

## Cờ gỡ lỗi toàn cầu

Cờ gỡ lỗi toàn cục là một cách đơn giản nhưng hiệu quả để kiểm soát mức độ đầu ra thông tin trong quá trình thực thi tập lệnh. Khi được bật, nó sẽ cung cấp các thông báo gỡ lỗi chi tiết có thể có giá trị cho việc theo dõi các vấn đề. Khi bị tắt, nó sẽ giữ kết quả đầu ra ngắn gọn để sử dụng trong sản xuất.

```scheme
;; Mục đích: Cờ toàn cục điều khiển đầu ra gỡ lỗi.
(define debug #f)
```

Theo mặc định, tính năng gỡ lỗi bị tắt. Để bật đầu ra dài dòng trong quá trình phát triển, chỉ cần đặt cờ thành `#t`:

```scheme
;; Mục đích: Cờ toàn cục điều khiển đầu ra gỡ lỗi.
(define debug #t)
```

Chúng ta cũng có thể tạm thời bật hoặc tắt tính năng gỡ lỗi cho các phần mã cụ thể bằng cách sử dụng các hàm trợ giúp.

### Kiểm soát gỡ lỗi cục bộ

Để kiểm soát tốt hơn, chúng ta có thể bật hoặc tắt tính năng gỡ lỗi trong các phần cụ thể của tập lệnh bằng cách sử dụng các hàm trợ giúp.

```scheme
;; Mục đích: Tắt chế độ gỡ lỗi cho một đoạn mã.
(define (debug-off)
  (set! debug #f))

;; Mục đích: Bật chế độ gỡ lỗi cho một đoạn mã.
(define (debug-on)
  (set! debug #t))
```

Điều này cho phép chúng ta kiểm soát việc gỡ lỗi một cách linh hoạt:

```scheme
(debug-on)  ;; Bật đầu ra chi tiết

;; Một số logic script ở đây

(debug-off) ;; Tắt đầu ra chi tiết
```

## Gỡ lỗi hệ thống nhắn tin

Để xử lý hiệu quả kết quả gỡ lỗi trong Scheme, chúng ta sử dụng phương pháp tiếp cận có cấu trúc bao gồm nhiều hàm trợ giúp. Các chức năng này đảm bảo rằng các thông báo gỡ lỗi và cảnh báo rõ ràng, dễ đọc và có thể bảo trì.

### Tổng quan về Hệ thống nhắn tin gỡ lỗi

Hệ thống nhắn tin gỡ lỗi của chúng ta bao gồm các thành phần sau:

1. `debug-message` – Hiển thị thông báo gỡ lỗi khi bật tính năng gỡ lỗi.
2. `serialize-item` – Chuyển đổi các loại dữ liệu Scheme khác nhau thành dạng biểu diễn chuỗi.
3. `concat` – Ghép nhiều mục thành một chuỗi duy nhất.
4. `list->string` – Định dạng danh sách thành chuỗi có thể đọc được.
5. `message` – Hiển thị đầu ra trong bảng điều khiển tin nhắn của Lumi.
6. `warning-message` – Hiển thị thông báo cảnh báo khi cảnh báo được bật.

Mỗi chức năng đóng một vai trò trong việc định dạng và hiển thị các thông báo có cấu trúc.

---

### Chức năng thông báo gỡ lỗi

Hàm `debug-message` là phương thức cốt lõi để hiển thị kết quả gỡ lỗi. Nó đảm bảo thông báo chỉ được hiển thị khi bật tính năng gỡ lỗi.

```scheme
;; Mục đích: Hiển thị message gỡ lỗi.
(define (debug-message . items)
  (when debug (message "> " (apply concat items))))
```

- Điều kiện `when debug` đảm bảo thông báo chỉ xuất hiện khi bật tính năng gỡ lỗi.
- Tin nhắn có tiền tố `"> "` để rõ ràng.
- Hàm sử dụng `concat` để định dạng nội dung tin nhắn.
- Cuối cùng nó gọi `message` để gửi đầu ra tới bảng điều khiển tin nhắn của Lumi.

Cách sử dụng ví dụ:

```scheme
;; Mục đích: Trả về vị trí cây của phần tử hoặc #f nếu không hợp lệ
(define (get-item-tree-position image item)
  (if (item-is-valid? item)
    (let ((position (list->item (lumi-image-get-item-position image item))))
      (debug-message "item : " (item-get-name item) " has tree position : " position)
      position)
    #f))
```

Khi bật tính năng gỡ lỗi, đầu ra có thể là:

```scheme
> item: background-layer has tree position : 3
```

### Tuần tự hóa dữ liệu cho thông báo gỡ lỗi

Tin nhắn có thể chứa các loại dữ liệu khác nhau như danh sách, vector và số. Để đảm bảo chúng được định dạng chính xác, chúng ta sử dụng `serialize-item`.

```scheme
;; Mục đích: Chuyển đổi các kiểu dữ liệu Scheme (danh sách, vector, cặp, v.v.)
;;          thành biểu diễn chuỗi.
(define (serialize-item item)
  (cond
    ((and (list? item) (null? item)) "\"\"")          ; Danh sách rỗng
    ((and (string? item) (string=? item "")) "\"\"")  ; Chuỗi rỗng
    ((list? item) (list->string item))                ; Danh sách lồng nhau
    ((vector? item)                                   ; Xử lý vector
     (string-append "#("
                    (string-join (map serialize-item (vector->list item)) " ")
                    ")"))
    ((pair? item)                                     ; Xử lý cặp
     (string-append "("
                    (serialize-item (car item))
                    " . "
                    (serialize-item (cdr item))
                    ")"))
    ((number? item) (number->string item))            ; Số
    ((symbol? item) (symbol->string item))            ; Ký hiệu
    ((boolean? item) (if item "#t" "#f"))             ; Giá trị boolean
    ((string? item) item)                             ; Chuỗi
    (else (warning-message "serialize-item: Unsupported item type!" item))))
```

Cách sử dụng ví dụ:

```scheme
(serialize-item '(1 2 3))
```

Đầu ra:

```scheme
list:
1
2
3
```

### Nối tin nhắn

Để hợp nhất nhiều thành phần thông báo thành một chuỗi duy nhất, chúng ta sử dụng `concat`.

```scheme
;; Mục đích: Nối nhiều phần tử thành một chuỗi.
(define (concat . items)
  (apply string-append (map serialize-item items)))
```

Cách sử dụng ví dụ:

```scheme
(concat "Image size: " 1920 "x" 1080)
```

### Định dạng danh sách dưới dạng chuỗi

Hàm `list->string` chuyển đổi danh sách thành một chuỗi được định dạng.

```scheme
;; Mục đích: Chuyển danh sách phần tử thành chuỗi dễ đọc.
(define (list->string list)
  (if (list? list)
      (string-append "list: \n" (string-join (map serialize-item list) "\n"))
      (warning-message "list->string: Input is not a list!")))
```

### Thông báo cảnh báo

Hàm `warning-message` hoạt động tương tự như `debug-message`, nhưng nó hiển thị cảnh báo ngay cả khi tính năng gỡ lỗi bị tắt.

```scheme
;; Mục đích: Hiển thị message cảnh báo.
(define (warning-message . items)
  (if warning
    (message "Warning: " (apply concat items)))
    #f)
```

- Đảm bảo thông báo chỉ được hiển thị khi cảnh báo được bật (cờ `warning` được đặt trong `common.scm` là `#t`).
- Gọi `concat` để định dạng nội dung tin nhắn.
- Sử dụng `message` để gửi đầu ra cho Lumi.

## Tăng cường chức năng tiêu chuẩn

Sau khi có hệ thống gỡ lỗi, chúng ta có thể nâng cao thư viện chức năng của mình bằng cách kết hợp các thông báo chi tiết. Điều này cung cấp cái nhìn sâu sắc về trạng thái mục, giá trị biến và lệnh gọi hàm.

Một ví dụ phổ biến là `item-is-valid?`, bao bọc `lumi-item-id-is-valid` để trả về `#t` hoặc `#f`. Nếu `#f` được trả về, chúng ta có thể kích hoạt `warning-message` trong mã cuộc gọi, nếu đầu vào không phải là số thì chúng ta có thể đưa ra cảnh báo trong hàm.

```scheme
;; Mục đích: Kiểm tra phần tử hợp lệ, trả về #t hoặc #f.
;;          Cảnh báo nếu phần tử không phải số.
(define (item-is-valid? item)
  (if (number? item)
      (= (list->item (lumi-item-id-is-valid item)) 1)
      (begin
        (warning-message "item-is-valid?: Expected a number, but received: " item)
        #f)))
```

## Cách sử dụng thực tế

Khi phát triển các plug-in Scheme, việc gói các hàm theo cách này giúp giảm đáng kể thời gian gỡ lỗi và đảm bảo mã mạnh mẽ, có thể bảo trì. Với hệ thống gỡ lỗi đã sẵn sàng, chúng ta có thể tạo luồng gỡ lỗi có cấu trúc trong bảng điều khiển lỗi chỉ bằng một cú nhấn nút chuyển.

Trong luồng gỡ lỗi này, các lệnh gọi hàm được đánh dấu bằng dấu hoa thị (*), giúp việc theo dõi việc thực thi tập lệnh và xác định lỗi dễ dàng hơn, đặc biệt là trong các plug-in phức tạp. Khả năng hiển thị này giúp chúng ta hiểu được quy trình hoạt động và chẩn đoán các hành vi không mong muốn một cách hiệu quả.

Trình bao bọc cho chức năng thông báo của chúng ta sử dụng `*`

```scheme
(define (call . items)
  (when debug (message "* (" (apply concat items) ")")))
```

Ví dụ về `call` được sử dụng trong thực tế:

```scheme
;; Mục đích: Áp dụng quy trình tạo kết cấu cho danh sách mặt nạ nhóm
(define (process-masks groups pattern) (call 'process-masks)
  (for-each
    (lambda (group)
      (let ((mask (add-mask-to-layer group ADD-MASK-WHITE)))
        (message "Process mask : " (item-get-name group))
        (fill-and-adjust-group-mask group mask pattern)
        (lumi-layer-set-opacity group (get 'color-opacity))
        (lumi-item-set-expanded (item-get-parent group) 0)
        (lumi-selection-none (get-image))))
    (ensure-list groups)))
```

Ví dụ về luồng gỡ lỗi khi plug-in thực thi:

```scheme
> Recording the plug-in settings
* (convert-gui-settings)
> all-masks : 1
> strokes : 1
> color : 1
> plate-layer : 1
> drawables : #(37)
* (filter-list-for-matching-groups)
> all-masks : #t
> sub-groups of group : root
blue
blue_strokes
blue_colour
yellow
yellow_strokes
yellow_colour
gray
gray_strokes
gray_colour
> groups with identifier in name: _colour
blue_colour
yellow_colour
gray_colour
* (filter-list-for-matching-groups)
> all-masks : #t
> sub-groups of group : root
blue
blue_strokes
blue_colour
yellow
yellow_strokes
yellow_colour
gray
gray_strokes
gray_colour
> groups with identifier in name: _strokes
blue_strokes
yellow_strokes
gray_strokes
* (begin-apply-texture)

Start Apply Texture

> color : #t

Texturing color group masks
> color-pattern : 2655
* (process-masks)
Process mask : blue_colour
* (fill-and-adjust-group-mask)
> Fill-and-adjust : blue_colour mask
> using pattern for fill : 2655
* (apply-color-effect)
> color-contrast : 64
> color-levels-gamma : 10
> levels on drawable: blue_colour mask
>   gamma: 8.2
>   low-in: 0.7278  high-in: 0.9222
>   low-out: 0  high-out: 1
> light-opacity : 6
> light-opacity : 6
* (apply-light-effect)
> apply-light-effect opacity : 6
> from layer : light_blue
> edit-copy light_blue
> edit-paste blue_colour mask
> shade-opacity : 60
> shade-opacity : 60
* (apply-light-effect)
> apply-light-effect opacity : 60
> from layer : shad_blue_opa*5
> edit-copy shad_blue_opa*5
> edit-paste blue_colour mask
* (apply-opaque-effect)
> children in : blue_colour
blue_colour
hue_blue
light_blue
shad_blue_opa*5
base_blue
...
...
...
Finished Apply Texture!
```

Nhật ký có cấu trúc này cung cấp dòng thời gian rõ ràng về các lệnh gọi hàm và thay đổi dữ liệu, giúp việc gỡ lỗi và phân tích hiệu suất trở nên dễ dàng hơn đáng kể.

## Kết luận

Bằng cách triển khai hệ thống gỡ lỗi có cấu trúc, chúng ta tạo ra các tập lệnh an toàn hơn, dễ bảo trì hơn, cung cấp thông tin chi tiết theo thời gian thực về quá trình thực thi chúng.

### Bài học chính

- **Kiểm soát mức độ chi tiết** – Sử dụng cờ gỡ lỗi chung để quản lý mức đầu ra.
- **Cung cấp phản hồi rõ ràng** – Bao bọc các chức năng tiêu chuẩn bằng các thông báo gỡ lỗi đầy thông tin.
- **Nâng cao tính chắc chắn** – Xử lý các thông tin đầu vào không mong muốn một cách khéo léo để ngăn ngừa lỗi.
- **Đơn giản hóa việc khắc phục sự cố** – Thông báo gỡ lỗi có cấu trúc giúp chẩn đoán và khắc phục sự cố dễ dàng hơn.

Với phương pháp này, các tập lệnh của chúng ta "tự giải thích" một cách hiệu quả khi xử lý dữ liệu, giảm bớt sự thất vọng và cải thiện hiệu quả quy trình làm việc. Việc gỡ lỗi trở thành một công cụ chủ động thay vì một công việc mang tính phản ứng, làm cho quá trình viết kịch bản của chúng ta trở nên mượt mà hơn và bổ ích hơn.