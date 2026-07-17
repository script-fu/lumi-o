---
title: "Bộ lọc"
type: docs
url: "hub/features/filters"
translation_source_sha256: 312088430d35761f6df789821c1629c829e6eb1d2f8b4be58c5843c893c3c7ed
translation_provenance: ai-reviewed
translation_lock: true
---

Menu Bộ lọc của Lumi gom các điều chỉnh hiệu chỉnh, hiệu ứng ống kính cách điệu, trình tạo kết cấu thủ tục, xử lý theo phong cách in ấn và công cụ phân tích vào một chỗ. Thứ tự menu hướng thực hành hơn là học thuật: công cụ làm mờ và cải thiện đặt cạnh nhau, hiệu ứng biến dạng và ánh sáng nhóm theo phong cách, còn trình tạo họa tiết hoặc kết cấu gom lại khi mục tiêu là tạo nguồn liệu chứ không phải chỉnh sửa ảnh có sẵn.

Các hộp thoại bộ lọc dùng chung một quy trình. Cài đặt sẵn, xem trước, chế độ chia đôi và điều khiển độ mờ hay pha trộn giúp tinh chỉnh hiệu ứng nhanh; trên lớp, kết quả có thể giữ dưới dạng bộ lọc không phá hủy có thể chỉnh sửa thay vì hợp nhất ngay. Lumi cũng lưu lịch sử bộ lọc gần đây, nên lặp lại hiệu ứng vừa dùng hoặc mở lại hộp thoại cuối trở thành phần nhịp vẽ thông thường, không phải thao tác riêng.

## Làm mờ

### Gaussian Blur

Gaussian Blur là bộ lọc làm mềm tiêu chuẩn của Lumi: mờ đều, sạch với điều khiển kích thước ngang và dọc riêng, xử lý cạnh và tùy chọn kernel. Đây là lựa chọn đa dụng cho tiêu điểm mềm, mặt nạ làm mềm, chiều sâu khí quyển và mọi quy trình cần độ mờ trung tính.

### Pixelize

Pixelize giảm chi tiết thành khối có chủ ý thay vì mờ mềm. Hộp thoại cho phép chỉnh chiều rộng và cao khối, độ lệch, hình pixel và cách lấp — vừa là hiệu ứng che thô, vừa là khảm hoặc xử lý đồ họa độ phân giải thấp có thể kiểm soát.

### Selective Gaussian Blur

Selective Gaussian Blur làm mềm trong vùng chọn nhưng cố giữ cạnh mạnh. Hữu ích khi ảnh cần kết cấu êm hơn hoặc giảm nhiễu chi tiết mà không mất đường biên hình lớn vẫn cần đọc rõ.

### Lens Blur

Lens Blur là một trong các bộ lọc mờ hướng minh họa của Lumi. Điều khiển xoay quanh khẩu độ đa giác, độ cong cánh, giãn anamorphic, tăng vùng sáng và vùng nét có thể cấu hình — ít giống công cụ làm mềm chung mà gần với độ sâu trường ảnh cách điệu với bokeh có hình.

### Tilt-shift

Tilt-shift giữ một dải nét có thể điều khiển, đồng thời mờ dần phía trên và dưới. Góc dải, phủ mềm, lệch phối cảnh, hình khẩu và tăng hiệu ứng thu nhỏ trong hộp thoại phù hợp cảnh kiểu mô hình thu nhỏ, kiến trúc hoặc bố cục cần vùng nét đọc như dải thiết kế chứ không phải tín hiệu độ sâu tròn.

### Circular Motion Blur

Circular Motion Blur kéo nhòe chi tiết quanh điểm trung tâm, biến cạnh thành vệt quay. Lựa chọn tự nhiên cho chủ thể xoay, năng lượng kiểu tuabin hoặc minh họa cần chuyển động quỹ đạo.

### Linear Motion Blur

Linear Motion Blur kéo dài chi tiết theo một hướng, mô phỏng di chuyển, rung máy hoặc cử chỉ nhanh qua khung. Đặc biệt hữu ích khi chuyển động cần cảm giác có hướng, đồ họa hơn là khuếch tán.

### Zoom Motion Blur

Zoom Motion Blur tỏa chi tiết từ trung tâm ra ngoài, gợi cảm giác lao về phía trước hoặc lùi xa người xem. Phù hợp khoảnh khắc tác động, vạch tốc độ và bố cục cần năng lượng zoom mà không phải vẽ lại toàn ảnh.

## Cải thiện

### High Pass

High Pass tách độ tương phản cục bộ vi mô thay vì thay đổi tông rộng. Chỉ có tỷ lệ và độ tương phản cần chỉnh — công cụ thẳng để trích cạnh, tạo lớp phủ sắc hoặc chuẩn bị bước làm sắc nhấn cấu trúc hơn màu.

### Noise Reduction

Noise Reduction đi ngược lại: giảm biến thiên vi mô không mong muốn để hình lớn đọc rõ hơn. Hữu ích khi tài liệu scan, kết cấu nén hoặc vùng vẽ quá tay cần đơn giản hóa trước khi vẽ hoặc lọc tiếp.

### Sharpen

Sharpen dùng mô hình unsharp mask, với bán kính, cường độ và ngưỡng điều khiển mức đẩy tương phản cục bộ. Thực tế phù hợp khôi phục độ nét sau mờ, thay đổi kích thước xuất hoặc bước hoàn thiện tinh khi chi tiết cần nổi mà không biến mỗi pixel thành nhiễu.

## Màu

### Tonal Grading

Tonal Grading ánh xạ màu theo dải tông thay vì chỉnh tương phản hay vẽ đường cong. Độ sáng từng pixel chọn pha mượt ba màu do người dùng đặt cho vùng tối, trung tính và sáng; ảnh giữ cấu trúc sáng-tối trong khi bảng màu dịch chuyển. Cường độ theo vùng, thiên lệch cân bằng kiểu Lightroom (trái thiên grade vùng tối, phải vùng sáng) và độ mềm chuyển tiếp kiểm soát tầm ảnh hưởng và cách các grade chồng lên nhau. Nhắm minh họa, truyện tranh, concept art và ảnh khi cần grade hoặc look thống nhất.

## Biến dạng

### Chromatic Aberration

Chromatic Aberration tách kênh màu ra xa trung tâm đã chọn, với điều khiển hướng xuyên tâm hoặc tiếp tuyến, lệch giữa cặp kênh, suy giảm và giữ độ sáng. Cả mã và hộp thoại đều coi đây là công cụ hai chiều: thêm viền quang sai cách điệu cho năng lượng, hoặc đảo dấu để sửa quang sai nhẹ trên nguồn.

### Lens Distortion

Lens Distortion biến dạng ảnh qua độ cong thùng hoặc gối, hạng cạnh, bù zoom, lệch tâm và làm sáng góc. Hữu ích cả khi sửa ảnh có cảm giác cong quang học lẫn khi cố tình đẩy về phong cách góc rộng hoặc ống cổ.

## Ánh sáng

### Bloom

Bloom biến vùng sáng thành quầng sáng có kiểm soát; ngưỡng, độ mềm, bán kính và cường độ xác định độ lan và mức nâng ảnh. Thêm giới hạn phơi sáng giúp dùng như hiệu ứng highlight chứ không tự động washout.

### Sky

Sky không chỉ là lớp phủ màu hay gradient: nó dựng bầu trời phân tích bằng mô hình Preetham, Hosek/Wilkie hoặc Nishita. Hộp thoại cho chỉnh chiếu, góc mặt trời, độ đục, mật độ khí quyển, độ cao, đĩa mặt trời và phơi sáng — từ phông trời trong đơn giản đến hoàng hôn hay chạng vạng có cơ sở vật lý hơn.

### Vignette

Vignette làm tối, tô màu hoặc thậm chí xóa dần về cạnh ảnh, với điều khiển hình dạng, bán kính, độ mềm, gamma, tỷ lệ, nén, xoay và đặt vị trí trên canvas. Vừa là xử lý cạnh ảnh cổ điển, vừa đủ linh hoạt làm khung che hoặc điểm nhấn bố cục không đều.

## Nhiễu

### HSV Noise

HSV Noise random hóa sắc độ, độ bão hòa và giá trị độc lập. Hữu ích khi ảnh cần màu sống động hoặc độ không ổn định kiểu analog mà không phá hẳn cấu trúc cục bộ.

### Hurl

Hurl là bản cực đoan của nhiễu: thay pixel bằng màu hoàn toàn ngẫu nhiên. Nên coi như nguồn hỗn loạn phá hủy cho glitch, kết cấu distressed hoặc mặt nạ cần vỡ mạnh.

### Pick

Pick thay mỗi pixel bằng hàng xóm được chọn ngẫu nhiên — ảnh vẫn liên hệ nguồn thay vì thành tĩnh thuần. Kết quả là biến thể xáo trộn, hạt có thể hữu cơ hơn nhiễu hoàn toàn ngẫu nhiên.

### Spread

Spread rải pixel bằng cách dịch chuyển ngẫu nhiên trong bán kính. Hữu ích khi cần phá vỡ tĩnh: bề mặt vỡ, cạnh lem hoặc kết cấu distressed vẫn giữ quan hệ màu của ảnh nguồn.

### Fractal

Fractal tạo nhiễu Perlin fractal tile được — đặc biệt hữu ích làm nguồn tái sử dụng cho mặt nạ, mây, kết cấu giấy, vỡ kiểu địa hình và lớp phủ thủ tục. Vì tile được nên nuôi quy trình lớn mà không lộ đường nối.

### Blue Noise Grain

Blue Noise Grain là trình tạo hạt đơn sắc kiểu phim-in của Lumi. Cài đặt sẵn cỡ hạt, masking blue-noise, thiên lệch trung tính và vùng tối, cùng seed cho thấy thiết kế đặt hạt đều và kiểm soát, không chỉ rải đốm ngẫu nhiên lên ảnh.

### Risograph Grain

Risograph Grain kế thừa logic hạt đó nhưng biến thành hiệu ứng in hai kẽm. Màu mực riêng, cân bằng kẽm, lệch kẽm có chủ ý và biến thể theo seed phù hợp poster, thẩm mỹ in indie và minh họa cần cảm giác in chồng vật lý hơn hoàn hảo số.

### Halftone (FM)

Halftone (FM) tạo halftone stochastic điều chế tần số bằng blue-noise hoặc phương pháp ngưỡng liên quan. Chế độ màu đơn sắc, duotone và CMYK, cùng điều khiển dot-gain và decorrelation kẽm hướng tới kết cấu in sống động, không đều thay vì lưới cứng.

## Cạnh

### Difference of Gaussians

Difference of Gaussians phát hiện cạnh bằng cách trừ hai bản mờ của ảnh. Toán tử gọn, hữu ích cho bản đồ cạnh, trích đường cách điệu và tìm chuyển tiếp cấu trúc mà không cam kết contour ngưỡng đầy đủ.

## Hình thái học

### Median

Median thay mỗi pixel bằng giá trị trung vị trong vùng lân cận — thường loại nhiễu đơn lẻ mà giữ ranh giới mạnh tốt hơn mờ đơn giản. Bộ lọc dọn thực dụng để làm phẳng nhiễu nhỏ mà không làm mềm cả ảnh ngay.

### Dilate

Dilate mở rộng vùng sáng ra ngoài bằng logic vùng lân cận nhận hình dạng tương tự. Trong tạo ảnh, có thể làm dày nét sáng, phình hình sáng hoặc lấp khe tối nhỏ.

### Erode

Erode làm bước bổ sung: mở rộng vùng tối, thu vùng sáng. Hữu ích mỏng chi tiết sáng, phóng khối tối hoặc siết mặt nạ và hình đồ họa.

## Họa tiết

### Checkerboard

Checkerboard tạo mẫu ô xen kẽ đều. Đơn giản nhưng hữu ích thử transparency, dựng mặt nạ, nền đồ họa hoặc nguồn hình học sạch.

### Grid

Grid vẽ chia ngang dọc lặp lại — hữu ích cho guide bố cục, phông thiết kế, minh họa kỹ thuật và masking thủ tục. Sinh bằng bộ lọc nên khoảng cách và diện mạo chỉnh được mà không dựng tay.

### Voronoi

Voronoi tạo kết cấu ô tile được từ điểm seed, với điều khiển loại đặc trưng, metric khoảng cách, độ ngẫu nhiên, chi tiết fractal và wrap liền mạch. Thực tế có thể đi từ ô nứt sạch đến đá, da, bản đồ hay mạng trừu tượng hữu cơ hơn.

### Wave

Wave tạo mẫu dải hoặc vòng theo profile sóng, bố cục hình học, biến dạng, chi tiết fractal và lệch pha. Hơn công cụ sọc đơn giản: gợn sóng kiểm soát, dải topo, đồ họa moiré hoặc trường vòng đồng tâm có nhiễu.

### Halftone (AM)

Halftone (AM) áp dụng màn chấm điều biên cổ điển, với tần số, hình chấm, độ sắc, chế độ màu và góc CMYK cho cấu trúc in rosette. So với halftone FM, đây là lựa chọn có trật tự, cơ học rõ hơn khi cần look báo in, offset hoặc hình lưới chấm cố ý lộ ra.
