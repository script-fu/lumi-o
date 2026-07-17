---
title: "CI GitLab"
type: docs
translation_provenance: ai-reviewed
translation_source_sha256: 9917cebc417adeeae24d91b05b919b679a397d5db652cf4442d4330c0f8eeea5
url: "hub/technical-guides/GitLab-CI"
translation_lock: true
---
Tích hợp liên tục (CI) là một cách để tự động kiểm tra, xây dựng và xác thực mã của bạn bất cứ khi nào có thay đổi.

**GitLab** cung cấp các tính năng CI/CD tích hợp thông qua tệp `.gitlab-ci.yml`. Tệp này, được đặt trong thư mục gốc của kho lưu trữ, cho GitLab biết cách xây dựng và thử nghiệm dự án của bạn. Nó xác định các giai đoạn và tập lệnh được chạy trong môi trường sạch sẽ mỗi khi có thay đổi.

Tài liệu này phác thảo cách hoạt động của quy trình GitLab CI/CD của Lumi, bao gồm vai trò của tệp `.gitlab-ci.yml`, tập lệnh shell và các công cụ bên ngoài như Meson và Ninja.

Để biết tài liệu kỹ thuật chi tiết về quy trình xây dựng Lumi CI, hãy xem [README-CI.md](https://gitlab.gnome.org/pixelmixer/lumi/-/blob/main/build/linux/appimage/README-CI.md) trong kho lưu trữ.

## Khái niệm cơ bản về GitLab CI/CD

CI được kiểm soát bởi một tệp có tên `.gitlab-ci.yml`. Tập tin này định nghĩa:

- **Giai đoạn**: Nhóm job theo thứ tự (ví dụ: `build-this`, `build-that`, `package-up`)
- **Job**: Tác vụ riêng lẻ chạy trong từng giai đoạn
- **Script**: Lệnh shell thực thi cho mỗi job
- **Runner**: Máy GitLab dùng để chạy job trong pipeline

Trong Lumi, các giai đoạn của quy trình là:

- `dependencies`
- `build lumi`
- `appimage`

## Build dựa trên container

Pipeline Lumi dùng container để build nhất quán:

1. **Tạo container build**: Giai đoạn đầu dùng Buildah tạo image Docker với mọi phụ thuộc
2. **Dùng container**: Các giai đoạn sau chạy trong container này, đảm bảo môi trường nhất quán
3. **Build có thể tái tạo**: Cô lập container đảm bảo kết quả giống nhau trên các runner

Cách tiếp cận này đảm bảo rằng các bản dựng hoạt động giống nhau trên mọi trình chạy GitLab và cung cấp môi trường được kiểm soát cho các quy trình xây dựng phức tạp.

### Nguồn phụ thuộc tích hợp

Image phụ thuộc CI của Lumi build ngăn xếp fork từ **nguồn tích hợp trong repo** (không phải clone bên ngoài):

- `lumi-babl/` (BABL)
- `lumi-gegl/` (GEGL)
- `lumi-gtk3/` (GTK3)

Các thư mục này được copy vào ngữ cảnh build container và biên dịch vào prefix phụ thuộc (thường là `/opt/lumi-deps`). Nhờ vậy CI có thể tái tạo và AppImage dùng cùng nguồn sự thật như phát triển cục bộ.

## Vai trò của tập lệnh shell

Job trong `.gitlab-ci.yml` thường gọi trực tiếp lệnh shell. Thao tác phức tạp thường tách thành tập lệnh riêng trong kho.

Lumi CI sử dụng các tập lệnh shell mô-đun để tổ chức logic xây dựng:

**Ví dụ gọi tập lệnh:**
```yaml
script:
  - bash build/linux/appimage/lumi-goappimage.sh 2>&1 | tee appimage_creation.log
```

**Lợi ích:**
- **YAML gọn**: Giữ `.gitlab-ci.yml` tập trung vào cấu trúc job
- **Dễ bảo trì**: Logic phức tạp dễ gỡ lỗi và sửa trong shell
- **Tái sử dụng**: Tập lệnh dùng được ở ngữ cảnh hoặc môi trường khác
- **Mô-đun**: Tách các khía cạnh build thành tập lệnh riêng

Điều này giữ cho cấu hình CI sạch sẽ đồng thời cho phép các quy trình xây dựng phức tạp.

## Tích hợp hệ thống build

Lumi dùng **Meson** và **Ninja** để chuẩn bị rồi biên dịch mã.

Ví dụ:

```
script:
  - meson setup _build-${CI_RUNNER_TAG} -Dprefix="${LUMI_PREFIX}"
  - ninja -C _build-${CI_RUNNER_TAG}
  - ninja -C _build-${CI_RUNNER_TAG} install
```

Đây:

- `meson setup` chuẩn bị thư mục build và tạo `build.ninja`
- `ninja` chạy lệnh build theo định nghĩa

## Cấu trúc hệ thống build Meson

Hệ thống build **Meson** dùng tệp gốc `meson.build` tại thư mục gốc dự án. Tệp này định nghĩa cấu hình build cấp cao và điểm vào quy trình build.

- `meson.build` gốc thường nằm cùng thư mục với `.gitlab-ci.yml`
- Từ đó, nó **lan xuống đệ quy** các thư mục con, mỗi thư mục có thể có `meson.build` riêng
- Các tệp con này định nghĩa target, nguồn, phụ thuộc và hướng dẫn build cho thư mục đó

## Biến môi trường

Các biến chính trong quy trình của Lumi bao gồm:

```yaml
variables:
  DEBIAN_FRONTEND: "noninteractive"  # Prevents interactive prompts
  DEB_VERSION: "trixie"              # Debian version for consistency
  CI_RUNNER_TAG: "x86_64"            # Architecture specification
```

**Biến theo job:**
```yaml
build-lumi:
  variables:
    COMPILER: "clang"                                           # Compiler selection
    LINKER: "lld"                                               # Linker selection
    LUMI_PREFIX: "${CI_PROJECT_DIR}/_install-${CI_RUNNER_TAG}"  # Installation path
    DEPS_PREFIX: "/opt/lumi-deps"                               # Prebuilt dependency prefix
    MESON_OPTIONS: "-Dpkgconfig.relocatable=true -Drelocatable-bundle=yes"  # Build configuration
```

Các biến này điều khiển hành vi build và đảm bảo nhất quán giữa giai đoạn và runner.

## Cấu trúc ví dụ

```
project-root/
├── .gitlab-ci.yml
├── meson.build              <-- Root Meson file
├── src/
│   ├── meson.build          <-- Subdirectory Meson file
│   └── some_source.c
├── data/
│   ├── meson.build
│   └── icons/
```

Trong cấu trúc này:

- Tệp `meson.build` gốc cấu hình môi trường build tổng thể
- Tệp `meson.build` con xử lý chi tiết biên dịch cho thành phần hoặc module cụ thể
- Bố cục phân cấp này giữ logic build mô-đun và dễ bảo trì

## Artifact giữa các giai đoạn

Artifact là tệp job tạo ra mà giai đoạn sau cần:

```yaml
build-lumi:
  # ...job configuration...
  artifacts:
    paths:
      - "${LUMI_PREFIX}/"      # Installation files
      - _build-${CI_RUNNER_TAG}/meson-logs/meson-log.txt  # Build logs
```

## Giai đoạn pipeline và phụ thuộc

Pipeline Lumi gồm ba giai đoạn chính:

1. **Dependencies**: Tạo môi trường build container với mọi công cụ và thư viện cần thiết
2. **Build Lumi**: Biên dịch Lumi bằng Meson và Ninja trong môi trường đã chuẩn bị
3. **AppImage**: Đóng gói ứng dụng đã build thành AppImage có thể phân phối

**Phụ thuộc giai đoạn:**
```yaml
build-lumi:
  needs: [deps-debian]  # Waits for dependency container

lumi-appimage:
  needs: [build-lumi] # Waits for application build
```

Mỗi giai đoạn chỉ chạy sau khi phụ thuộc hoàn thành thành công, đảm bảo thứ tự build và artifact sẵn sàng.

## Tên job hiện tại

Lumi `.gitlab-ci.yml` hiện xác định các tên công việc sau:

- `deps-debian`
- `build-lumi`
- `lumi-appimage`

## Tóm tắt

- `.gitlab-ci.yml` định nghĩa cấu trúc và logic pipeline
- Job chứa lệnh shell hoặc tập lệnh bên ngoài
- Meson và Ninja được dùng trong job như phần của quy trình build

Lumi dùng GitLab CI để tự động build AppImage cho nền tảng Debian. Pipeline build phụ thuộc, biên dịch Lumi rồi đóng gói AppImage.

Để biết chi tiết cấp nguồn, hãy sử dụng:

- `.gitlab-ci.yml` trong thư mục gốc của kho Lumi
- `build/linux/appimage/lumi-goappimage.sh`
- `build/linux/appimage/README-CI.md`

Để biết chi tiết kỹ thuật toàn diện về quy trình xây dựng Lumi CI, bao gồm thiết lập môi trường, kiến trúc tập lệnh và khắc phục sự cố, hãy tham khảo [README-CI.md](https://gitlab.gnome.org/pixelmixer/lumi/-/blob/main/build/linux/appimage/README-CI.md).