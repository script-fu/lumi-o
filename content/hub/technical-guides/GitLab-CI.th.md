---
title: "GitLab CI"
type: docs
url: "hub/technical-guides/GitLab-CI"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 9917cebc417adeeae24d91b05b919b679a397d5db652cf4442d4330c0f8eeea5
---

Continuous Integration (CI) คือวิธีทดสอบ สร้าง และตรวจสอบโค้ดของคุณโดยอัตโนมัติทุกครั้งที่มีการเปลี่ยนแปลง

**GitLab** มีฟีเจอร์ CI/CD ในตัวผ่านไฟล์ `.gitlab-ci.yml` ไฟล์นี้ที่วางไว้ที่ root ของ repository จะบอก GitLab ว่าควรสร้างและทดสอบโปรเจกต์อย่างไร โดยกำหนด stages และ scripts ที่จะรันในสภาพแวดล้อมที่สะอาดทุกครั้งที่มีการ push การเปลี่ยนแปลง

เอกสารนี้อธิบายว่า CI/CD pipeline ของ GitLab ใน Lumi ทำงานอย่างไร รวมถึงบทบาทของไฟล์ `.gitlab-ci.yml` สคริปต์ shell และเครื่องมือภายนอก เช่น Meson และ Ninja

สำหรับเอกสารทางเทคนิคโดยละเอียดของกระบวนการ build CI ของ Lumi ดูที่ [README-CI.md](https://gitlab.gnome.org/pixelmixer/lumi/-/blob/main/build/linux/appimage/README-CI.md) ใน repository

## พื้นฐาน GitLab CI/CD

CI ถูกควบคุมโดยไฟล์ชื่อ `.gitlab-ci.yml` ไฟล์นี้กำหนด:

- **Stages**: กลุ่ม jobs ที่เรียงลำดับ (เช่น `build-this`, `build-that`, `package-up`)
- **Jobs**: งานแต่ละรายการที่รันภายในแต่ละ stage
- **Scripts**: คำสั่ง shell ที่รันสำหรับแต่ละ job
- **Runners**: คอมพิวเตอร์ที่ GitLab ใช้รัน jobs ที่กำหนดไว้ใน pipeline

ใน Lumi stages ของ pipeline คือ:

- `dependencies`
- `build lumi`
- `appimage`

## การ build แบบใช้ container

Pipeline ของ Lumi ใช้ containerization เพื่อให้การ build สม่ำเสมอ:

1. **สร้าง build container**: stage แรกใช้ Buildah สร้าง Docker image พร้อม dependencies ทั้งหมด
2. **ใช้ container**: stages ถัดไปรันภายใน container นี้ เพื่อให้สภาพแวดล้อมสม่ำเสมอ
3. **Build ที่ทำซ้ำได้**: การแยก container รับประกันผลลัพธ์เดียวกันบน runners ต่างๆ

แนวทางนี้ทำให้การ build ทำงานเหมือนกันบน GitLab runner ทุกตัว และให้สภาพแวดล้อมที่ควบคุมได้สำหรับกระบวนการ build ที่ซับซ้อน

### แหล่ง dependency ที่รวมอยู่ใน repo

CI dependency image ของ Lumi สร้าง stack ที่ fork จาก **แหล่งที่รวมอยู่ใน repo** (ไม่ clone จากภายนอก):

- `lumi-babl/` (BABL)
- `lumi-gegl/` (GEGL)
- `lumi-gtk3/` (GTK3)

ไดเรกทอรีเหล่านี้ถูกคัดลอกไปยัง build context ของ container และ compile เข้า dependency prefix (โดยทั่วไปคือ `/opt/lumi-deps`) วิธีนี้ทำให้ CI ทำซ้ำได้และ build AppImage ใช้ source of truth เดียวกับการพัฒนาในเครื่อง

## บทบาทของ shell scripts

Jobs ใน `.gitlab-ci.yml` มักเรียกคำสั่ง shell โดยตรง การทำงานที่ซับซ้อนมักย้ายไปยัง scripts แยกที่เก็บไว้ใน repository

Lumi CI ใช้ shell scripts แบบ modular เพื่อจัดระเบียบ logic ของการ build:

**ตัวอย่างการเรียก script:**
```yaml
script:
  - bash build/linux/appimage/lumi-goappimage.sh 2>&1 | tee appimage_creation.log
```

**ข้อดีของแนวทางนี้:**
- **YAML สะอาด**: ทำให้ไฟล์ `.gitlab-ci.yml` เน้นที่โครงสร้าง jobs
- **ดูแลรักษาง่าย**: logic ที่ซับซ้อน debug และแก้ไขได้ง่ายกว่าใน shell scripts
- **นำกลับมาใช้ได้**: scripts ใช้ได้ในบริบทหรือสภาพแวดล้อมต่างๆ
- **แบบ modular**: แยกด้านต่างๆ ของการ build เป็น scripts ที่เฉพาะเจาะจง

วิธีนี้ทำให้การตั้งค่า CI สะอาด ในขณะที่ยังรองรับกระบวนการ build ที่ซับซ้อน

## การเชื่อมต่อกับระบบ build

Lumi ใช้ **Meson** และ **Ninja** เพื่อเตรียมและ build โค้ด

ตัวอย่าง:

```
script:
  - meson setup _build-${CI_RUNNER_TAG} -Dprefix="${LUMI_PREFIX}"
  - ninja -C _build-${CI_RUNNER_TAG}
  - ninja -C _build-${CI_RUNNER_TAG} install
```

ที่นี่:

- `meson setup` เตรียมไดเรกทอรี build และสร้าง `build.ninja`
- `ninja` รันคำสั่ง build ตามที่กำหนด

## โครงสร้างระบบ build Meson

ระบบ build **Meson** ใช้ไฟล์ root `meson.build` ที่วางไว้ที่ root directory ของโปรเจกต์ ไฟล์นี้กำหนดการตั้งค่า build ระดับบนสุดและจุดเริ่มต้นของกระบวนการ build

- ไฟล์ root `meson.build` มักอยู่ในไดเรกทอรีเดียวกับ `.gitlab-ci.yml`
- จากนั้นมัน **cascade แบบ recursive** ไปยัง subdirectory ซึ่งแต่ละอันอาจมีไฟล์ `meson.build` ของตัวเอง
- ไฟล์ subdirectory เหล่านี้กำหนด targets, sources, dependencies และคำสั่ง build ที่เกี่ยวข้องกับไดเรกทอรีนั้น

## ตัวแปรสภาพแวดล้อม

ตัวแปรสำคัญใน pipeline ของ Lumi ได้แก่:

```yaml
variables:
  DEBIAN_FRONTEND: "noninteractive"  # Prevents interactive prompts
  DEB_VERSION: "trixie"              # Debian version for consistency
  CI_RUNNER_TAG: "x86_64"            # Architecture specification
```

**ตัวแปรเฉพาะ job:**
```yaml
build-lumi:
  variables:
    COMPILER: "clang"                                           # Compiler selection
    LINKER: "lld"                                               # Linker selection
    LUMI_PREFIX: "${CI_PROJECT_DIR}/_install-${CI_RUNNER_TAG}"  # Installation path
    DEPS_PREFIX: "/opt/lumi-deps"                               # Prebuilt dependency prefix
    MESON_OPTIONS: "-Dpkgconfig.relocatable=true -Drelocatable-bundle=yes"  # Build configuration
```

ตัวแปรเหล่านี้ควบคุมพฤติกรรมการ build และรับประกันความสม่ำเสมอระหว่าง stages และ runners ต่างๆ

## ตัวอย่างโครงสร้าง

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

ในโครงสร้างนี้:

- ไฟล์ root `meson.build` กำหนดค่าสภาพแวดล้อม build โดยรวม
- ไฟล์ `meson.build` ใน subdirectory จัดการรายละเอียดการ compile ของ component หรือ module เฉพาะ
- การจัด layout แบบลำดับชั้นนี้ทำให้ logic ของการ build เป็น modular และดูแลรักษาง่าย

## Artifacts ระหว่าง stages

Artifacts คือไฟล์ที่ jobs สร้างขึ้นและจำเป็นต้องใช้ใน stages ถัดไป:

```yaml
build-lumi:
  # ...job configuration...
  artifacts:
    paths:
      - "${LUMI_PREFIX}/"      # Installation files
      - _build-${CI_RUNNER_TAG}/meson-logs/meson-log.txt  # Build logs
```

## Stages ของ pipeline และ dependencies

Pipeline ของ Lumi ประกอบด้วย stages หลักสามอัน:

1. **Dependencies**: สร้างสภาพแวดล้อม build แบบ container พร้อมเครื่องมือและไลบรารีที่จำเป็นทั้งหมด
2. **Build Lumi**: compile Lumi ด้วย Meson และ Ninja ในสภาพแวดล้อมที่เตรียมไว้
3. **AppImage**: แพ็กแอปที่ build แล้วเป็นรูปแบบ AppImage ที่แจกจ่ายได้

**Dependencies ระหว่าง stages:**
```yaml
build-lumi:
  needs: [deps-debian]  # Waits for dependency container

lumi-appimage:
  needs: [build-lumi] # Waits for application build
```

แต่ละ stage จะรันหลังจาก dependencies เสร็จสมบูรณ์เท่านั้น เพื่อให้ลำดับการ build ถูกต้องและ artifacts พร้อมใช้งาน

## ชื่อ jobs ปัจจุบัน

`.gitlab-ci.yml` ของ Lumi กำหนดชื่อ jobs เหล่านี้:

- `deps-debian`
- `build-lumi`
- `lumi-appimage`

## สรุป

- `.gitlab-ci.yml` กำหนดโครงสร้างและ logic ของ pipeline
- Jobs มีคำสั่ง shell หรือ scripts ภายนอก
- เครื่องมืออย่าง Meson และ Ninja ใช้ภายใน jobs เป็นส่วนหนึ่งของกระบวนการ build

Lumi ใช้ GitLab CI เพื่อ build AppImage โดยอัตโนมัติสำหรับแพลตฟอร์มที่ใช้ Debian Pipeline นี้จะ build dependencies, compile Lumi และแพ็กเป็น AppImage

สำหรับรายละเอียดระดับ source code ให้ดูที่:

- `.gitlab-ci.yml` ที่ root ของ repository Lumi
- `build/linux/appimage/lumi-goappimage.sh`
- `build/linux/appimage/README-CI.md`

สำหรับรายละเอียดทางเทคนิคครบถ้วนเกี่ยวกับกระบวนการ build CI ของ Lumi รวมถึงการตั้งค่าสภาพแวดล้อม สถาปัตยกรรม scripts และการแก้ปัญหา ดูที่ [README-CI.md](https://gitlab.gnome.org/pixelmixer/lumi/-/blob/main/build/linux/appimage/README-CI.md)
