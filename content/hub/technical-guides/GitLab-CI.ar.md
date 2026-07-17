---
title: "GitLab CI"
type: docs
translation_provenance: ai-reviewed
translation_source_sha256: 9917cebc417adeeae24d91b05b919b679a397d5db652cf4442d4330c0f8eeea5
url: "hub/technical-guides/GitLab-CI"
translation_lock: true
---

التكامل المستمر (CI) طريقة لاختبار الكود وبنائه والتحقق منه تلقائيًا عند إجراء تغييرات.

**GitLab** يوفّر ميزات CI/CD مدمجة عبر ملف `.gitlab-ci.yml`. هذا الملف، الموضوع في جذر المستودع، يخبر GitLab كيف يبني المشروع ويختبره. يحدّد مراحل وسكربتات تُشغَّل في بيئة نظيفة في كل مرة تُدفع فيها تغييرات.

يوضّح هذا المستند كيف يعمل خط أنابيب GitLab CI/CD في Lumi، بما في ذلك دور ملف `.gitlab-ci.yml` وسكربتات shell والأدوات الخارجية مثل Meson وNinja.

للتوثيق الفني التفصيلي لعملية بناء Lumi CI، راجع [README-CI.md](https://gitlab.gnome.org/pixelmixer/lumi/-/blob/main/build/linux/appimage/README-CI.md) في المستودع.

## أساسيات GitLab CI/CD

يُتحكَّم في CI عبر ملف `.gitlab-ci.yml`. يحدّد هذا الملف:

- **المراحل**: مجموعات مرتّبة من المهام (مثل `build-this` و`build-that` و`package-up`)
- **المهام**: مهام فردية تُنفَّذ في كل مرحلة
- **السكربتات**: أوامر shell تُنفَّذ لكل مهمة
- **Runners**: أجهزة يستخدمها GitLab لتشغيل المهام المعرّفة في خط الأنابيب

في Lumi، مراحل خط الأنابيب هي:

- `dependencies`
- `build lumi`
- `appimage`

## بناءات قائمة على الحاويات

يستخدم خط أنابيب Lumi الحاويات لبناءات متسقة:

1. **إنشاء حاوية البناء**: تستخدم المرحلة الأولى Buildah لإنشاء صورة Docker بكل التبعيات
2. **استخدام الحاوية**: تُشغَّل المراحل اللاحقة داخل هذه الحاوية لضمان بيئة متسقة
3. **بناءات قابلة للتكرار**: يضمن عزل الحاوية النتائج نفسها عبر runners مختلفة

يضمن هذا أن تعمل البناءات بنفس الطريقة على أي GitLab runner ويوفر بيئة مضبوطة لعمليات بناء معقّدة.

### مصادر التبعيات المدمجة

تبني صورة تبعيات CI في Lumi المكدّس المتفرّع من **مصادر مدمجة داخل المستودع** (وليس من استنساخات خارجية):

- `lumi-babl/` (BABL)
- `lumi-gegl/` (GEGL)
- `lumi-gtk3/` (GTK3)

تُنسخ هذه المجلدات إلى سياق بناء الحاوية وتُجمَّع في بادئة التبعيات (عادةً `/opt/lumi-deps`). يبقي ذلك CI قابلًا للتكرار ويضمن أن بناء AppImage يستخدم نفس مصدر الحقيقة كالتطوير المحلي.

## دور سكربتات shell

تستدعي المهام في `.gitlab-ci.yml` عادةً أوامر shell مباشرة. غالبًا ما تُنقل العمليات المعقّدة إلى سكربتات منفصلة في المستودع.

يستخدم Lumi CI سكربتات shell معيارية لتنظيم منطق البناء:

**مثال على استدعاء سكربت:**
```yaml
script:
  - bash build/linux/appimage/lumi-goappimage.sh 2>&1 | tee appimage_creation.log
```

**فوائد هذا النهج:**
- **YAML نظيف**: يُبقي ملف `.gitlab-ci.yml` مركّزًا على بنية المهام
- **قابلية الصيانة**: المنطق المعقّد أسهل في تصحيحه وتعديله في سكربتات shell
- **إعادة الاستخدام**: يمكن استخدام السكربتات في سياقات أو بيئات مختلفة
- **النمطية**: يمكن فصل جوانب البناء إلى سكربتات متخصّصة

يُبقي ذلك تكوين CI نظيفًا مع السماح بعمليات بناء متقدّمة.

## التكامل مع أنظمة البناء

يستخدم Lumi **Meson** و**Ninja** لإعداد الكود ثم بنائه.

مثال:

```
script:
  - meson setup _build-${CI_RUNNER_TAG} -Dprefix="${LUMI_PREFIX}"
  - ninja -C _build-${CI_RUNNER_TAG}
  - ninja -C _build-${CI_RUNNER_TAG} install
```

هنا:

- `meson setup` يُعدّ دليل البناء وينشئ `build.ninja`
- `ninja` ينفّذ أوامر البناء كما هو محدّد

## هيكل نظام بناء Meson

يستخدم نظام البناء **Meson** ملفًا جذريًا `meson.build` في دليل جذر المشروع. يحدّد هذا الملف تكوين البناء على المستوى الأعلى ونقطة الدخول لعملية البناء.

- يقع `meson.build` الجذري عادةً في نفس دليل `.gitlab-ci.yml`
- من هناك، **يتفرّع بشكل متكرّر** إلى أدلة فرعية، لكل منها قد يكون لها ملف `meson.build` خاص
- تحدّد ملفات الأدلة الفرعية الأهداف والمصادر والتبعيات وتعليمات البناء ذات الصلة

## متغيّرات البيئة

تشمل المتغيّرات الرئيسية في خط أنابيب Lumi:

```yaml
variables:
  DEBIAN_FRONTEND: "noninteractive"  # يمنع المطالبات التفاعلية
  DEB_VERSION: "trixie"              # إصدار Debian للاتساق
  CI_RUNNER_TAG: "x86_64"            # تحديد البنية
```

**متغيّرات خاصة بالمهمة:**
```yaml
build-lumi:
  variables:
    COMPILER: "clang"                                           # اختيار المترجم
    LINKER: "lld"                                               # اختيار الرابط
    LUMI_PREFIX: "${CI_PROJECT_DIR}/_install-${CI_RUNNER_TAG}"  # مسار التثبيت
    DEPS_PREFIX: "/opt/lumi-deps"                               # بادئة التبعيات المسبقة البناء
    MESON_OPTIONS: "-Dpkgconfig.relocatable=true -Drelocatable-bundle=yes"  # تكوين البناء
```

تتحكّم هذه المتغيّرات في سلوك البناء وتضمن الاتساق عبر المراحل والrunners.

## هيكل مثال

```
project-root/
├── .gitlab-ci.yml
├── meson.build              <-- ملف Meson الجذري
├── src/
│   ├── meson.build          <-- ملف Meson للدليل الفرعي
│   └── some_source.c
├── data/
│   ├── meson.build
│   └── icons/
```

في هذا الهيكل:

- يُكوّن ملف `meson.build` الجذري بيئة البناء العامة
- تتولّى ملفات `meson.build` الفرعية تفاصيل التجميع لمكوّنات أو وحدات محدّدة
- يحافظ هذا التخطيط الهرمي على منطق البناء معياريًا وقابلًا للصيانة

## المخرجات بين المراحل

المخرجات (artifacts) ملفات تُنشئها المهام ويحتاجها لاحقًا:

```yaml
build-lumi:
  # ...تكوين المهمة...
  artifacts:
    paths:
      - "${LUMI_PREFIX}/"      # ملفات التثبيت
      - _build-${CI_RUNNER_TAG}/meson-logs/meson-log.txt  # سجلات البناء
```

## مراحل خط الأنابيب والتبعيات

يتكوّن خط أنابيب Lumi من ثلاث مراحل رئيسية:

1. **التبعيات**: إنشاء بيئة بناء محتواة بكل الأدوات والمكتبات المطلوبة
2. **Build Lumi**: تجميع Lumi باستخدام Meson وNinja في البيئة المُعدّة
3. **AppImage**: تغليف التطبيق المبني في تنسيق AppImage قابل للتوزيع

**تبعيات المراحل:**
```yaml
build-lumi:
  needs: [deps-debian]  # ينتظر حاوية التبعيات

lumi-appimage:
  needs: [build-lumi] # ينتظر بناء التطبيق
```

تُشغَّل كل مرحلة فقط بعد اكتمال تبعياتها بنجاح، ما يضمن ترتيب بناء صحيحًا وتوفر المخرجات.

## أسماء المهام الحالية

يُعرّف `.gitlab-ci.yml` في Lumi حاليًا هذه المهام:

- `deps-debian`
- `build-lumi`
- `lumi-appimage`

## ملخص

- `.gitlab-ci.yml` يحدّد بنية ومنطق خط الأنابيب
- تحتوي المهام على أوامر shell أو سكربتات خارجية
- تُستخدم أدوات مثل Meson وNinja داخل المهام كجزء من عملية البناء

يستخدم Lumi GitLab CI لبناء AppImage تلقائيًا لمنصات مبنية على Debian. يبني خط الأنابيب التبعيات، يجمّع Lumi، ثم يغلّف AppImage.

للتفاصيل على مستوى المصدر، استخدم:

- `.gitlab-ci.yml` في جذر مستودع Lumi
- `build/linux/appimage/lumi-goappimage.sh`
- `build/linux/appimage/README-CI.md`

للتفاصيل الفنية الشاملة حول عملية بناء Lumi CI، بما في ذلك إعداد البيئة وبنية السكربتات واستكشاف الأخطاء، راجع [README-CI.md](https://gitlab.gnome.org/pixelmixer/lumi/-/blob/main/build/linux/appimage/README-CI.md).
