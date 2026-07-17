---
title: "معالجة دفعية"
type: docs
translation_provenance: ai-reviewed
translation_source_sha256: 141b5ee23e77ecfc8ef5e8112706cfaebc1a2a528518218296dffda5b9dee6d1
url: "hub/scripting/plug-ins/batch-process"
translation_lock: true
---
مثال عملي شامل لمعالجة ملفات عديدة دفعة واحدة.

## أين يوجد

- [عرض المصدر](https://gitlab.gnome.org/pixelmixer/lumi-dev/-/blob/main/plug-ins/lumi/batch-process/batch-process.scm)

## أين يظهر في Lumi

- **ملف ← معالجة دفعية**

## ما يوضّحه

- معاملات `SF-DIRNAME` لمجلدات المصدر/الوجهة
- التحقّق من مسارات واجهة المستخدم الرسومية مع بدائل (`validate-path-and-dir`)
- مسح المجلدات بشكل متكرّر والتكرار
- الإبلاغ عن التقدّم للعمليات طويلة الأمد
