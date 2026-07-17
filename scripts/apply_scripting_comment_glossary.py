#!/usr/bin/env python3
"""Apply scripting_comment_glossary.json to translated scripting pages."""

from __future__ import annotations

import json
import re
import sys
from pathlib import Path

LANGS = ["de", "es", "fr", "it", "ja", "ko", "nl", "pl", "pt-br", "ru", "sv", "th", "zh-cn", "zh-tw"]
LANG_RE = re.compile(r"\.(" + "|".join(re.escape(x) for x in LANGS) + r")\.md$")


def main() -> int:
    site_root = Path(sys.argv[1] if len(sys.argv) > 1 else ".")
    glossary_path = site_root / "scripts" / "scripting_comment_glossary.json"
    content_root = site_root / "content" / "hub" / "scripting"

    glossary: dict[str, dict[str, str]] = json.loads(glossary_path.read_text(encoding="utf-8"))
    # Longest keys first to avoid partial replacement issues.
    items = sorted(glossary.items(), key=lambda kv: len(kv[0]), reverse=True)

    updated_files = 0
    replacements = 0

    for path in sorted(content_root.rglob("*.md")):
        m = LANG_RE.search(path.name)
        if not m:
            continue
        lang = m.group(1)
        text = path.read_text(encoding="utf-8")
        original = text
        for en_comment, translations in items:
            localized = translations.get(lang)
            if not localized or localized == en_comment:
                continue
            if en_comment in text:
                count = text.count(en_comment)
                text = text.replace(en_comment, localized)
                replacements += count
        if text != original:
            path.write_text(text, encoding="utf-8")
            updated_files += 1

    print(f"[apply] glossary entries: {len(glossary)}")
    print(f"[apply] files updated: {updated_files}")
    print(f"[apply] comment replacements: {replacements}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
