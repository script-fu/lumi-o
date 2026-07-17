#!/usr/bin/env python3
"""Generate self-contained _batch_fundamentals_2.py with all 126 translations."""

from __future__ import annotations

import importlib.util
import json
import pprint
from pathlib import Path

SCRIPT_DIR = Path(__file__).resolve().parent
OUT = SCRIPT_DIR / "_batch_fundamentals_2.py"
HEADER = (SCRIPT_DIR / "_batch_fundamentals_2.py").read_text(encoding="utf-8").split("# --- Page content")[0].rstrip()

spec = importlib.util.spec_from_file_location("data", SCRIPT_DIR / "_batch_fundamentals_2_data.py")
data = importlib.util.module_from_spec(spec)
spec.loader.exec_module(data)  # type: ignore[union-attr]

LANGS = ["de", "es", "fr", "it", "ja", "ko", "nl", "pl", "pt-br", "ru", "sv", "th", "zh-cn", "zh-tw"]

# Load pre-built translation payloads from JSON (generated alongside this script)
PAYLOAD_PATH = SCRIPT_DIR / "_batch_fundamentals_2_payload.json"
if not PAYLOAD_PATH.exists():
    raise SystemExit(f"Missing {PAYLOAD_PATH} — run build payload first")

PAYLOAD: dict[str, dict[str, dict[str, str]]] = json.loads(PAYLOAD_PATH.read_text(encoding="utf-8"))

# Merge cond_index from data module (native quality)
for lang in LANGS:
    title, body = data.cond_index(lang)
    PAYLOAD.setdefault("content/hub/scripting/fundamentals/Conditionals/_index.md", {})[lang] = {
        "title": title,
        "body": body,
    }

lines = [
    "",
    "TRANSLATIONS: dict[str, dict[str, dict[str, str]]] = " + pprint.pformat(PAYLOAD, width=120, sort_keys=True),
    "",
    "",
    "def main() -> None:",
    "    count = 0",
    "    for source_rel, lang_map in TRANSLATIONS.items():",
    "        for lang, meta in lang_map.items():",
    "            write(source_rel, lang, meta['title'], meta['body'])",
    "            count += 1",
    "    print(f'Wrote {count} translation files')",
    "",
    "",
    'if __name__ == "__main__":',
    "    main()",
    "",
]

OUT.write_text(HEADER + "\n".join(lines), encoding="utf-8")
total = sum(len(v) for v in PAYLOAD.values())
print(f"Wrote {OUT} ({total} entries)")
