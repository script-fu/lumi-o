#!/usr/bin/env python3
"""Create _batch_fundamentals_2_pages.py with all page builders."""

from pathlib import Path
import json
import re

# Import cond_index translations from data module at runtime in generated file
HEADER = '''"""Page builders for batch fundamentals 2."""

from __future__ import annotations

from _batch_fundamentals_2_data import cond_index

LANGS = ["de", "es", "fr", "it", "ja", "ko", "nl", "pl", "pt-br", "ru", "sv", "th", "zh-cn", "zh-tw"]

def _j(*p):
    return "\\n".join(p)

'''

OUT = Path(__file__).resolve().parent / "_batch_fundamentals_2_pages.py"

# Load prose bodies from JSON if present
PROSE = Path(__file__).resolve().parent / "_batch_fundamentals_2_prose.json"
if not PROSE.exists():
    raise SystemExit("Run _generate_f2_prose.py first")

data = json.loads(PROSE.read_text(encoding="utf-8"))

lines = [HEADER]

for rel, lang_bodies in data.items():
    safe = re.sub(r"[^a-zA-Z0-9_]", "_", rel)
    if rel == "content/hub/scripting/fundamentals/Conditionals/_index.md":
        continue
    lines.append(f"def _page_{safe}(lang: str) -> tuple[str, str]:")
    lines.append(f"    bodies = {repr(lang_bodies)}")
    lines.append("    meta = bodies[lang]")
    lines.append('    return meta["title"], meta["body"]')
    lines.append("")

lines.append("PAGE_BUILDERS = {")
for rel in data:
    safe = re.sub(r"[^a-zA-Z0-9_]", "_", rel)
    if rel == "content/hub/scripting/fundamentals/Conditionals/_index.md":
        lines.append(f'    "{rel}": cond_index,')
    else:
        lines.append(f'    "{rel}": _page_{safe},')
lines.append("}")
lines.append("")

OUT.write_text("\n".join(lines), encoding="utf-8")
print(f"Wrote {OUT}")
