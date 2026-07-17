#!/usr/bin/env python3
"""Batch writer for fundamentals Conditionals and Iteration translations."""

from __future__ import annotations

import json
from pathlib import Path

SITE_ROOT = Path(__file__).resolve().parents[1]
SCRIPT_DIR = Path(__file__).resolve().parent
LANGS = ["de", "es", "fr", "it", "ja", "ko", "nl", "pl", "pt-br", "ru", "sv", "th", "zh-cn", "zh-tw"]

HASHES = {
    "content/hub/scripting/fundamentals/Conditionals/_index.md": "a6a08e6af8a8a31688dabd4434bee5da3ff07ec61763f636fb5c2029da03f472",
    "content/hub/scripting/fundamentals/Conditionals/conditionals-cond.md": "32d7e6d0c54bc515f245b0c108d23441754f7248c2510c61a552c693f37d0382",
    "content/hub/scripting/fundamentals/Conditionals/conditionals-if.md": "a31916ea815a99deebce805ed2023a7bedbf63325938649cebdd80e7eba209ee",
    "content/hub/scripting/fundamentals/Conditionals/conditionals-when.md": "61f1a78c3b37d9a33d3dff25f889287b32fc932bea8c22b4c06100052944b6a6",
    "content/hub/scripting/fundamentals/Iteration/_index.md": "1dc2e6858c3fe17ed2256e479e5f0e9ed9a7a3baea2e37a667f325fbbc39166d",
    "content/hub/scripting/fundamentals/Iteration/do.md": "db8c12b44717a78fddabba563fc62d081db9644b8a1f2b09d74db91eec84bfd1",
    "content/hub/scripting/fundamentals/Iteration/for-each.md": "e1e9a2537cadc894d45c7e25e28e9234f35e06298c289c5be57c15e7800cb8cd",
    "content/hub/scripting/fundamentals/Iteration/map.md": "c11f2c7984493d3fda20fca757958884b8752ef9a15640e4a7357c544e29c6c6",
    "content/hub/scripting/fundamentals/Iteration/recursion.md": "47fd79f37d5542e30722efaf4f87cd10efb77d825101f2045b191e3640137168",
}

WEIGHTS = {
    "content/hub/scripting/fundamentals/Conditionals/_index.md": 2,
    "content/hub/scripting/fundamentals/Conditionals/conditionals-cond.md": 5,
    "content/hub/scripting/fundamentals/Conditionals/conditionals-if.md": 4,
    "content/hub/scripting/fundamentals/Conditionals/conditionals-when.md": 5,
    "content/hub/scripting/fundamentals/Iteration/_index.md": 4,
    "content/hub/scripting/fundamentals/Iteration/do.md": 5,
    "content/hub/scripting/fundamentals/Iteration/for-each.md": 5,
    "content/hub/scripting/fundamentals/Iteration/map.md": 3,
    "content/hub/scripting/fundamentals/Iteration/recursion.md": 5,
}

PROSE_PATH = SCRIPT_DIR / "_batch_fundamentals_2_prose.json"


def fm(title: str, rel: str) -> str:
    return (
        "---\n"
        f'title: "{title}"\n'
        "type: docs\n"
        f"weight: {WEIGHTS[rel]}\n"
        "translation_provenance: ai-reviewed\n"
        "translation_lock: true\n"
        f"translation_source_sha256: {HASHES[rel]}\n"
        "---\n"
    )


def write(rel: str, lang: str, title: str, body: str) -> Path:
    src = Path(rel)
    out = SITE_ROOT / src.with_name(f"{src.stem}.{lang}.md")
    out.parent.mkdir(parents=True, exist_ok=True)
    out.write_text(fm(title, rel) + body, encoding="utf-8")
    return out


def load_translations() -> dict[str, dict[str, dict[str, str]]]:
    if not PROSE_PATH.exists():
        raise SystemExit(
            f"Missing {PROSE_PATH.name}. Run: python3 scripts/_write_f2_all.py"
        )
    return json.loads(PROSE_PATH.read_text(encoding="utf-8"))


def main() -> None:
    translations = load_translations()
    count = 0
    for rel, lang_map in translations.items():
        for lang, meta in lang_map.items():
            write(rel, lang, meta["title"], meta["body"])
            count += 1
    print(f"Wrote {count} translation files")


if __name__ == "__main__":
    main()
