#!/usr/bin/env python3
"""Generate ai-reviewed fundamentals translations for Lumi website."""

from __future__ import annotations

import hashlib
import re
from pathlib import Path

SITE_ROOT = Path(__file__).resolve().parent.parent
LANGS = ["de", "es", "fr", "it", "ja", "ko", "nl", "pl", "pt-br", "ru", "sv", "th", "zh-cn", "zh-tw"]

SOURCE_HASHES = {
    "content/hub/scripting/_index.md": "2116be7665eb5fa29e6a526814a2158919062b9bde91cd12390d1dc4d13e19e0",
    "content/hub/scripting/fundamentals/_index.md": "201acfcd95ecd79800e3cb6faf6628700649b91b6f9669b83acea9fb7c3c40ff",
    "content/hub/scripting/fundamentals/Conditionals/_index.md": "a6a08e6af8a8a31688dabd4434bee5da3ff07ec61763f636fb5c2029da03f472",
    "content/hub/scripting/fundamentals/Conditionals/conditionals-cond.md": "32d7e6d0c54bc515f245b0c108d23441754f7248c2510c61a552c693f37d0382",
    "content/hub/scripting/fundamentals/Conditionals/conditionals-if.md": "a31916ea815a99deebce805ed2023a7bedbf63325938649cebdd80e7eba209ee",
    "content/hub/scripting/fundamentals/Conditionals/conditionals-when.md": "61f1a78c3b37d9a33d3dff25f889287b32fc932bea8c22b4c06100052944b6a6",
    "content/hub/scripting/fundamentals/Data Structures/_index.md": "f2e5dad6d2a5b677f9f18aefab366c5499cdbb8a5f3ebe5d97656255810e854f",
    "content/hub/scripting/fundamentals/Data Structures/alists.md": "05e4621ad061bed6351b31246d6705025936683acec1f2d104a0fd7f038f31f7",
    "content/hub/scripting/fundamentals/Data Structures/lists.md": "3402372e80f6b7c94f9e9423f7a9285a03a76d1706433953a93e65c34a2318e9",
    "content/hub/scripting/fundamentals/Data Structures/vectors.md": "57cae97347c4a9524567ebcc9eafbdf04228c2792c24e87784bf5f6255987d79",
    "content/hub/scripting/fundamentals/Functions/_index.md": "c00aaf1e68592dcb36dc914e6713d3dfe68870506b71842d63f8d6a02a01337a",
    "content/hub/scripting/fundamentals/Functions/lambda.md": "0b7e9469f1005fe09a6600b0e748e282fe860a701b6d4cdc65854420ab6f99f7",
    "content/hub/scripting/fundamentals/Functions/variadic.md": "0433dd9a86d6d273965c5f962121c0cc17717b5407f3f4b87282a33e2ea89c78",
    "content/hub/scripting/fundamentals/Iteration/_index.md": "1dc2e6858c3fe17ed2256e479e5f0e9ed9a7a3baea2e37a667f325fbbc39166d",
    "content/hub/scripting/fundamentals/Iteration/do.md": "db8c12b44717a78fddabba563fc62d081db9644b8a1f2b09d74db91eec84bfd1",
    "content/hub/scripting/fundamentals/Iteration/for-each.md": "e1e9a2537cadc894d45c7e25e28e9234f35e06298c289c5be57c15e7800cb8cd",
    "content/hub/scripting/fundamentals/Iteration/map.md": "c11f2c7984493d3fda20fca757958884b8752ef9a15640e4a7357c544e29c6c6",
    "content/hub/scripting/fundamentals/Iteration/recursion.md": "47fd79f37d5542e30722efaf4f87cd10efb77d825101f2045b191e3640137168",
    "content/hub/scripting/fundamentals/Variables and Scope/_index.md": "a9918c313de4c5b034465400bfcbf1d493996435543a410382e481bde0d19ae4",
    "content/hub/scripting/fundamentals/Variables and Scope/define.md": "da147bc6719c5d9c569a9e6e0f50ecf0cb8cb3ed90179fe969457b802a19890f",
    "content/hub/scripting/fundamentals/Variables and Scope/let vs define.md": "3931ad66060e30fb62a4634fd1c6dc05a008c71dfee8bd5b80d832036ae117f1",
    "content/hub/scripting/fundamentals/Variables and Scope/let.md": "005223eb0588849468d9d96fec0070456c3dc53fa30bee45eedd1a793c20875d",
    "content/hub/scripting/fundamentals/Variables and Scope/symbols.md": "4ae0cc2f5749cbe997d6fa25315ee3fe54646eb065b4dba0114778c75a889ae5",
}

# source_rel -> lang -> full markdown content (without needing to re-read English)
TRANSLATIONS: dict[str, dict[str, str]] = {}


def split_front_matter(text: str) -> tuple[str, str]:
    if not text.startswith("---"):
        return "", text
    end = text.find("\n---", 3)
    if end == -1:
        return "", text
    end += 4
    if end < len(text) and text[end] == "\n":
        end += 1
    return text[:end], text[end:]


def extract_front_matter_field(front: str, key: str) -> str:
    match = re.search(rf"(?m)^{re.escape(key)}:\s*(.+)$", front)
    return match.group(1).strip().strip('"') if match else ""


def build_front_matter(source_rel: str, lang: str, translated: str) -> str:
    src_path = SITE_ROOT / source_rel
    src_front, _ = split_front_matter(src_path.read_text(encoding="utf-8"))
    tr_front, _ = split_front_matter(translated)

    title = extract_front_matter_field(tr_front, "title") or extract_front_matter_field(src_front, "title")
    lines = ["---"]
    lines.append(f'title: "{title}"')

    for key in ("type", "weight", "toc"):
        value = extract_front_matter_field(tr_front, key) or extract_front_matter_field(src_front, key)
        if value:
            if key == "weight" or key == "toc":
                lines.append(f"{key}: {value}")
            else:
                lines.append(f'{key}: "{value}"')

    sha = SOURCE_HASHES[source_rel]
    lines.extend(
        [
            "translation_provenance: ai-reviewed",
            "translation_lock: true",
            f"translation_source_sha256: {sha}",
            "---",
        ]
    )
    return "\n".join(lines) + "\n"


def write_translation(source_rel: str, lang: str, body: str) -> None:
    src_path = SITE_ROOT / source_rel
    stem = src_path.with_suffix("")
    out_path = Path(str(stem) + f".{lang}.md")
    front = build_front_matter(source_rel, lang, body)
    _, tr_body = split_front_matter(body)
    out_path.write_text(front + tr_body, encoding="utf-8")


def main() -> None:
    count = 0
    for source_rel, lang_map in TRANSLATIONS.items():
        for lang, content in lang_map.items():
            write_translation(source_rel, lang, content)
            count += 1
    print(f"Wrote {count} translation files")


if __name__ == "__main__":
    main()
