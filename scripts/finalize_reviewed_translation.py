#!/usr/bin/env python3
"""Stamp ai-reviewed metadata on a website translation page."""

from __future__ import annotations

import argparse
import hashlib
import re
from pathlib import Path

SITE_ROOT = Path(__file__).resolve().parent.parent
LANG_SUFFIX_RE = re.compile(r"\.([a-z]{2}(?:-[a-z]{2})?)\.md$")


def file_hash(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def split_front_matter(text: str) -> tuple[str, str]:
    match = re.match(r"\ufeff?\s*---\r?\n([\s\S]*?)\r?\n---\r?\n?", text)
    if not match:
        return "", text
    end = match.end()
    return text[:end], text[end:]


def extract_value(front: str, key: str) -> str:
    match = re.search(rf"(?im)^{re.escape(key)}\s*:\s*(.*?)\s*$", front)
    if not match:
        return ""
    return match.group(1).strip().strip("\"'")


def set_scalar(front: str, key: str, value: str, quoted: bool = False) -> str:
    rendered = f'"{value}"' if quoted else value
    pattern = rf"(?im)^{re.escape(key)}\s*:.*$"
    replacement = f"{key}: {rendered}"
    if re.search(pattern, front):
        return re.sub(pattern, replacement, front, count=1)
    closing = re.search(r"(?m)^---\s*$", front[3:])
    if not closing:
        return front
    idx = 3 + closing.start()
    return f"{front[:idx]}{replacement}\n{front[idx:]}"


def source_for_target(target: Path) -> Path:
    match = LANG_SUFFIX_RE.search(target.name)
    if not match:
        raise ValueError(f"not a translation file: {target}")
    lang = match.group(1)
    source_name = target.name[: -(len(lang) + 4)] + ".md"
    return target.with_name(source_name)


def finalize(target: Path, provenance: str = "ai-reviewed") -> None:
    source = source_for_target(target)
    if not source.exists():
        raise FileNotFoundError(source)

    text = target.read_text(encoding="utf-8")
    front, body = split_front_matter(text)
    src_front, _ = split_front_matter(source.read_text(encoding="utf-8"))

    url = extract_value(src_front, "url")
    if url:
        front = set_scalar(front, "url", url, quoted=True)

    front = set_scalar(front, "translation_provenance", provenance, quoted=False)
    front = set_scalar(front, "translation_lock", "true", quoted=False)
    front = set_scalar(front, "translation_source_sha256", file_hash(source), quoted=False)

    target.write_text(front + body, encoding="utf-8")


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("paths", nargs="+", help="Translation markdown files")
    parser.add_argument("--provenance", default="ai-reviewed")
    args = parser.parse_args()

    for raw in args.paths:
        path = Path(raw)
        if not path.is_absolute():
            path = SITE_ROOT / path
        finalize(path, provenance=args.provenance)
        print(f"[finalize] {path.relative_to(SITE_ROOT)}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
