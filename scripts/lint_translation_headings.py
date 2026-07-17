#!/usr/bin/env python3
"""Lint ai-reviewed translation pages for heading structure defects.

Checks (see docs/protocols/web-translation-protocol.md § Validate):

  H1 — English prose headings identical to the English source (after stripping
       inline code in headings).
  H2 — Glued headings: markdown heading lines longer than 80 characters
       (title merged with body text).
  H3 — Fullwidth hash corruption (＃＃＃ instead of ###).
  H4 — Summary glue: ``### Title- `` immediately followed by list content.

Usage:
  python3 scripts/lint_translation_headings.py
  python3 scripts/lint_translation_headings.py --locale es
"""

from __future__ import annotations

import argparse
import re
import sys
from pathlib import Path

# Hugo site locales (filename suffixes); en-gb is the default source.
LOCALES = frozenset(
    {
        "ar",
        "de",
        "es",
        "fr",
        "id",
        "it",
        "ja",
        "ko",
        "nl",
        "pl",
        "pt",
        "pt-br",
        "ru",
        "sv",
        "th",
        "uk",
        "vi",
        "zh-cn",
        "zh-tw",
    }
)

# Headings that legitimately match English in some targets.
VALID_SAME_HEADING = frozenset({"Conclusion", "Conclusione"})

TRANSLATION_FILE_RE = re.compile(r"\.[a-z]{2}(?:-[a-z]{2})?\.md$", re.IGNORECASE)
HEADING_RE = re.compile(r"^(#{1,6})\s+(.+)$")
SUMMARY_GLUE_RE = re.compile(r"^(#{1,6}\s+.{1,60})-\s+(\*\*|`|\S)")


def parse_args():
    parser = argparse.ArgumentParser(description="Lint translation heading structure")
    parser.add_argument("--site-root", default=".", help="Website repository root")
    parser.add_argument("--content-dir", default="content", help="Content directory")
    parser.add_argument(
        "--locale",
        action="append",
        dest="locales",
        help="Limit scan to one or more locale suffixes (repeatable)",
    )
    return parser.parse_args()


def strip_inline_code(text: str) -> str:
    text = re.sub(r"```.*?```", "", text, flags=re.DOTALL)
    text = re.sub(r"`[^`]+`", "", text)
    return text


def extract_headings(text: str) -> list[str]:
    return [m.group(2).strip() for m in HEADING_RE.finditer(text, re.MULTILINE)]


def looks_english_prose(heading: str) -> bool:
    if heading in VALID_SAME_HEADING:
        return False
    if not heading or len(heading) < 4:
        return False
    if re.fullmatch(r"[A-Z0-9_\-/().:]+", heading):
        return False
    if re.fullmatch(r"[a-z][a-z\-]*", heading):
        return False
    if not re.search(
        r"\b(the|and|for|with|your|using|from|into|how|what|when|example|"
        r"best|practices|operations|searching|filtering|validate|display|"
        r"server|manager|working|spaces|random|seed|shipped|wrappers|local|"
        r"remote|repositories|push|changes|basic|steps|make|library|function|"
        r"post|processing|shebang|line|path|building|simple|list|touch|ring|"
        r"wacom|allows|overview|summary|warning|comma|backquote|named)\b",
        heading,
        re.I,
    ):
        return False
    latin = sum(c.isascii() and c.isalpha() for c in heading)
    return latin >= max(3, len(heading) * 0.5)


def locale_from_path(path: Path) -> str | None:
    stem = path.stem
    if "." not in stem:
        return None
    loc = stem.rsplit(".", 1)[-1].lower()
    return loc if loc in LOCALES else None


def is_ai_reviewed(text: str) -> bool:
    return "translation_provenance: ai-reviewed" in text


def lint_file_pair(source: Path, target: Path) -> list[str]:
    issues: list[str] = []
    rel_str = str(target)

    target_text = target.read_text(encoding="utf-8")
    if not is_ai_reviewed(target_text):
        return issues

    source_headings = extract_headings(strip_inline_code(source.read_text(encoding="utf-8")))
    target_headings = extract_headings(strip_inline_code(target_text))

    for src_h, tgt_h in zip(source_headings, target_headings):
        if src_h == tgt_h and looks_english_prose(src_h):
            issues.append(f"H1 english_heading_match: {rel_str}: {src_h!r}")

    for line_no, line in enumerate(target_text.splitlines(), 1):
        if "＃＃＃" in line:
            issues.append(f"H3 fullwidth_hash: {rel_str}:{line_no}: {line[:100]!r}")
            continue
        match = HEADING_RE.match(line)
        if not match:
            continue
        body = match.group(2)
        if len(body) > 80:
            issues.append(f"H2 glued_heading: {rel_str}:{line_no}: {line[:100]!r}")
        elif SUMMARY_GLUE_RE.match(line):
            issues.append(f"H4 summary_glue: {rel_str}:{line_no}: {line[:100]!r}")

    return issues


def iter_source_files(content_dir: Path):
    for path in sorted(content_dir.rglob("*.md")):
        if TRANSLATION_FILE_RE.search(path.name):
            continue
        yield path


def main() -> int:
    args = parse_args()
    site_root = Path(args.site_root).resolve()
    content_dir = (site_root / args.content_dir).resolve()
    locales_filter = {loc.lower() for loc in args.locales} if args.locales else None

    if not content_dir.is_dir():
        print(f"[headings] Content directory not found: {content_dir}", file=sys.stderr)
        return 2

    all_issues: list[str] = []
    for source in iter_source_files(content_dir):
        scan_locales = LOCALES if locales_filter is None else locales_filter
        for loc in sorted(scan_locales):
            target = source.with_name(f"{source.stem}.{loc}.md")
            if not target.is_file():
                continue
            all_issues.extend(lint_file_pair(source, target))

    if not all_issues:
        print("[headings] OK — no heading structure issues found")
        return 0

    print(f"[headings] {len(all_issues)} issue(s):")
    for issue in all_issues:
        print(f"  {issue}")
    return 1


if __name__ == "__main__":
    sys.exit(main())
