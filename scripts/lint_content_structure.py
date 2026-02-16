#!/usr/bin/env python3

import argparse
import re
from pathlib import Path


TRANSLATION_FILE_RE = re.compile(r"\.[a-z]{2}(?:-[a-z]{2})?\.md$", re.IGNORECASE)
HEADING_RE = re.compile(r"^(#{1,6})\s+(.+?)\s*$")


def parse_args():
    parser = argparse.ArgumentParser(
        description="Lint Lumi content frontmatter and heading conventions"
    )
    parser.add_argument("--site-root", default=".", help="Website root directory")
    parser.add_argument("--content-dir", default="content", help="Content directory")
    return parser.parse_args()


def split_frontmatter(text: str):
    match = re.match(r"\ufeff?\s*---\r?\n([\s\S]*?)\r?\n---\r?\n?", text)
    if not match:
        return "", text
    return match.group(1), text[match.end() :]


def first_markdown_heading(body: str):
    for line in body.splitlines():
        match = HEADING_RE.match(line)
        if match:
            level = match.group(1)
            text = match.group(2).strip()
            return level, text
    return "", ""


def has_frontmatter_key(frontmatter: str, key: str) -> bool:
    return bool(re.search(rf"(?im)^{re.escape(key)}\s*:", frontmatter))


def iter_source_markdown_files(content_dir: Path):
    for path in sorted(content_dir.rglob("*.md")):
        if TRANSLATION_FILE_RE.search(path.name):
            continue
        yield path


def lint_file(path: Path, rel_path: Path):
    issues = []
    text = path.read_text(encoding="utf-8", errors="ignore")
    frontmatter, body = split_frontmatter(text)

    if not frontmatter:
        issues.append("missing frontmatter")
        return issues

    for key in ("title", "url"):
        if not has_frontmatter_key(frontmatter, key):
            issues.append(f"missing frontmatter key: {key}")

    level, heading_text = first_markdown_heading(body)
    if level and heading_text.casefold() == "introduction":
        issues.append(
            f'first heading should not be "Introduction" (found "{level} {heading_text}")'
        )

    return issues


def main():
    args = parse_args()
    site_root = Path(args.site_root).resolve()
    content_dir = (site_root / args.content_dir).resolve()

    if not content_dir.exists():
        print(f"[lint] Content directory not found: {content_dir}")
        return 2

    all_issues = {}
    for path in iter_source_markdown_files(content_dir):
        rel_path = path.relative_to(site_root)
        issues = lint_file(path, rel_path)
        if issues:
            all_issues[str(rel_path)] = issues

    if all_issues:
        print("[lint] Content structure violations found:")
        for file_path, issues in all_issues.items():
            for issue in issues:
                print(f"- {file_path}: {issue}")
        print(f"[lint] Total files with violations: {len(all_issues)}")
        return 1

    print("[lint] Content structure check passed.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())