#!/usr/bin/env python3
"""Sync and look up Lumi app UI terms for website translation review."""

from __future__ import annotations

import argparse
import json
import os
import re
import sys
from pathlib import Path

try:
    import yaml
except ImportError:  # pragma: no cover
    yaml = None  # type: ignore

SITE_TO_LUMI = {
    "en-gb": "en_GB",
    "pt-br": "pt_BR",
    "zh-cn": "zh_CN",
    "zh-tw": "zh_TW",
}
LUMI_TO_SITE = {v: k for k, v in SITE_TO_LUMI.items()}

# Common interface terms used on the website but not always in the GUI audit glossary.
EXTRA_PO_MSGIDS = [
    "Layer",
    "Layers",
    "Mask",
    "Brush",
    "Brushes",
    "Filter",
    "Filters",
    "Workspace",
    "Selection",
    "Opacity",
    "Gradient",
    "Pattern",
    "Image",
    "Channel",
    "Alpha channel",
    "Blend mode",
    "Quick Mask",
    "Layer mask",
    "Floating selection",
    "Plug-in",
    "Plug-ins",
    "Drawable",
    "Palette",
]

MNEMONIC_RE = re.compile(r"\(_[A-Za-z]\)")
UNDERSCORE_ACCEL_RE = re.compile(r"(?<=\w)_(\w)")


def default_lumi_root() -> Path:
    env = os.environ.get("LUMI_ROOT")
    if env:
        return Path(env).expanduser().resolve()
    sibling = Path(__file__).resolve().parents[2] / "lumi-dev" / "build" / "lumi"
    if sibling.is_dir():
        return sibling
    return Path("/home/mark/code/lumi-dev/build/lumi")


def site_to_lumi(site_lang: str) -> str:
    return SITE_TO_LUMI.get(site_lang, site_lang)


def lumi_to_site(lumi_lang: str) -> str:
    return LUMI_TO_SITE.get(lumi_lang, lumi_lang)


def clean_ui_translation(text: str) -> str:
    text = MNEMONIC_RE.sub("", text)
    text = UNDERSCORE_ACCEL_RE.sub(r"\1", text)
    return text.replace("_", "").strip()


def parse_po_catalog(path: Path) -> dict[str, str]:
    entries: dict[str, str] = {}
    msgid: str | None = None
    msgstr_parts: list[str] = []
    in_msgstr = False

    for raw_line in path.read_text(encoding="utf-8").splitlines():
        line = raw_line.strip()
        if line.startswith("msgid "):
            if msgid is not None and msgstr_parts:
                entries.setdefault(msgid, "".join(msgstr_parts))
            msgid = json.loads(line[6:])
            msgstr_parts = []
            in_msgstr = False
            continue
        if line.startswith("msgstr "):
            msgstr_parts = [json.loads(line[7:])]
            in_msgstr = True
            continue
        if in_msgstr and line.startswith('"'):
            msgstr_parts.append(json.loads(line))
            continue
        if not line and msgid is not None and msgstr_parts:
            entries.setdefault(msgid, "".join(msgstr_parts))
            msgid = None
            msgstr_parts = []
            in_msgstr = False

    if msgid is not None and msgstr_parts:
        entries.setdefault(msgid, "".join(msgstr_parts))
    return entries


def load_glossary_entries(glossary_path: Path) -> dict[str, dict[str, str]]:
    if yaml is None:
        raise SystemExit("PyYAML is required. Install with: pip install pyyaml")

    data = yaml.safe_load(glossary_path.read_text(encoding="utf-8"))
    entries: dict[str, dict[str, str]] = {}
    for item in data.get("entries", []):
        label = item.get("canonical_label")
        preferred = item.get("preferred_translations") or {}
        if not label or not preferred:
            continue
        translations = {
            lang: clean_ui_translation(info["translation"])
            for lang, info in preferred.items()
            if isinstance(info, dict) and info.get("translation")
        }
        if translations:
            entries[label] = translations
    return entries


def build_glossary(lumi_root: Path) -> dict:
    glossary_yaml = lumi_root / "docs" / "gui_audit" / "canonical" / "translation-glossary.yaml"
    po_dir = lumi_root / "po"
    if not glossary_yaml.is_file():
        raise SystemExit(f"Missing Lumi glossary: {glossary_yaml}")
    if not po_dir.is_dir():
        raise SystemExit(f"Missing Lumi po directory: {po_dir}")

    entries: dict[str, dict] = {}

    for label, translations in load_glossary_entries(glossary_yaml).items():
        entries[label] = {
            "source": "translation-glossary.yaml",
            "translations": translations,
        }

    po_files = {p.stem: p for p in po_dir.glob("*.po")}
    for msgid in EXTRA_PO_MSGIDS:
        if msgid in entries:
            continue
        translations: dict[str, str] = {}
        sources: set[str] = set()
        for lumi_lang, po_path in sorted(po_files.items()):
            catalog = parse_po_catalog(po_path)
            msgstr = catalog.get(msgid, "").strip()
            if msgstr:
                translations[lumi_lang] = clean_ui_translation(msgstr)
                sources.add(po_path.name)
        if translations:
            entries[msgid] = {
                "source": ", ".join(sorted(sources)),
                "translations": translations,
            }

    return {
        "schema_version": 1,
        "entry_count": len(entries),
        "entries": dict(sorted(entries.items(), key=lambda kv: kv[0].lower())),
    }


def cmd_build(args: argparse.Namespace) -> int:
    site_root = Path(args.site_root).resolve()
    lumi_root = Path(args.lumi_root).resolve() if args.lumi_root else default_lumi_root()
    payload = build_glossary(lumi_root)
    out_path = site_root / "scripts" / "app_ui_glossary.json"
    out_path.write_text(json.dumps(payload, ensure_ascii=False, indent=2) + "\n", encoding="utf-8")
    print(f"[build] lumi_root: {lumi_root}")
    print(f"[build] entries: {payload['entry_count']}")
    print(f"[build] wrote: {out_path}")
    return 0


def cmd_lookup(args: argparse.Namespace) -> int:
    site_root = Path(args.site_root).resolve()
    glossary_path = site_root / "scripts" / "app_ui_glossary.json"
    if not glossary_path.is_file():
        print(f"Missing {glossary_path}. Run: python3 scripts/app_ui_glossary.py build", file=sys.stderr)
        return 1

    data = json.loads(glossary_path.read_text(encoding="utf-8"))
    entries: dict[str, dict] = data["entries"]
    lumi_lang = site_to_lumi(args.lang)
    site_lang = args.lang

    terms = args.terms or []
    if not terms:
        print("Provide one or more English terms to look up.", file=sys.stderr)
        return 1

    for term in terms:
        record = entries.get(term)
        if not record:
            print(f"{term}: (not in glossary)")
            continue
        translation = record["translations"].get(lumi_lang, "")
        site_code = lumi_to_site(lumi_lang)
        print(f"{term} [{site_code} / {lumi_lang}]: {translation or '(missing)'}")
        print(f"  source: {record['source']}")
    return 0


def cmd_list(args: argparse.Namespace) -> int:
    site_root = Path(args.site_root).resolve()
    glossary_path = site_root / "scripts" / "app_ui_glossary.json"
    if not glossary_path.is_file():
        print(f"Missing {glossary_path}. Run: python3 scripts/app_ui_glossary.py build", file=sys.stderr)
        return 1

    data = json.loads(glossary_path.read_text(encoding="utf-8"))
    lumi_lang = site_to_lumi(args.lang)
    for label in sorted(data["entries"]):
        translation = data["entries"][label]["translations"].get(lumi_lang, "")
        if translation:
            print(f"{label}\t{translation}")
    return 0


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--site-root", default=".", help="Website repository root")
    parser.add_argument("--lumi-root", help="Lumi source tree (default: LUMI_ROOT or sibling path)")

    sub = parser.add_subparsers(dest="command", required=True)
    sub.add_parser("build", help="Refresh scripts/app_ui_glossary.json from Lumi")
    lookup = sub.add_parser("lookup", help="Look up app UI translations for a site language")
    lookup.add_argument("--lang", required=True, help="Site language code (e.g. id, pt-br, uk)")
    lookup.add_argument("terms", nargs="+", help="English UI terms to look up")
    list_cmd = sub.add_parser("list", help="List all known terms for a site language")
    list_cmd.add_argument("--lang", required=True, help="Site language code")

    args = parser.parse_args()
    if args.command == "build":
        return cmd_build(args)
    if args.command == "lookup":
        return cmd_lookup(args)
    if args.command == "list":
        return cmd_list(args)
    return 1


if __name__ == "__main__":
    raise SystemExit(main())
