#!/usr/bin/env python3
"""Lint translation pages for common machine-translation corruption traps.

See docs/protocols/web-translation-protocol.md § Common MT traps and § Validate.

Checks:
  T1 — wrapping.*: ``car`` homograph translated as a vehicle (heading or ***auto*** body).
  T2 — conditionals-when.*: French source pasted into non-fr locales.
  T3 — alists.*: German scheme comments outside de.
  T4 — Zero-width space (U+200B) in prose.
  T5 — English ``Message console`` in scheme comments (non en-gb targets).
  T6 — English UI path ``Mouse and Touchpad`` in install-linux pages.
  T7 — spurious ``Lumi-o`` where the English source says ``Lumi`` only.
  T8 — English UI menu paths (``Edit > Preferences``, ``Properties > Permissions``, …).
  T9 — ``zip`` homograph in ``Download-and-Install.*`` (zipper / artifact MT).
  T10 — English ``;; Define`` scheme comment in non-en pages.
  T11 — ``driver`` homograph (vehicle driver) in ``Wacom-Configuration.*``.

Usage:
  python3 scripts/lint_translation_traps.py
  python3 scripts/lint_translation_traps.py --locale nl
"""

from __future__ import annotations

import argparse
import re
import sys
from pathlib import Path

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

TRANSLATION_FILE_RE = re.compile(r"\.[a-z]{2}(?:-[a-z]{2})?\.md$", re.IGNORECASE)

# Car Replacement section: vehicle words in heading (locale-specific MT of ``car``).
CAR_VEHICLE_HEADING = re.compile(
    r"^###\s+(?:"
    r"Autoersatz|Autovervanging|Bilbyte|"
    r"Reemplazo de automóvil|Remplacement de voiture|Sostituzione auto|"
    r"Substituição de carro|Pengganti Otomatis|"
    r"Замена автомобиля|"
    r"汽车更换|汽車更換|"
    r"車の買い替え|자동차 교체|"
    r"Thay xe|เปลี่ยนรถ"
    r")\s*$",
    re.MULTILINE,
)

CAR_VEHICLE_BODY = re.compile(
    r"\*\*\*(auto|coche|voiture|macchina|samochód|samochodu)\*\*\*",
    re.IGNORECASE,
)

FRENCH_WHEN_LEAK = re.compile(r"est polyvalent, mais sans `else` explicite")

GERMAN_ALIST_COMMENT = re.compile(r";;\s*Alist manuell definieren")

ZWSP = "\u200b"

MESSAGE_CONSOLE = re.compile(r"Message console")

MOUSE_TOUCHPAD = re.compile(r"Mouse and Touchpad")

LUMI_O = re.compile(r"(?<![a-z-])Lumi-o(?![a-z])")

EN_UI_PATH = re.compile(
    r"(?:"
    r"Edit\s*>\s*Preferences|Edit\s*->\s*Preferences|"
    r"Lumi\s*->\s*Edit\s*->|Lumi\s*>\s*Edit\s*>|"
    r"Properties\s*>\s*Permissions|Properties\s*->\s*Permissions|"
    r"Allow executing file as program"
    r")"
)

ZIP_HOMOGRAPH = re.compile(
    r"(?:"
    r"Reißverschluss|ritssluiting|cremallera|blixtlåset|"
    r"молнию|拉开拉链|拉開拉鍊|"
    r"神器.*[Zz]ip|ontwikkeling AppImage|utvecklingen AppImage"
    r")",
    re.IGNORECASE,
)

EN_SCHEME_DEFINE = re.compile(r"^\s*;;\s*Define\b", re.MULTILINE)

DRIVER_VEHICLE = re.compile(
    r"(?:"
    r"conducteur neutre|conducente neutral|conductor neutral|"
    r"bestuurder neutral|föraren neutral|"
    r"нейтральность водителя|"
    r"驾驶员中立|駕駛員中立|"
    r"คนขับให้เป็นกลาง|"
    r"mondiale curve|ทั่วโลกของ Lumi"
    r")",
    re.IGNORECASE,
)

GLUED_LIST_REF_HEADING = re.compile(r"`\)####\s")

FM_RE = re.compile(r"^\ufeff?\s*---\r?\n[\s\S]*?\r?\n---\r?\n?", re.MULTILINE)


def parse_args():
    parser = argparse.ArgumentParser(description="Lint translation MT trap patterns")
    parser.add_argument("--site-root", default=".", help="Website repository root")
    parser.add_argument("--content-dir", default="content", help="Content directory")
    parser.add_argument(
        "--locale",
        action="append",
        dest="locales",
        help="Limit scan to locale suffixes (repeatable)",
    )
    return parser.parse_args()


def locale_of(path: Path) -> str | None:
    name = path.name
    if not TRANSLATION_FILE_RE.search(name):
        return None
    parts = name.rsplit(".", 2)
    if len(parts) < 3:
        return None
    return parts[-2]


def strip_front_matter(text: str) -> str:
    return FM_RE.sub("", text, count=1)


def source_for_target(target: Path, loc: str) -> Path | None:
    source_name = target.name[: -(len(loc) + 4)] + ".md"
    source = target.with_name(source_name)
    return source if source.exists() else None


def iter_translation_files(content_dir: Path, locales: set[str] | None):
    for path in sorted(content_dir.rglob("*.md")):
        loc = locale_of(path)
        if loc is None:
            continue
        if locales and loc not in locales:
            continue
        yield loc, path


def lint_file(loc: str, path: Path) -> list[str]:
    rel = path.as_posix()
    text = path.read_text(encoding="utf-8")
    issues: list[str] = []

    if path.name.startswith("wrapping.") and "Wrapping" in rel:
        if CAR_VEHICLE_HEADING.search(text):
            issues.append("T1: car homograph — vehicle heading in Car Replacement section")
        if CAR_VEHICLE_BODY.search(text):
            issues.append("T1: car homograph — translated ***auto*** (or similar) in body")

    if path.name.startswith("conditionals-when.") and loc != "fr":
        if FRENCH_WHEN_LEAK.search(text):
            issues.append("T2: French prose pasted into non-fr conditionals-when page")

    if path.name.startswith("alists.") and loc != "de":
        if GERMAN_ALIST_COMMENT.search(text):
            issues.append("T3: German scheme comment in non-de alists page")

    if ZWSP in text:
        issues.append("T4: zero-width space (U+200B)")

    if MESSAGE_CONSOLE.search(text):
        issues.append("T5: English 'Message console' leak")

    if "Installing-Debian" in rel and MOUSE_TOUCHPAD.search(text):
        issues.append("T6: English 'Mouse and Touchpad' UI path")

    if LUMI_O.search(strip_front_matter(text)) and "lumi-o" not in rel:
        source = source_for_target(path, loc)
        if source and not LUMI_O.search(
            strip_front_matter(source.read_text(encoding="utf-8"))
        ):
            issues.append("T7: spurious Lumi-o — English source uses Lumi only")

    if EN_UI_PATH.search(text):
        issues.append("T8: English UI menu path leak")

    if path.name.startswith("Download-and-Install.") and ZIP_HOMOGRAPH.search(text):
        issues.append("T9: zip homograph or AppImage MT calque in Download-and-Install")

    if loc not in ("en-gb",) and EN_SCHEME_DEFINE.search(text):
        issues.append("T10: English ';; Define' scheme comment")

    if path.name.startswith("Wacom-Configuration.") and DRIVER_VEHICLE.search(text):
        issues.append("T11: driver homograph — vehicle driver in Wacom-Configuration")

    if path.name.startswith("lists.") and GLUED_LIST_REF_HEADING.search(text):
        issues.append("T11: glued heading — list-ref section merged into prior line")

    if (
        path.name.startswith("the-procedure-browser.")
        and re.search(r"中的\s*####\s*\(", text)
    ):
        issues.append("T11: broken heading — literal #### in procedure-browser prose")

    return [f"{rel}: {msg}" for msg in issues]


def main() -> int:
    args = parse_args()
    root = Path(args.site_root)
    content_dir = root / args.content_dir
    locales = set(args.locales) if args.locales else None

    all_issues: list[str] = []
    for loc, path in iter_translation_files(content_dir, locales):
        all_issues.extend(lint_file(loc, path))

    if all_issues:
        print(f"[traps] {len(all_issues)} issue(s) found:", file=sys.stderr)
        for issue in all_issues:
            print(f"  {issue}", file=sys.stderr)
        return 1

    print("[traps] OK — no MT trap patterns found")
    return 0


if __name__ == "__main__":
    sys.exit(main())
