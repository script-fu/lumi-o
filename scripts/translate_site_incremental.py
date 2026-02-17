#!/usr/bin/env python3

import argparse
import hashlib
import json
import re
from pathlib import Path

try:
    from deep_translator import GoogleTranslator
except Exception:
    GoogleTranslator = None


LANGUAGE_TARGETS = {
    "de": "de",
    "es": "es",
    "fr": "fr",
    "it": "it",
    "ja": "ja",
    "ko": "ko",
    "nl": "nl",
    "pl": "pl",
    "pt-br": "pt",
    "ru": "ru",
    "sv": "sv",
    "th": "th",
    "zh-cn": "zh-CN",
    "zh-tw": "zh-TW",
}

PLACEHOLDER_RE = re.compile(r"@@LUMI_TOKEN_(\d+)@@")


def file_hash(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def split_front_matter(text: str):
    start = 0
    if text.startswith("\ufeff"):
        start = 1

    while start < len(text) and text[start] in " \t\r\n":
        start += 1

    candidate = text[start:]
    match = re.match(r"---\r?\n[\s\S]*?\r?\n---\r?\n?", candidate)
    if not match:
        return "", text

    end = match.end()
    front = text[:start] + candidate[:end]
    body = candidate[end:]
    return front, body


def protect_tokens(text: str):
    patterns = [
        re.compile(r"```[\s\S]*?```", re.MULTILINE),
        re.compile(r"~~~[\s\S]*?~~~", re.MULTILINE),
        # Indented code blocks (Markdown): protect to avoid translating code that isn't fenced.
        re.compile(r"(?m)(?:^(?: {4}|\t).*(?:\n|$))+"),
        re.compile(r"`[^`\n]+`"),
        re.compile(r"\{\{[%<][\s\S]*?[%>]\}\}"),
        re.compile(r"!\[[^\]]*\]\([^\)]+\)"),
        re.compile(r"\[[^\]]*\]\([^\)]+\)"),
        re.compile(r"https?://\S+"),
    ]

    tokens = []

    def repl(match):
        index = len(tokens)
        tokens.append(match.group(0))
        return f"@@LUMI_TOKEN_{index}@@"

    protected = text
    for pattern in patterns:
        protected = pattern.sub(repl, protected)
    return protected, tokens


def safe_translate(translator, text: str, *, label: str) -> str:
    try:
        return translator.translate(text)
    except Exception as exc:
        print(f"[translate] warning: translate failed ({label}): {exc}")
        return text


def translate_card_shortcodes(text: str, translator):
    pattern = re.compile(r'(\{\{<\s*card\b[^>]*\btitle=")([^"]+)("[^>]*>\}\})')

    def repl(match):
        prefix, title, suffix = match.groups()
        if not re.search(r"[A-Za-z]", title):
            return match.group(0)
        translated_title = safe_translate(translator, title, label="card.title")
        translated_title = translated_title.replace('"', "\\\"")
        return f"{prefix}{translated_title}{suffix}"

    return pattern.sub(repl, text)


def restore_tokens(text: str, tokens):
    def repl(match):
        index = int(match.group(1))
        if 0 <= index < len(tokens):
            return tokens[index]
        return match.group(0)

    return PLACEHOLDER_RE.sub(repl, text)


def chunk_text(text: str, max_size: int = 3500):
    blocks = re.split(r"(\n\s*\n)", text)
    chunks = []
    buffer = ""

    for block in blocks:
        if len(buffer) + len(block) > max_size and buffer:
            chunks.append(buffer)
            buffer = block
        else:
            buffer += block

    if buffer:
        chunks.append(buffer)

    return chunks


def translate_body(body: str, translator):
    body = translate_card_shortcodes(body, translator)
    protected, tokens = protect_tokens(body)
    chunks = chunk_text(protected)
    translated_chunks = []

    for chunk in chunks:
        if not re.search(r"[A-Za-z]", chunk):
            translated_chunks.append(chunk)
            continue

        translated_chunks.append(safe_translate(translator, chunk, label="body.chunk"))

    translated = "".join(translated_chunks)
    restored = restore_tokens(translated, tokens)
    restored = re.sub(r"^(#{1,6})\u00A0", r"\1 ", restored, flags=re.MULTILINE)
    return re.sub(r"^(#{1,6})([^\s#])", r"\1 \2", restored, flags=re.MULTILINE)


def translated_path(src_path: Path, lang: str):
    stem = src_path.stem
    return src_path.with_name(f"{stem}.{lang}.md")


def is_translation_file(path: Path):
    return re.search(r"\.[a-z]{2}(?:-[a-z]{2})?\.md$", path.name) is not None


def load_state(path: Path):
    if not path.exists():
        return {"files": {}}
    try:
        return json.loads(path.read_text(encoding="utf-8"))
    except Exception:
        return {"files": {}}


def save_state(path: Path, state):
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(state, indent=2, sort_keys=True), encoding="utf-8")


def parse_args():
    parser = argparse.ArgumentParser(description="Incremental website markdown translation")
    parser.add_argument("--site-root", default=".", help="Website root directory")
    parser.add_argument("--content-dir", default="content", help="Content directory")
    parser.add_argument(
        "--state-file",
        default=".cache/lumi-web-translation-state.json",
        help="State file path relative to site root",
    )
    parser.add_argument(
        "--languages",
        default=",".join(LANGUAGE_TARGETS.keys()),
        help="Comma-separated language codes",
    )
    return parser.parse_args()


def ensure_translated_title(front_matter: str, source_file: Path, translator):
    if not front_matter:
        return front_matter

    source_title = extract_front_matter_title(front_matter)
    if not source_title:
        if source_file.stem == "_index":
            return front_matter
        source_title = re.sub(r"[-_]+", " ", source_file.stem).strip()

    if not re.search(r"[A-Za-z]", source_title):
        return front_matter

    translated_label = safe_translate(translator, source_title, label="front_matter.title").replace('"', "\\\"")

    if re.search(r"(?im)^title\s*:", front_matter):
        return re.sub(
            r"(?im)^title\s*:\s*.*$",
            f'title: "{translated_label}"',
            front_matter,
            count=1,
        )

    if "\r\n" in front_matter:
        newline = "\r\n"
    else:
        newline = "\n"

    closing = f"{newline}---"
    index = front_matter.rfind(closing)
    if index == -1:
        return front_matter

    before = front_matter[:index]
    after = front_matter[index:]
    return f'{before}{newline}title: "{translated_label}"{after}'


def strip_front_matter_keys(front_matter: str, keys: set[str]) -> str:
    """Remove selected top-level YAML keys from a --- front matter block.

    This is used for translated files to avoid cross-language URL collisions.
    """
    if not front_matter:
        return front_matter

    lines = front_matter.splitlines(keepends=True)

    def is_top_level_key(line: str, key: str) -> bool:
        return re.match(rf"(?im)^\s*{re.escape(key)}\s*:.*$", line) is not None and not line.startswith(" ")

    out: list[str] = []
    skip_aliases_block = False

    for line in lines:
        if skip_aliases_block:
            # Stop skipping when we hit a new top-level key or the front matter end.
            if line.startswith("---") or (line.strip() and not line.startswith(" ") and not line.startswith("\t")):
                skip_aliases_block = False
            else:
                continue

        if is_top_level_key(line, "url") and "url" in keys:
            continue

        if is_top_level_key(line, "aliases") and "aliases" in keys:
            # Skip this line and any indented list items that follow.
            skip_aliases_block = True
            continue

        out.append(line)

    cleaned = "".join(out)
    # Clean up accidental double blank lines inside front matter
    cleaned = re.sub(r"\n{3,}", "\n\n", cleaned)
    return cleaned


def extract_front_matter_title(front_matter: str):
    match = re.search(r'(?im)^title\s*:\s*"?(.*?)"?\s*$', front_matter)
    if not match:
        return ""
    return match.group(1).strip()


def dedupe_leading_h1_with_title(body: str, title: str):
    if not title:
        return body

    lines = body.splitlines(keepends=True)
    for index, line in enumerate(lines):
        if line.strip() == "":
            continue

        match = re.match(r'^\s*#\s+(.+?)\s*$', line)
        if not match:
            return body

        heading = match.group(1).strip()
        if heading.casefold() != title.casefold():
            return body

        del lines[index]
        if index < len(lines) and lines[index].strip() == "":
            del lines[index]
        return "".join(lines)

    return body


def main():
    args = parse_args()
    site_root = Path(args.site_root).resolve()
    content_dir = (site_root / args.content_dir).resolve()
    state_path = (site_root / args.state_file).resolve()

    if GoogleTranslator is None:
        print("[translate] deep-translator is not installed. Skipping translation.")
        print("[translate] Install with: pip install deep-translator")
        return 0

    requested_languages = [lang.strip().lower() for lang in args.languages.split(",") if lang.strip()]
    languages = [lang for lang in requested_languages if lang in LANGUAGE_TARGETS]
    if not languages:
        print("[translate] No valid target languages configured.")
        return 0

    state = load_state(state_path)
    files_state = state.setdefault("files", {})

    source_files = [
        path
        for path in content_dir.rglob("*.md")
        if path.is_file() and not is_translation_file(path)
    ]

    source_keys = set()
    translated_count = 0
    skipped_count = 0

    for source_file in source_files:
        rel_key = str(source_file.relative_to(content_dir)).replace("\\", "/")
        source_keys.add(rel_key)
        src_hash = file_hash(source_file)
        previous_hash = files_state.get(rel_key)

        missing_any = False
        for lang in languages:
            if not translated_path(source_file, lang).exists():
                missing_any = True
                break

        if src_hash == previous_hash and not missing_any:
            skipped_count += 1
            continue

        raw_text = source_file.read_text(encoding="utf-8")
        front_matter, body = split_front_matter(raw_text)

        for lang in languages:
            target_code = LANGUAGE_TARGETS[lang]
            translator = GoogleTranslator(source="en", target=target_code)
            translated_front_matter = ensure_translated_title(front_matter, source_file, translator)
            # Avoid language redirect/meta-refresh pages by not pinning translated files to the same URL.
            translated_front_matter = strip_front_matter_keys(translated_front_matter, {"url", "aliases"})
            translated_body = translate_body(body, translator)
            translated_title = extract_front_matter_title(translated_front_matter)
            translated_body = dedupe_leading_h1_with_title(translated_body, translated_title)
            output = f"{translated_front_matter}{translated_body}"
            out_path = translated_path(source_file, lang)
            out_path.parent.mkdir(parents=True, exist_ok=True)
            out_path.write_text(output, encoding="utf-8")

        files_state[rel_key] = src_hash
        translated_count += 1
        print(f"[translate] updated: {rel_key}")

    removed = 0
    stale_keys = [key for key in files_state.keys() if key not in source_keys]
    for key in stale_keys:
        source_path = content_dir / key
        for lang in languages:
            stale_path = translated_path(source_path, lang)
            if stale_path.exists():
                stale_path.unlink()
                removed += 1
        files_state.pop(key, None)

    save_state(state_path, state)
    print(
        "[translate] done: "
        f"updated={translated_count}, skipped={skipped_count}, removed={removed}, langs={len(languages)}"
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
