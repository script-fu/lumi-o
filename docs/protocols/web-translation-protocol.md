# Lumi-o Website Translation Protocol

**Purpose:** Produce native-quality translations of Lumi-o website pages while
preserving Hugo structure, links, and product terminology.

This protocol is for agent-led, human-reviewed translation. It is not a
bulk-machine-translation workflow.

## Agent Prompt

> Read `docs/protocols/web-translation-protocol.md`. Translate the requested
> English source page into the requested language for a native reader. Read the
> English source and the existing target page side by side, preserve all
> Hugo/Markdown structure, use existing translations for Lumi terminology, then
> edit **only** the target page manually. Do **not** run the incremental
> translator on locked or `ai-reviewed` pages. When review is complete, run
> `python3 scripts/finalize_reviewed_translation.py` on the target file (or set
> provenance, lock, and source hash by hand), then run all validation checks
> before reporting completion.

## File Pairing

English source pages use `.md`; language variants use `.<language>.md`.

```text
content/hub/features/palette-map.md       # English source
content/hub/features/palette-map.fr.md    # French translation
```

Do not edit the English source while translating. Do not translate files under
`themes/`; they belong to the bundled Hugo theme.

## Supported Languages

The website mirrors Lumi's 20 supported locales. English (UK) is the default
source language (`en-gb`); the other 19 use translated filename suffixes.

| Site code | Lumi code | Language |
|-----------|-----------|----------|
| `en-gb` | `en_GB` | English (UK) — source |
| `ar` | `ar` | Arabic |
| `de` | `de` | German |
| `es` | `es` | Spanish |
| `fr` | `fr` | French |
| `id` | `id` | Indonesian |
| `it` | `it` | Italian |
| `ja` | `ja` | Japanese |
| `ko` | `ko` | Korean |
| `nl` | `nl` | Dutch |
| `pl` | `pl` | Polish |
| `pt` | `pt` | Portuguese (Portugal) |
| `pt-br` | `pt_BR` | Portuguese (Brazilian) |
| `ru` | `ru` | Russian |
| `sv` | `sv` | Swedish |
| `th` | `th` | Thai |
| `uk` | `uk` | Ukrainian |
| `vi` | `vi` | Vietnamese |
| `zh-cn` | `zh_CN` | Chinese (Simplified) |
| `zh-tw` | `zh_TW` | Chinese (Traditional) |

Site codes use lowercase Hugo filename suffixes (`page.pt-br.md`). Lumi gettext
catalogues use underscore forms (`pt_BR.po`). Keep both lists aligned with
`hugo.toml`, `scripts/translate_site_incremental.py`, and Lumi's `LINGUAS`
files.

New languages need entries in `hugo.toml` and `LANGUAGE_TARGETS` in
`scripts/translate_site_incremental.py` before translation work begins.
Add a matching site i18n file at `i18n/<lang>.yaml` with the menu keys from
`i18n/en-gb.yaml` (navbar and sidebar labels use these via `T` lookups).
Existing pages do not need immediate retranslation; run the incremental
translator or follow this protocol page-by-page.

**Note:** `scripts/translate_site_incremental.py` maps both `pt` and `pt-br` to
the same machine-translation target (`pt`). Use this protocol's manual workflow
for European and Brazilian Portuguese; do not rely on the incremental translator
to maintain distinct `pt` and `pt-br` wording.

## Translation Provenance

| Value | Meaning |
|-------|---------|
| `machine` | Written by `translate_site_incremental.py`; draft only — **not reviewed** |
| `ai-reviewed` | Agent- or human-reviewed against the English source; native-quality target copy |
| `manual` | Human-authored or comprehensively rewritten without machine draft |

`ai-reviewed` means a **source↔target review** produced native-quality copy, not
that the incremental translator ran. Never tag a page `ai-reviewed` without that
review.

**Locked pages:** Files with `translation_lock: true` must be edited manually in
the target language. Do **not** bulk re-translate them with the incremental
translator. Fix defects (headings, terminology, glued text) by hand, then
re-finalize.

The incremental translator skips locked files by default. Use `--force-locked`
only when deliberately regenerating a draft from English — then review and
re-finalize before marking `ai-reviewed` again.

## Translation Workflow

### 1. Read context first

Read:

1. The English source page.
2. The existing target translation, if present.
3. Related pages in the same language for established names and tone.
4. Existing occurrences of product and interface terms in that language.

Translate meaning, not sentence structure. The target should read as original
website copy written for a native reader: clear, direct, and appropriate for a
professional digital-painting application.

Before writing, identify titles, card labels, navigation labels, and technical
feature names. These short, prominent strings need an idiomatic target-language
equivalent, not necessarily the closest dictionary translation.

For Lumi interface and tool terms, cross-check the app gettext catalogues and
the canonical GUI glossary before choosing wording. See Lumi's
[`translation-system.md`](https://gitlab.gnome.org/pixelmixer/lumi-dev/-/blob/main/docs/protocols/translation-system.md)
for the full app workflow; the website should follow the same terminology
authority.

**Terminology hierarchy** (highest priority first):

1. **`translation-glossary.yaml`** — generated by Lumi's `build-glossary` from
   `canonical-terminology.yaml`; preferred translations for controlled GUI
   labels, aggregated across all active gettext domains (`po`, `po-gegl`,
   `po-plug-ins`, `po-scheme`, etc.)
2. **Domain `.po` catalogues** — for strings not in the glossary (e.g. general
   nouns like `Layer`, menu paths, plug-in names in `plug-ins/lumi/po/`)
3. **Existing website copy** in the same language — keep internal consistency

**Quick lookup** (after syncing the local glossary cache):

```bash
python3 scripts/app_ui_glossary.py --lumi-root /path/to/lumi build
python3 scripts/app_ui_glossary.py lookup --lang id Layer Brush
python3 scripts/app_ui_glossary.py list --lang uk
```

`build` reads Lumi's `docs/gui_audit/canonical/translation-glossary.yaml` and
common `po/*.po` msgids, then writes `scripts/app_ui_glossary.json`. Set
`LUMI_ROOT` or pass `--lumi-root` when the Lumi tree is not at the default
path. Re-run `build` after Lumi runs `build-glossary` or other translation
updates that change preferred terms.

**Direct source files** in the Lumi repository:

- `docs/gui_audit/canonical/translation-glossary.yaml` — audited preferred
  translations for controlled GUI terms (primary reference)
- `po/<lang>.po` — main application UI
- Other domains when relevant: `po-gegl/`, `po-plug-ins/`, `po-scheme/`,
  `plug-ins/lumi/po/` (see *Active domains* in `translation-system.md`)

Use site language codes with the lookup script (`id`, `pt-br`, `uk`). These map
to Lumi gettext codes (`id`, `pt_BR`, `uk`) automatically.

Prefer the app translation when a page describes the same feature or control.
Keep established product names (`Palette Map`, `Mixer`) and intentional English
loanwords when the app or existing website copy already uses them. Document
deliberate exceptions in review notes when website prose needs a different term
for readability (for example, shell **scripts** vs UI **Script** menu paths).

### 2. Preserve site structure exactly

Keep all of the following unchanged:

- YAML front matter keys other than `title` and translation metadata
- Hugo shortcodes, their names, parameters, link targets, and icons
- Heading **levels** (`#`, `##`, …) and section order
- Tables, code blocks, images, URLs, anchors, and HTML
- File paths, command names, CSS classes, variables, version numbers, and
  product names such as `Lumi-o`, `GIMP`, `Debian`, and `Cinnamon`

**Headings:** Translate reader-facing heading **text** into natural target-language
prose. Preserve inline code and product names inside headings when the source does.
Do not leave English prose headings verbatim unless the source heading is an
intentional product or preset name (for example `Sumi Preset`). After editing,
run `lint_translation_headings.py` to catch English leaks, glued headings, and
fullwidth-hash corruption.

Translate visible shortcode labels, such as `title`, `subtitle`, and `tag`.
Keep user-interface labels in their established localized or product spelling.

In code blocks, preserve executable code exactly: identifiers, literals,
shebangs, function names, and string arguments. Localize `;;` / `;` comments and
`#` shell comments in every target language. Translate comments separately for
each target language; never copy comment text from another language file.

For scripting pages, maintain `scripts/scripting_comment_glossary.json` and
apply it with:

```bash
python3 scripts/apply_scripting_comment_glossary.py
```

### 3. Write for native reading

- Avoid literal calques and English word order.
- Keep terminology consistent across related pages.
- Use the target language’s normal punctuation, typography, and capitalization.
- Preserve the source’s claims and scope; do not add features, promises, or
  technical detail.
- Keep an intentional technical term in English when that is how Lumi presents
  it in the interface or existing localized content.
- Prefer established, reader-friendly technical phrasing over literal calques.
  For example, use the target language’s normal term for a saved composite
  image, rather than translating “composite” word-for-word.
- Make references unambiguous. If a sentence refers to a collapsed group, make
  clear whether it is the group, its layers, or its placeholders that changes
  when it is expanded.
- Use conventional website navigation labels. Do not translate “About”,
  “Features”, or similar labels word-for-word when the target language has a
  more natural convention.

For French, prefer idiomatic terms such as `chargement différé` over a literal
translation of “lazy loading”. Preserve established Lumi names, including
`Palette Map` and `Mixer`, when they identify interface features.

**Common machine-translation traps** (fix manually during review):

- **Scheme** — the scripting language is always **Scheme**, never *Schema*
  (German), *schéma* (French, when meaning the language), *esquema* (Spanish),
  or similar calques.
- **Homographs** — *schema* / *schéma* / *esquema* may be correct when the
  source means a diagram or data layout; compare the English source before
  changing.
- **Product names** — use `Lumi` where the source and app UI do; do not invent
  variants such as `Lumi-o` unless the English source uses that spelling.
- **Technical calques** — literal renderings of “dynamic typing”, “shell script”,
  “function call”, “touch ring”, and similar phrases often need idiomatic
  rewrites; check sibling pages in the same language.
- **Glued headings** — machine output sometimes merges a heading with the first
  sentence (`### TitleBody text…`); split and translate the heading separately.
- **Homograph corruption** — Scheme function names must not be translated as
  ordinary words. The English tutorial *Car Replacement* refers to the `car`
  function, not a vehicle; the same applies to *Random Seed* (RNG seed, not
  plant seed) and similar terms. Scan `wrapping.*` and `Download-and-Install.*`
  especially; also *driver* in `Wacom-Configuration.*` (OS tablet driver, not a
  vehicle driver).
- **Product name drift** — use `Lumi` where the English source says `Lumi`;
  do not substitute `Lumi-o` unless the English page does (the about page slug
  is `lumi-o`, but the product name in prose is usually `Lumi`).
- **Wrong-locale paste** — entire pages or paragraphs copied into the wrong
  language file (for example French prose in `conditionals-when.it.md`).
- **Comment locale** — scheme/shell comments in `;;` / `#` lines must match the
  target language; do not leave German or English comments in non-de/en pages.
- **Zero-width spaces** — MT tools sometimes insert U+200B after words like
  *that*; strip them during review.

Run the trap linter after heading and structure checks:

```bash
python3 scripts/lint_translation_traps.py
python3 scripts/lint_translation_traps.py --locale pl
```

It reports:

- **T1** — `car` homograph (vehicle heading or `***auto***` in `wrapping.*`)
- **T2** — French `conditionals-when` leak in non-`fr` files
- **T3** — German `alist` comments outside `de`
- **T4** — zero-width space (U+200B)
- **T5** — English `Message console` in comments or prose
- **T6** — English `Mouse and Touchpad` UI path in install guides
- **T7** — spurious `Lumi-o` where the English source says `Lumi` only
- **T8** — English UI menu paths (`Edit > Preferences`, `Properties > Permissions`, …)
- **T9** — `zip` homograph or AppImage calque in `Download-and-Install.*`
- **T10** — English `;; Define` scheme comment in non-en pages
- **T11** — `driver` homograph in `Wacom-Configuration.*`; glued `list-ref`
  heading in `lists.*`; broken `####` in `the-procedure-browser.*`

Fix reported issues manually (or with `scripts/fix_translation_traps.py` for
known bulk patterns), re-finalize, and re-run until clean.

### 4. Review: source↔target, then target-only

**Source↔target pass** — With the English source and target open together:

- Confirm section count, heading levels, tables, shortcodes, and code blocks
  match structurally.
- Compare each heading and title: translated prose, not English copy-paste.
- Check fidelity of technical claims, steps, and warnings.
- Scan for English sentences or labels left in the target body.
- Verify front matter: `title` translated; `url` copied from source (finalize
  script does this).

**Target-only pass** — Then read the completed translation without looking at
the English source. Confirm that it reads as native website copy, not as a
translation.

Check in particular:

- Page titles, card labels, and navigation labels use the target language's
  normal website conventions.
- The register is consistent with nearby pages: do not mix formal and informal
  address within a language.
- Product and interface names follow their established target-language spelling.
- Technical descriptions use clear, reader-friendly terms for lazy loading,
  saved composite images, placeholders, layers, groups, and autosave.
- Pronouns and references make clear whether an action applies to a group, its
  layers, masks, or placeholders.
- Grammar, number agreement, punctuation, and capitalization are natural for
  the target language.
- Headings read naturally in the target language (no glued text, no fullwidth
  `#` characters, no English prose leaks).

When a page establishes a preferred rendering for a recurring product or
technical term, reuse it in related pages. Record terms that need a durable
decision in the language's glossary before translating further pages.

For periodic quality audits, spot-check random source↔target pairs across
locales using the same criteria above plus `lint_translation_headings.py` and
`lint_translation_traps.py`.

### 5. Mark a reviewed translation as protected

After reviewing the complete page, add or update this front matter:

```yaml
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: <sha256 of the English source file>
```

Or run:

```bash
python3 scripts/finalize_reviewed_translation.py content/path/to/page.<lang>.md
```

after editing the body — it copies `url` from the English source and stamps the
metadata fields above.

Use `manual` instead of `ai-reviewed` when a human authored or comprehensively
rewrote the translation.

Compute the hash from the paired English source file (the `.md` file without a
language suffix). When a locked translation already has
`translation_source_sha256`, keep or refresh that value during review.

The incremental translator preserves files with `translation_lock: true`. If
the English page changes, a matching hash reports the locked page as current; a
mismatch reports it as stale without overwriting it. Locked pages without a
hash always appear stale, so always set the hash when marking a page reviewed.

To deliberately regenerate a locked translation from English (draft only):

```bash
python3 scripts/translate_site_incremental.py --site-root . --force-locked
```

Then perform a full manual review and re-finalize; do not leave `--force-locked`
output marked `ai-reviewed`.

### 6. Validate

Run these checks from the website repository (all must pass):

```bash
python3 scripts/lint_translation_headings.py
python3 scripts/lint_translation_traps.py
python3 scripts/lint_content_structure.py
hugo --minify
```

`lint_translation_headings.py` scans ai-reviewed translation pages and reports:

- **H1** — English prose headings still identical to the English source
- **H2** — Glued headings (heading line merged with body text; usually >80 chars)
- **H3** — Fullwidth `#` corruption (`＃＃＃`)
- **H4** — Summary sections glued to the first bullet (`### Title- **`)

Optional filters: `--locale es` or `--path content/hub/features/page.es.md`.

Fix any reported issues manually in the target language; do not bulk re-translate
locked pages. Re-run until the linter passes, then set provenance and lock.

`lint_content_structure.py` checks **English source** pages only (for example
missing `url` in front matter); it does not lint translation files.

Also review the diff to confirm that only the intended target page changed and
that no Markdown, shortcode, link, or front-matter structure was damaged.

## Review Checklist

- Has the page been compared **source↔target** for structure, headings, and
  fidelity?
- Is every reader-facing sentence natural in the target language?
- Are technical claims faithful to the English source?
- Are interface and product terms consistent with nearby translated pages and
  with Lumi app UI translations (`scripts/app_ui_glossary.py lookup`)?
- Is **Scheme** spelled correctly (not confused with schema/diagram terms)?
- Do page titles, card labels, and navigation labels sound like native website
  copy rather than dictionary translations?
- Are technical terms clear to their intended audience, and are pronouns or
  references to groups, layers, and controls unambiguous?
- Has the completed page been read once solely in the target language for
  register, grammar, terminology, and natural flow?
- Do titles, card labels, and links remain valid?
- For locked or `ai-reviewed` pages, were edits **manual** (not bulk MT)?
- Is the translation marked with provenance, `translation_lock: true`, and a
  current `translation_source_sha256` for the English source?
- Do headings pass `python3 scripts/lint_translation_headings.py` (no H1–H4 issues)?
- Do pages pass `python3 scripts/lint_translation_traps.py` (no T1–T6 MT traps)?
- Does the site build successfully?
