# Lumi-o Website Translation Protocol

**Purpose:** Produce native-quality translations of Lumi-o website pages while
preserving Hugo structure, links, and product terminology.

This protocol is for agent-led, human-reviewed translation. It is not a
bulk-machine-translation workflow.

## Agent Prompt

> Read `docs/protocols/web-translation-protocol.md`. Translate the requested
> English source page into the requested language for a native reader. Read the
> source and existing target page, preserve all Hugo/Markdown structure, use
> existing translations for Lumi terminology, then edit only the target page.
> Mark the completed translation `ai-reviewed` and lock it. Run the required
> checks before reporting completion.

## File Pairing

English source pages use `.md`; language variants use `.<language>.md`.

```text
content/hub/features/palette-map.md       # English source
content/hub/features/palette-map.fr.md    # French translation
```

Do not edit the English source while translating. Do not translate files under
`themes/`; they belong to the bundled Hugo theme.

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

### 2. Preserve site structure exactly

Keep all of the following unchanged unless the text is a clearly translatable
human-facing label:

- YAML front matter keys other than `title` and translation metadata
- Hugo shortcodes, their names, parameters, link targets, and icons
- Markdown headings, tables, code blocks, images, URLs, anchors, and HTML
- file paths, command names, CSS classes, variables, version numbers, and
  product names such as `Lumi-o`, `GIMP`, `Debian`, and `Cinnamon`

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

### 4. Perform a target-language-only final pass

Before marking the page reviewed, read the completed translation without
looking at the English source. Confirm that it reads as native website copy,
not as a translation.

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

When a page establishes a preferred rendering for a recurring product or
technical term, reuse it in related pages. Record terms that need a durable
decision in the language's glossary before translating further pages.

### 5. Mark a reviewed translation as protected

After reviewing the complete page, add or update this front matter:

```yaml
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: <sha256 of the English source file>
```

Use `manual` instead of `ai-reviewed` when a human authored or comprehensively
rewrote the translation.

Compute the hash from the paired English source file (the `.md` file without a
language suffix). When a locked translation already has
`translation_source_sha256`, keep or refresh that value during review.

The incremental translator preserves files with `translation_lock: true`. If
the English page changes, a matching hash reports the locked page as current; a
mismatch reports it as stale without overwriting it. Locked pages without a
hash always appear stale, so always set the hash when marking a page reviewed.

To deliberately regenerate a locked translation:

```bash
python3 scripts/translate_site_incremental.py --site-root . --force-locked
```

### 6. Validate

Run these checks from the website repository:

```bash
python3 scripts/lint_content_structure.py
hugo --minify
```

Also review the diff to confirm that only the intended target page changed and
that no Markdown, shortcode, link, or front-matter structure was damaged.

## Review Checklist

- Is every reader-facing sentence natural in the target language?
- Are technical claims faithful to the English source?
- Are interface and product terms consistent with nearby translated pages?
- Do page titles, card labels, and navigation labels sound like native website
  copy rather than dictionary translations?
- Are technical terms clear to their intended audience, and are pronouns or
  references to groups, layers, and controls unambiguous?
- Has the completed page been read once solely in the target language for
  register, grammar, terminology, and natural flow?
- Do titles, card labels, and links remain valid?
- Is the translation marked with provenance, `translation_lock: true`, and a
  current `translation_source_sha256` for the English source?
- Does the site build successfully?
