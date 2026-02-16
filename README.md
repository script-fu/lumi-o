# Lumi·o Website

Hugo site source for https://lumi-o.org.

## Local development

Requirements:
- Hugo (extended)
- Python 3
- `deep-translator` (`pip install deep-translator`)

### Automatic translation on local preview

When launching preview via:

```bash
gnome-terminal -- /home/mark/code/bash/website/hugo-start-lumi.sh
```

the site now runs an incremental translation pass first:

- scans `content/**/*.md` English source pages
- translates only files that changed since the previous run
- writes language variants like `page.fr.md`, `page.de.md`, etc.
- skips unchanged files for fast subsequent preview starts

Run locally from this directory:

```bash
cd /home/mark/web/lumi
hugo server -D
```

Open: http://localhost:1313/

## Production build

```bash
cd /home/mark/web/lumi
hugo --minify
```

Generated output is written to `public/`.

## Content style check

Lint source markdown pages for required frontmatter keys and to avoid redundant leading "Introduction" headings:

```bash
cd /home/mark/web/lumi
python3 scripts/lint_content_structure.py
```

## Cloudflare Pages

Suggested build settings:
- Framework preset: None
- Build command: `hugo --minify`
- Build output directory: `public`
- Environment variable: `HUGO_VERSION` set to a current extended Hugo release

Custom domain:
- `lumi-o.org`
