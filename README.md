# jordanhutchings.com

Personal academic website, built with [Quarto](https://quarto.org). The site is
pre-rendered locally into `_site/`, which is committed and served verbatim by
Netlify (no build step on Netlify — see `netlify.toml`).

## Updating the site

```sh
quarto render   # regenerates _site/ — commit the changes and push
```

## Papers: single source of truth

The research page and the agent-readable index are both generated from
`_data/papers.yml` — **never edit `research.qmd`'s paper list or `llms.txt` by
hand**. On every `quarto render`, the pre-render hook
`scripts/build-agent-files.R` regenerates:

- `_generated/research-list.md` — included by `research.qmd`
- `llms.txt` — the LLM-friendly site index served at `/llms.txt`
  (see <https://llmstxt.org>)

and the post-render hook `scripts/append-sitemap.R` adds `/llms.txt` and the
markdown paper versions to `sitemap.xml`.

To change a paper status, abstract, or link: edit `_data/papers.yml`, run
`quarto render`, commit. Contact info, bio, public code/data, and teaching
lines for `llms.txt` live in `_data/profile.yml` (keep teaching in sync with
the CV).

## Agent readability

Pages advertise the index via a
`<link rel="alternate" type="text/plain" href="/llms.txt">` tag in the head,
a comment in `robots.txt`, and entries in `sitemap.xml` (no visible on-page
link by choice). Full markdown versions of papers (better than
PDF for AI agents) live at `papers/<slug>/paper.md`; set `paper_md: true` in
`_data/papers.yml` once the file exists and the build links it from both the
research page ("[md]") and `llms.txt`.
