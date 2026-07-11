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

## Agent-readable materials

The rendered site carries no visible links to these (by choice) — this repo
and the machine-readable channels are the entry points. Discovery on the site
itself works via a `<link rel="alternate" type="text/plain" href="/llms.txt">`
tag in the head of every page, a comment in `robots.txt`, and entries in
`sitemap.xml`.

| Material | Repo | Live URL |
| --- | --- | --- |
| LLM-friendly site index ([llmstxt.org](https://llmstxt.org)) | [`llms.txt`](llms.txt) | <https://www.jordanhutchings.com/llms.txt> |
| Visibility and Retail Demand — full text markdown | [`papers/visibility-and-retail-demand/paper.md`](papers/visibility-and-retail-demand/paper.md) | <https://www.jordanhutchings.com/papers/visibility-and-retail-demand/paper.md> |
| Choice architecture ethics (Sci. Reports 2023) — full text markdown | [`papers/choice-architecture-ethics/paper.md`](papers/choice-architecture-ethics/paper.md) | <https://www.jordanhutchings.com/papers/choice-architecture-ethics/paper.md> |
| Structured paper data | [`_data/papers.yml`](_data/papers.yml) | — |

To add a markdown version for a new paper: write `papers/<slug>/paper.md`,
set `paper_md: true` on that paper in `_data/papers.yml`, and render —
`llms.txt` and `sitemap.xml` pick it up automatically (the site's HTML pages
never link it).
