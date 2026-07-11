# Post-render hook (see _quarto.yml): adds llms.txt and the markdown paper
# versions to _site/sitemap.xml, which Quarto only populates with HTML pages.

suppressPackageStartupMessages(library(yaml))

smap <- "_site/sitemap.xml"
if (!file.exists(smap)) {
  stop("sitemap.xml not found - is site-url set in _quarto.yml?")
}

papers <- read_yaml("_data/papers.yml")$papers
site   <- sub("/$", "", read_yaml("_data/profile.yml")$site_url)

extras <- c(
  paste0(site, "/llms.txt"),
  unlist(lapply(papers, function(p) {
    if (isTRUE(p$paper_md)) sprintf("%s/papers/%s/paper.md", site, p$slug)
  }))
)

xml <- readLines(smap, warn = FALSE)
missing <- extras[!vapply(extras, function(u) any(grepl(u, xml, fixed = TRUE)),
                          logical(1))]
if (length(missing)) {
  entries <- sprintf("  <url>\n    <loc>%s</loc>\n  </url>", missing)
  xml <- sub("</urlset>", paste(c(entries, "</urlset>"), collapse = "\n"),
             paste(xml, collapse = "\n"))
  writeLines(xml, smap)
}
cat("sitemap.xml includes", length(extras), "agent-readable URLs\n")
