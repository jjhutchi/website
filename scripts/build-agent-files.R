# Generates llms.txt and _generated/research-list.md from _data/papers.yml
# and _data/profile.yml, so the research page and the agent-readable index
# can never drift apart.
#
# Runs automatically as the Quarto pre-render hook (see _quarto.yml).
# Manual run: Rscript scripts/build-agent-files.R

suppressPackageStartupMessages(library(yaml))

papers  <- read_yaml("_data/papers.yml")$papers
profile <- read_yaml("_data/profile.yml")
site    <- sub("/$", "", profile$site_url)

abs_url <- function(path) {
  if (is.null(path)) return(NULL)
  if (grepl("^https?://", path)) path else paste0(site, "/", path)
}

md_path <- function(p) sprintf("papers/%s/paper.md", p$slug)

has_md <- function(p) isTRUE(p$paper_md)

# main link for a paper: local PDF if present, else external URL, else NULL
main_link <- function(p) {
  if (!is.null(p$pdf)) p$pdf else p$url
}

join_names <- function(nm) {
  n <- length(nm)
  if (n == 1) nm
  else if (n == 2) paste(nm, collapse = " and ")
  else paste0(paste(nm[-n], collapse = ", "), ", and ", nm[n])
}

coauthors_md <- function(p) {
  if (is.null(p$coauthors)) return(NULL)
  nm <- vapply(p$coauthors, function(a) {
    if (!is.null(a$url)) sprintf("[%s](%s)", a$name, a$url) else a$name
  }, character(1))
  paste0("with ", join_names(nm))
}

coauthors_txt <- function(p) {
  if (is.null(p$coauthors)) return(NULL)
  paste0("with ", join_names(vapply(p$coauthors, `[[`, character(1), "name")))
}

# ---- research page (_generated/research-list.md) ---------------------------

# paper_md files must exist before we link to them
for (p in papers) {
  if (has_md(p) && !file.exists(md_path(p))) {
    stop("papers.yml: paper_md is true for '", p$slug, "' but ", md_path(p),
         " does not exist")
  }
}

status_line <- function(p) {
  parts <- c(
    if (!is.null(p$status)) p$status,
    if (!is.null(p$venue))  sprintf("_**%s**_%s", p$venue,
                                    if (!is.null(p$year)) paste0(", ", p$year) else "")
  )
  if (length(parts)) paste(parts, collapse = ", ")
}

detailed_entry <- function(p) {
  extra <- c(
    if (!is.null(p$ssrn)) sprintf("[SSRN](%s)", p$ssrn),
    if (!is.null(p$code)) sprintf("[%s](%s)",
                                  if (!is.null(p$code_label)) p$code_label else "Code",
                                  p$code)
  )
  c(
    paste0(sprintf("**[%s](%s)**", p$title, main_link(p)), "\\"),
    if (!is.null(coauthors_md(p))) paste0("_", coauthors_md(p), "_\\"),
    paste0(status_line(p), "\\"),
    if (!is.null(p$award)) paste0(p$award, "\\"),
    "<details>",
    paste0("<summary>", paste(c("Abstract", extra), collapse = " | "),
           "</summary>"),
    "",
    trimws(p$abstract),
    "",
    "</details>",
    ""
  )
}

progress_entry <- function(p) {
  c(
    paste0(p$title, ",\\"),
    if (!is.null(coauthors_md(p))) paste0("_", coauthors_md(p), "_,\\"),
    paste0("_", p$status, "_"),
    ""
  )
}

section_of <- function(id) Filter(function(p) identical(p$section, id), papers)

research <- c(
  "<!-- GENERATED FILE - do not edit. -->",
  "<!-- Source: _data/papers.yml, rendered by scripts/build-agent-files.R -->",
  "",
  "## Working papers",
  "",
  unlist(lapply(section_of("working-papers"), detailed_entry)),
  "## Papers in progress",
  "",
  unlist(lapply(section_of("in-progress"), progress_entry)),
  "## Publications",
  "",
  unlist(lapply(section_of("publications"), detailed_entry))
)

dir.create("_generated", showWarnings = FALSE)
writeLines(research, "_generated/research-list.md")

# ---- llms.txt ---------------------------------------------------------------

llms_entry <- function(p) {
  link <- abs_url(main_link(p))
  if (is.null(link)) link <- paste0(site, "/research.html")
  bits <- c(
    if (!is.null(coauthors_txt(p))) paste0(coauthors_txt(p), "."),
    if (!is.null(p$llms_summary)) trimws(p$llms_summary),
    {
      status <- paste(c(p$status, p$venue, p$year), collapse = ", ")
      if (nzchar(status)) paste0("Status: ", status, ".")
    },
    if (!is.null(p$award)) paste0(p$award, ".")
  )
  links <- c(
    if (has_md(p)) sprintf("[Full text (markdown)](%s)", abs_url(md_path(p))),
    if (!is.null(p$ssrn)) sprintf("[SSRN](%s)", p$ssrn),
    if (!is.null(p$code)) sprintf("[%s](%s)",
                                  if (!is.null(p$code_label)) p$code_label else "Code",
                                  p$code)
  )
  paste0("- [", p$title, "](", link, "): ",
         paste(bits, collapse = " "),
         if (length(links)) paste0(" ", paste(links, collapse = " | ")) else "")
}

is_jmp <- vapply(papers, function(p) isTRUE(p$jmp), logical(1))
jmp     <- papers[is_jmp]
wp      <- Filter(function(p) identical(p$section, "working-papers"), papers[!is_jmp])
wip     <- Filter(function(p) identical(p$section, "in-progress"),    papers[!is_jmp])
pubs    <- Filter(function(p) identical(p$section, "publications"),   papers[!is_jmp])

llms <- c(
  paste0("# ", profile$name),
  "",
  paste0("> ", trimws(profile$bio)),
  "",
  paste0("- Email: ", profile$email),
  paste0("- Website: ", site, "/"),
  paste0("- CV (PDF): ", abs_url(profile$cv)),
  paste0("- Google Scholar: ", profile$google_scholar),
  paste0("- GitHub: ", profile$github),
  "",
  "## Job Market Paper",
  "",
  vapply(jmp, llms_entry, character(1)),
  "",
  "## Working Papers",
  "",
  vapply(wp, llms_entry, character(1)),
  "",
  "## Work in Progress",
  "",
  vapply(wip, llms_entry, character(1)),
  "",
  "## Publications",
  "",
  vapply(pubs, llms_entry, character(1)),
  "",
  "## Research Code and Data",
  "",
  vapply(profile$code_data, function(d) {
    paste0("- [", d$name, "](", d$url, ")",
           if (!is.null(d$note)) paste0(": ", d$note) else "")
  }, character(1)),
  "",
  "## Teaching",
  "",
  paste0("- ", unlist(profile$teaching)),
  "",
  "## Site",
  "",
  paste0("- [Research](", site, "/research.html): all papers with abstracts and statuses"),
  paste0("- [Blog](", site, "/blog.html): posts on data, tools, and workflows"),
  paste0("- [Home](", site, "/)"),
  "",
  paste0("Generated from structured data (_data/papers.yml) at build time on ",
         format(Sys.Date()),
         ". Full-text markdown versions of papers are linked inline above.")
)

writeLines(llms, "llms.txt")
cat("wrote llms.txt and _generated/research-list.md\n")
