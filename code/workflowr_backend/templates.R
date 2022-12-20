
templates <- list(
  .gitattributes = '
# Classify R Markdown files as R code for GitHub language statistics
# https://github.com/github/linguist#overrides
*.[Rr]md linguist-language=R
',
  .gitignore = '
.Rproj.user
.Rhistory
.RData
.Ruserdata
.Rapp.history
.DS_Store
analysis/figure
analysis/*png
analysis/*html
analysis/*_cache
analysis/site_libs
',
#   .Rprofile = '
# ## This makes sure that R loads the workflowr package
# ## automatically, everytime the project is loaded
# if (requireNamespace("workflowr", quietly = TRUE)) {{
#   message("Loading .Rprofile for the current workflowr project")
#   library("workflowr")
# }} else {{
#   message("workflowr package not installed, please run install.packages(\\"workflowr\\") to use the workflowr functions")
# }}
# ',
#   `_workflowr.yml` = '
# # workflowr options
#
# # The seed to use for random number generation. See ?set.seed for details.
# seed: 10000
# # The working directory to build the R Markdown files. The path is relative to
# # _workflowr.yml. See ?rmarkdown::render for details.
# knit_root_dir: "."
# ',
  `analysis/_site.yml` = '
name: "test"
output_dir: ../docs
pre-render: data/data_cleaning
navbar:
  title: "test"
  left:
  - text: Home
    href: index.html
  - text: About
    href: about.html
  # - text: License
  #   href: license.html
output:
  workflowr::wflow_html:
    toc: yes
    toc_float: yes
    theme: cosmo
    highlight: textmate
',
  `analysis/index.qmd` = '
---
title: "Home"
site: workflowr::wflow_site
output:
  workflowr::wflow_html:
    toc: false
editor_options:
  chunk_output_type: console
---
Welcome to my research website.
',
  `analysis/about.qmd` = '
---
title: "About"
output:
  workflowr::wflow_html:
    toc: false
editor_options:
  chunk_output_type: console
---
Describe your project.
',
#   `analysis/license.qmd` = '
# ---
# title: "License"
# output:
#   workflowr::wflow_html:
#     toc: false
# editor_options:
#   chunk_output_type: console
# ---
# What license are you using for your code? See [choosealicense.com][choose] for
# help deciding. It\'s a convention to save a file `LICENSE` in the root of your
# Git repo that contains the license text.
# What license are you using for the written content on your site? It is
# traditional to choose a [Creative Commons][cc] license for this type of content.
# [choose]: https://choosealicense.com/
# [cc]: https://creativecommons.org/choose/
# How should others cite your work? It\'s a convention to save a file `CITATION`
# in the root of your Git repo that contains the citation information.
# ',
  `code/README.md` = '
# Code
Save command-line scripts and shared R code here.
',
  `data/README.md` = '
# Data
Save raw data files here.
',
  `output/README.md` = '
# Output
Save processed data files here.
',
  README.md = '
# test
,
  "Rproj" = '
Version: 1.0
RestoreWorkspace: No
SaveWorkspace: No
AlwaysSaveHistory: Yes
EnableCodeIndexing: Yes
UseSpacesForTab: Yes
NumSpacesForTab: 2
Encoding: UTF-8
RnwWeave: Sweave
LaTeX: pdfLaTeX
AutoAppendNewline: Yes
StripTrailingWhitespace: Yes
'
)
