knitr::knit("README.Rmd", "README.md")

# for gh-pages
# devtools::install_github("hadley/staticdocs")
devtools::document()
devtools::build_vignettes()
# staticdocs::build_site()

# file.remove(
#   list.files(path = "vignettes", pattern = "*.html", full.names = TRUE)
# )

#system('git push origin --delete gh-pages')
# Windows is strange, you have to copy-paste this:
# system('git add -f inst/web && git commit -m "gh-pages subtree commit"')
# system('git subtree push --prefix inst/web origin gh-pages')
