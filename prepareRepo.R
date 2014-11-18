knitr::knit("README.Rmd", "README.md")

# for gh-pages
# devtools::install_github("hadley/staticdocs")
devtools::document()
devtools::build_vignettes()
staticdocs::build_site()
file.remove(
  list.files(path = "vignettes", pattern = "*.html", full.names = TRUE)
)

# Fix for vignettes -- not correctly build by staticdocs
file.copy(list.files(path = "inst/doc/", pattern = "*.html", full.names = TRUE),
          list.files(path = "inst/web/vignettes/", pattern = "*.html", full.names = TRUE),
          overwrite = TRUE)

#system('git push origin --delete gh-pages')
system('git add -f inst/web && git commit -m "gh-pages subtree commit"')
system('git subtree push --prefix inst/web origin gh-pages')
