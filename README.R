## ----, results='asis', echo=FALSE----------------------------------------
cat(gsub("\\n   ", "", packageDescription("aoos", fields = "Description")))

## ----, eval=FALSE--------------------------------------------------------
#  install.packages("aoos")

## ----, eval=FALSE--------------------------------------------------------
#  library(devtools)
#  install_github("wahani/aoos")

## ----, echo=FALSE--------------------------------------------------------
getLocalPackageUpdates <- function(package) {
  ps <- summary(packageStatus())
  cranVersion <- ps$avail[rownames(ps$avail) == package, "Version"]
  localVersion <- ps$inst[rownames(ps$inst) == package, "Version"]
  cat("Version on CRAN:", cranVersion, "\n")
  cat("Development Version:", localVersion, "\n\n")
  cat("Updates in package NEWS-file since last release to CRAN:\n\n")
  print(news(Version > cranVersion, package = package))
  invisible(NULL)
}

getLocalPackageUpdates("aoos")

