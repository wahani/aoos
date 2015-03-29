## ----, results='asis', echo=FALSE----------------------------------------
cat(gsub("\\n   ", "", packageDescription("aoos", fields = "Description")))

## ----, eval=FALSE--------------------------------------------------------
#  install.packages("aoos")

## ----, eval=FALSE--------------------------------------------------------
#  library(devtools)
#  install_github("wahani/aoos")

## ----, echo=FALSE--------------------------------------------------------
wahaniMiscs::getLocalPackageUpdates("aoos")

