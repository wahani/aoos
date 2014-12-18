[![Build Status](https://travis-ci.org/wahani/aoos.png?branch=master)](https://travis-ci.org/wahani/aoos)

# Another object orientation system in R
Another implementation of object-orientation in R. Has reference semantics and is build around S4. Hopefully improves readability of your code has auto-complete in RStudio for public methods.

# Installation
To install from CRAN:

```r
install.packages("aoos")
```

To install from this repo:

```r
library(devtools)
install_github("wahani/aoos")
```


```
## Version on CRAN: 0.0.3 
## Development Version: 0.0.6 
## 
## Updates in package NEWS-file since last release to CRAN:
## 
## Changes in version 0.0.6:
## 
##     o   Handling of fields inheriting from 'environment'
## 
## Changes in version 0.0.5:
## 
##     o   Use the method public instead of publicFunction and publicValue
## 
##     o   Renaming of class oom to aoos
## 
##     o   Vignette: Introduction
## 
## Changes in version 0.0.4:
## 
##     o   Can define function 'init' which will be called if arguments are supplied to the constructor.
## 
##     o   defineClass has new side effect: S4 method for 'initialize'
## 
##     o   defineClass lost side effect: constructor is not assigned to the class-name
## 
##     o   Constructor functions can be named differnet from the class-name
## 
##     o   S4-constructor 'new' can be used to create a new object
```

# Material

- [Introduction Vignette](https://wahani.github.io/aoos/vignettes/Introduction.html)
- [Homepage](https://wahani.github.io/aoos)
- [GitHub](https://github.com/wahani/aoos)
