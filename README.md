[![Build Status](https://travis-ci.org/wahani/aoos.png?branch=master)](https://travis-ci.org/wahani/aoos)

# Another object orientation system in R
Another implementation of object-orientation in R. Has reference semantics and is built around S4.

## Installation
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
## Version on CRAN: 0.2.0 
## Development Version: 0.2.3 
## 
## Updates in package NEWS-file since last release to CRAN:
## 
## Changes in version 0.2.3:
## 
##     o   retList can now 'inherit' from another list. An extra argument superEnv can be used to really extend a class definition in this framework.
## 
## Changes in version 0.2.2:
## 
##     o   New classes Infix and Print which enable encapsulated definitions of infix operators and print methods for S3 classes.
## 
## Changes in version 0.2.1:
## 
##     o   New functions retList and funNames to work with closures as objects
```

## Material

- [Introduction Vignette](https://wahani.github.io/aoos/vignettes/Introduction.html)
- [aoosClasses Vignette](https://wahani.github.io/aoos/vignettes/aoosClasses.html)
- [referenceClasses Vignette](https://wahani.github.io/aoos/vignettes/referenceClasses.html)
- [Homepage](https://wahani.github.io/aoos)
- [GitHub](https://github.com/wahani/aoos)

## Posts

- [Introducing v0.1.0](http://wahani.github.io/2015/01/Introducing-Another-Object-Orientation-System/)
