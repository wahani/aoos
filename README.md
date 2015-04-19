[![Build Status](https://travis-ci.org/wahani/aoos.png?branch=master)](https://travis-ci.org/wahani/aoos)

# Another object orientation system in R
Another implementation of object-orientation in R. Has reference semantics and is built around S4.

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
## Version on CRAN: 0.1.0 
## Development Version: 0.1.6 
## 
## Updates in package NEWS-file since last release to CRAN:
## 
## Changes in version 0.1.6:
## 
##     o   New class 'Binary' to add binary operators to aoos classes
## 
## Changes in version 0.1.5:
## 
##     o   Bugfix when calling new/constructor without arguments
## 
##     o   Inheritance of non-aoos classes - for S4 Method dispatch
## 
##     o   update to summary method for aoos
## 
##     o   New class 'Show' to easily override default show method for aoos classes
## 
##     o   New class 'Accessor' to override the default accessor for aoos classes
## 
## Changes in version 0.1.4:
## 
##     o   Bugfix in Class when constructing refClass with empty fields
## 
##     o   Updated behaviour of aoos classes. Leading '.' indicates a private member, all other names refer to public member
## 
##     o   New function private to declare a member to be private regardless of name
## 
##     o   Initialise method for aoos classes now properly passes arguments to init, so new can be used for initialisation
## 
##     o   Updated vignettes
## 
## Changes in version 0.1.3:
## 
##     o   New class 'Private' to add restricted access to methods and fields of reference classes
## 
## Changes in version 0.1.2:
## 
##     o   New function Class, which is a wrapper around setRefClass
## 
## Changes in version 0.1.1:
## 
##     o   Bugfix when accessing reference objects.
```

# Material

- [Introduction Vignette](https://wahani.github.io/aoos/vignettes/Introduction.html)
- [aoosClasses Vignette](https://wahani.github.io/aoos/vignettes/aoosClasses.html)
- [referenceClasses Vignette](https://wahani.github.io/aoos/vignettes/aoosClasses.html)
- [Homepage](https://wahani.github.io/aoos)
- [GitHub](https://github.com/wahani/aoos)
