[![Build Status](https://travis-ci.org/wahani/aoos.png?branch=master)](https://travis-ci.org/wahani/aoos)

# Another object orientation system in R
Another implementation of object-orientation in R. Has an 
  interface to S4 referenceClasses and two alternative new implementations. One 
  is an experimental version built oround S4 (defineClass) and the other one 
  (retList) makes it more convenient to work with lists returned from functions 
  and uses only S3.

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
## Error in summary(packageStatus()): Fehler bei der Auswertung des Argumentes 'object' bei der Methodenauswahl
## für Funktion 'summary': Fehler in contrib.url(getOption("repos"), type = type) : 
##   versuche CRAN ohne einen Spiegelserver zu nutzen
```

## Material

- [Introduction Vignette](https://wahani.github.io/aoos/vignettes/Introduction.html)
- [aoosClasses Vignette](https://wahani.github.io/aoos/vignettes/aoosClasses.html)
- [referenceClasses Vignette](https://wahani.github.io/aoos/vignettes/referenceClasses.html)
- [Homepage](https://wahani.github.io/aoos)
- [GitHub](https://github.com/wahani/aoos)

## Posts

- [Introducing v0.1.0](http://wahani.github.io/2015/01/Introducing-Another-Object-Orientation-System/)
