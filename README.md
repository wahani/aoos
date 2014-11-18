[![Build Status](https://travis-ci.org/wahani/aoos.png?branch=master)](https://travis-ci.org/wahani/aoos)

# Another object orientation system in R
Another implementation of object-orientation in R. Private and public methods are part of the class-definition. Has reference semantics and is build around S4. Hopefully improves readability of your code has auto-complete in RStudio for public member functions.

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
## Development Version: 0.0.4 
## 
## Updates in package NEWS-file since last release to CRAN:
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

# Example I
This example is adapted from the [R6](https://github.com/wch/R6) package from the [private members example](http://rpubs.com/wch/24456) to have a direct comparison.


```r
suppressPackageStartupMessages(library(aoos))

Queue <- defineClass("Queue", {
  
  queue <- list()
  
  add <- publicFunction(function(x) {
      queue <<- c(queue, list(x))
      invisible(self)
    })
  
  remove <- publicFunction(function() {
      if (queueIsEmpty()) return(NULL)
      head <- queue[[1]]
      queue <<- queue[-1]
      head
    })
  
  queueIsEmpty <- function() length(queue) == 0
})

q <- Queue()
q
```

```
## Approximated memory size: 0 (Mib)
## public member:
##   add 
##   remove
```


```r
q$add(5)
q$add("something")
q$add(17)
q$remove()
```

```
## [1] 5
```

```r
q$remove()
```

```
## [1] "something"
```

```r
q$add(1)$add(2)
q$remove()
```

```
## [1] 17
```

```r
q$add(matrix("", ncol = 1000, nrow = 1000))
summary(q)
```

```
##                        Type Size.Mib
## add          publicFunction      0.0
## queue                  list      7.6
## queueIsEmpty       function      0.0
## remove       publicFunction      0.0
## self                  Queue      0.0
```

# The use of the self

This example is also adapted from the [R6](https://github.com/wch/R6) package from the [basics example](http://rpubs.com/wch/24456).

If it is desirable you can access all (public and private) members using `self` inside the class definition. Maybe you want to write `self$privateMember <- 1` instead of `privateMember <<- 1`...


```r
Person <- defineClass("Person", {
    name <- NA
    hair <- NA
    
    set <- publicFunction(function(name, hair) {
      if(!missing(name)) self$name <- name
      if(!missing(hair)) self$hair <- hair
      self$greet()
      invisible(self)
    })
    
    greet = function() {
      cat(paste0("Hello, my name is ", self$name, ".\n"))
    }
})

ann <- Person()
ann$set("Ann", "")
```

```
## Hello, my name is Ann.
```

If you return `self` as in the `set` function -- `self` and the `ann` are both pointing to the same object -- you can chain method calls.


```r
identical(ann, ann$set("not Ann"))
```

```
## Hello, my name is not Ann.
```

```
## [1] TRUE
```

```r
ann$set()$set()
```

```
## Hello, my name is not Ann.
## Hello, my name is not Ann.
```

