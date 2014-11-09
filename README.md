[![Build Status](https://travis-ci.org/wahani/aoos.png?branch=master)](https://travis-ci.org/wahani/aoos)

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

# Another object orientation system in R
Another implementation of object-orientation in R. Private and public methods are part of the class-definition. Allows to write a lot of small (lines of code) functions as part of the class definition without cluttering the object.

This example is adapted from the [R6](https://github.com/wch/R6) package from the [private members example](http://rpubs.com/wch/24456) to have a direct comparison.


```r
library(aoos)

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

If it is desirable you can access all (public and private) members using `self` inside the class definition. If you think `self$privateMember <- 1` is better writing style than `privateMember <<- 1` you can do that...


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


