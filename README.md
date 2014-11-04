[![Build Status](https://travis-ci.org/wahani/aoos.png?branch=master)](https://travis-ci.org/wahani/aoos)

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
      if (queueHasElements()) return(NULL)
      head <- queue[[1]]
      queue <<- queue[-1]
      head
    })
  
  queueHasElements <- function() length(queue) == 0
})

q <- Queue()
q
```

```
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

# Installation

```r
library(devtools)
install_github("wahani/aoos")
```

