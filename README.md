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
## Version on CRAN: 0.2.0 
## Development Version: 0.2.6 
## 
## Updates in package NEWS-file since last release to CRAN:
## 
## Changes in version 0.2.6:
## 
##     o   Renaming Class -> defineRefClass
## 
## Changes in version 0.2.5:
## 
##     o   retList: renamed argument 'exports' to 'public'.
## 
##     o   Possible to define encapsulated unary operators.
## 
##     o   Vignette on classes with reList.
## 
##     o   Vignette on performance.
## 
## Changes in version 0.2.4:
## 
##     o   retList has now an object called .self referring to itself.
## 
## Changes in version 0.2.3:
## 
##     o   retList can now 'inherit' from another list. An extra argument superEnv can be used to really extend a class definition in this framework.
## 
##     o   retList has new arguments to control inheritance. 
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

- [Introduction Vignette](http://htmlpreview.github.io/?https://github.com/wahani/aoos/blob/master/inst/doc/Introduction.html): Is an overview of the things in this package.
- [retList](http://htmlpreview.github.io/?https://github.com/wahani/aoos/blob/master/inst/doc/retListClasses.html): Is what I would recommend to use from this package.
- [Performace Vignette](http://htmlpreview.github.io/?https://github.com/wahani/aoos/blob/master/inst/doc/performance.html): If this is an issue for you.
- [aoosClasses Vignette](http://htmlpreview.github.io/?https://github.com/wahani/aoos/blob/master/inst/doc/aoosClasses.html)
- [referenceClasses Vignette](http://htmlpreview.github.io/?https://github.com/wahani/aoos/blob/master/inst/doc/referenceClasses.html)

## Posts

- [Introducing v0.1.0](http://wahani.github.io/2015/01/Introducing-Another-Object-Orientation-System/)
- [Introducing v0.2.0](http://wahani.github.io/2015/05/Introducing-Another-Object-Orientation-System-2/)

## Examples:

### Simple class:

Basically you define constructor functions. There is no *formal* class definition. The function body will define what members an object will have. You quit the function defining the return value using `retList` which is a *generic* constructor function. By default it will look at the environment from which it is called and convert that environment into a list. That list is returned and is an object. Names with a "." are not part of the constructed *list* (by default).


```r
Employee <- function(.name, .salary) {
  'Common base class for all employees'
  
  print <- function(x, ...) {
    cat("Name  : ", .self$.name, "\nSalary: ", .self$.salary)
  }
  
  getName <- function() .name
  getSalary <- function() .self$.salary
  
  retList(c("Employee", "Print"))
  
}

peter <- Employee("Peter", 5)
peter
```

```
## Name  :  Peter 
## Salary:  5
```

```r
peter$getName()
```

```
## [1] "Peter"
```

```r
peter$getSalary()
```

```
## [1] 5
```

Here every instance is of class *Employee* and also inherits from class *Print*. This enables us to define the print method in the functions body and is equivalent to invoking the print method directly:


```r
peter
```

```
## Name  :  Peter 
## Salary:  5
```

```r
peter$print()
```

```
## Name  :  Peter 
## Salary:  5
```


### Inheritance:

You can inherit methods and fields from a super class, or rather an instance, because there is no *formal* calls definition. Methods and fields can be replaced in the child, all member from the parent are also available for the methods of the child.


```r
Manager <- function(.name, .salary, .bonus) {
  'Extending the Employee class'
  
  bonus <- function(x) {
    if (!missing(x)) .self$.bonus <- x
    .self$.bonus
  }
  
  print <- function(x, ...) {
    cat("Name  : ", .self$.name, "\nSalary: ", .self$.salary, 
        "\nBonus:", .self$.bonus)
  }
  
  retList("Manager", super = Employee(.name, .salary))
  
}

julia <- Manager("Julia", 5, 5 * 1e6)
julia
```

```
## Name  :  Julia 
## Salary:  5 
## Bonus: 5e+06
```

```r
julia$getSalary()
```

```
## [1] 5
```

```r
julia$bonus(10)
```

```
## [1] 10
```

```r
julia
```

```
## Name  :  Julia 
## Salary:  5 
## Bonus: 10
```

### More 

Something you have to keep in mind is that returned objects are of class *list*. If you want to have a public field you have to define get and set methods, because you will see a copy of those fields in the object, they behave more like an attribute.


```r
ObjectWithField <- function(name) {
  getName <- function() {
    name
  }
  retList()
} 

obj <- ObjectWithField("Alexander")
obj$name <- "Noah"
obj$getName()
```

```
## [1] "Alexander"
```

```r
obj$name
```

```
## [1] "Noah"
```

After all this framework is very flexible (as is `R`) and we can do more abstract representations of *things*. In this example I want to create a constructor object which also keeps track of how many instances exist, or rather have been created so far. Also every instance should know how many siblings it has, or in other words all instances share the reference to a field accessible by all of them.


```r
initFamily <- function(.familyName) {
  
  .superEnv <- environment()
  .count <- 0
  
  getCount <- function() {
    cat("There are", .self$.count, paste0(.familyName, "s"), "out there.")
  }
  
  new <- function(.name) {
    # happens on init
    .count <<- .count + 1
    
    print <- function(x, ...) cat("My name is", .self$.name, .familyName, "!")
    countSiblings <- function() cat("I have", .count, "siblings!")
    
    # So every instance knows about .count and .familyName:
    retList(c("Person", "Print"), superEnv = new.env(parent = .superEnv))
  }
  
  retList("ConstructorPerson")
  
}

schmidt <- initFamily("Schmidt")
schmidt$getCount()
```

```
## There are 0 Schmidts out there.
```

```r
lisa <- schmidt$new("Lisa")

sandra <- schmidt$new("Sandra")
schmidt$getCount()
```

```
## There are 2 Schmidts out there.
```

```r
sandra$countSiblings()
```

```
## I have 2 siblings!
```

```r
sandra
```

```
## My name is Sandra Schmidt !
```

```r
lisa
```

```
## My name is Lisa Schmidt !
```
