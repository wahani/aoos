[![Build Status](https://travis-ci.org/wahani/aoos.png?branch=master)](https://travis-ci.org/wahani/aoos)
[![CRAN Version](http://www.r-pkg.org/badges/version/aoos)](http://cran.rstudio.com/web/packages/aoos)
![](http://cranlogs.r-pkg.org/badges/aoos)
[![codecov.io](https://codecov.io/github/wahani/aoos/coverage.svg?branch=master)](https://codecov.io/github/wahani/aoos?branch=master)

# Another object orientation system in R
Another implementation of object-orientation in R. It provides
syntactic sugar for the S4 class system and two alternative new
implementations. One is an experimental version built around S4
and the other one makes it more convenient to work with lists as objects.

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


## Examples:

### retList:

Basically you define constructor functions. There is no *formal* class
definition. The function body will define what members an object will have. You
quit the function defining the return value using `retList` which is a *generic*
constructor function. By default it will look at the environment from which it
is called and convert that environment into a list. That list is returned and is
an object. Names with a "." are not part of the constructed *list* (by default).


```r
library("aoos")
Employee <- function(.name, .salary) {
  "Common base class for all employees"
  
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

Here every instance is of class *Employee* and also inherits from class *Print*.
This enables us to define the print method in the functions body and is
equivalent to invoking the print method directly:


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


### retList + Inheritance:

You can inherit methods and fields from a super class, or rather an instance,
because there is no *formal* class definition. Methods and fields can be
replaced in the child, all member from the parent are also available for the
methods of the child.


```r
Manager <- function(.name, .salary, .bonus) {
  "Extending the Employee class"
  
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

### S4 generics

As of version 0.3.1 there exist two binary operators, `%g%` and `%m%`, which
link to the S4 system for generic functions. They provide (hopefully) concise
alternatives to `methods::setGeneric` and `methods::setMethod`:


```r
# Standard definition for a generic without default:
numeric : strLength(x, ...) %g% standardGeneric("strLength")

# A method for x:character
strLength(x ~ character, ...) %m% { nchar(x) }

# Kind of the default method for x:ANY
strLength(x ~ ANY, ...) %m% { strLength(as.character(x)) }

# Check that it works:
strLength(123)
```

```
## [1] 3
```

```r
strLength("ab")
```

```
## [1] 2
```

You may have noticed that we also constrained the return value of any method
belonging to the generic `strLength` to be a *numeric*. There exist methods
for objects of type *character* and *ANY* type.

In S4, methods can have defaults for arguments which are not formals of the
generic. Otherwise the defaults of the generic are passed down to its methods.
This is not changed: Define defaults for the generic. If a method has more
arguments than its generic you can define defaults for them. For the *shared*
argument names provide a class name. One exception to the rule is `...` which
can have no type. 

### S4 Types

The following presents the function `%type%` which is a link to S4s `setClass`. 
`%type%` tries to abstract a typical scenario of using `setClass`. 

In the following We define two types. One is *Test* which has two fields, *x*
and *y*. *x* is of type *numeric*, *y* is a *list*. Notice that you can either
define a prototype (a default) for a field (for which the class is inferred), or
you state the class explicitly using `~`.

The second is *Child* and inherits the properties from *Test*. Thus it has
also two fields, *x* and *y*, and in addition we say it inherits from type
*character*. So *Child* is basically a character vector with two attributes:


```r
Test(x ~ numeric, y = list()) %type% {
  stopifnot(length(.Object@x) == 1)
  stopifnot(.Object@x > 0)
  .Object
}

Test : character : Child() %type% .Object

Test(2)
```

```
## An object of class "Test"
## Slot "x":
## [1] 2
## 
## Slot "y":
## list()
```

```r
Child("Hej", x = 2)
```

```
## An object of class "Child"
## [1] "Hej"
## Slot "x":
## [1] 2
## 
## Slot "y":
## list()
```

Notice that the right hand side of the expression is more or less the definition
of the initialization method for a type. Arbitrary operations can be made during 
init, in the above example we formulate some assertions (x > 0 and scalar). The
init method for type *Child* just returns the object itself named `.Object`
(see the help page for `methods::initialize` to understand the naming).

### S4 Type Unions

S4 provides also the possibility to construct type unions which are useful to
allow a type to inherit from different types at the same time, e.g. a type which
can either be a *numeric* or *character*. This feature is not yet complete, but
here are some ways you can use it. For the definition of a type:


```r
'numeric | character' : Either() %type% .Object
Either(1)
```

```
## An object of class "Either"
## [1] 1
```

```r
Either("Hello World!")
```

```
## An object of class "Either"
## [1] "Hello World!"
```

In the definition of a field:


```r
Either(x ~ numeric | character) %type% .Object
Either(1)
```

```
## An object of class "Either"
## Slot "x":
## [1] 1
```

```r
Either("Hello World!")
```

```
## An object of class "Either"
## Slot "x":
## [1] "Hello World!"
```

In the definition of a generic or method:


```r
'numeric | character' : complicatedFunction(x = 1) %g% as.character(x)
complicatedFunction(x ~ character | integer) %m% as.numeric(x)
complicatedFunction()
```

```
## [1] "1"
```

```r
complicatedFunction("1")
```

```
## [1] 1
```

```r
complicatedFunction(1L)
```

```
## [1] 1
```

### Polymorphic Methods in Objects

In contrast to the defaults in S4, `%g%` and `%m%` have side effects in the
environment they are called in. That means you can define generics which are 
local to a function or closure. Nice all by itself but it also extends the
`retList`-idea of representing objects in *R* as demonstrated here:


```r
Class <- function() {
  
  overloaded(x) %g% { 
    cat("This is the default ... \n")
    x 
  } 
  
  overloaded(x ~ numeric) %m% {
    cat("This is the method for 'numeric' values ... \n")
    x
  }
  
  retList("Class")
}

instance <- Class()
instance$overloaded(1)
```

```
## This is the method for 'numeric' values ...
```

```
## [1] 1
```

```r
instance$overloaded("a")
```

```
## This is the default ...
```

```
## [1] "a"
```

The next question is how to inherit or extend an existing generic which is a
member of a class? I am not entirely happy with how this works at the moment,
but this is one way to approach it (which works...):


```r
Child <- function() {
  
  # Normally you would make the call to the parents constructor in the call
  # to retList. But here we need to access the elements directly during init...
  .super <- Class()
  
  # This points %m% to the generic (in .super) which should be extended:
  .super$overloaded(x ~ integer) %m% {
    cat("This is the method for 'integer' values ... \n")
    x
  }
  
  retList("Child", super = .super)
  
}

instance <- Child()
instance$overloaded(1)
```

```
## This is the method for 'numeric' values ...
```

```
## [1] 1
```

```r
instance$overloaded("a")
```

```
## This is the default ...
```

```
## [1] "a"
```

```r
instance$overloaded(1L)
```

```
## This is the method for 'integer' values ...
```

```
## [1] 1
```

### More unsorted ideas

#### Public Fields

Something you have to keep in mind is that returned objects are of class *list*.
If you want to have a public field you have to define get and set methods,
because you will see a copy of those fields in the object, they behave more like
an attribute.


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

We can do more abstract representations of *things*. In this example I want to
create a constructor object which keeps track of how many instances it created.
Also every instance should know how many siblings it has, or in other words all
instances share a reference to a field accessible by all of them.


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

#### Self-contained Method Invocation

These ideas can be combined to construct something object like in S4 with fields
as slots and methods contained in a list (other patterns can be thought of...):


```r
# Some extra stuff to make the instance on which a method is invoked available
# during the call.
setClass("Self", contains = "VIRTUAL")

setMethod("$", "Self", function(x, name) {
  function(...) x[[name]](.Object = x, ...)
})
```

```
## [1] "$"
```

```r
Self : Show : Test : list : Super() %type% {
  method <- function(.Object) .Object@x * 2
  show <- function(.Object) cat("Value for x:", .Object@x)
  S3Part(.Object) <- retList(super = .Object)
  .Object
}

x <- Super(x = 2)
x
```

```
## Value for x: 2
```

```r
x$method()
```

```
## [1] 4
```

And for the S3 retList thing:


```r
"$.Self" <- function(x, name) {
  callIsLocal <- any(sapply(aoos:::envirSearch(list(parent.frame())), 
                            identical, y = attr(x, ".self")))
  if (callIsLocal) {
    obj <- attr(x, ".self")[[name]]
    if (inherits(obj, "function")) function(...) obj(self = x, ...)
    else obj
  }
  else function(...) x[[name]](self = x, ...)
}

"$<-.Self" <- function(x, name, value) {
  callIsLocal <- any(sapply(aoos:::envirSearch(list(parent.frame())), 
                            identical, y = attr(x, ".self")))
  
  if (callIsLocal) {
    assign(name, value = value, envir = attr(x, ".self"))
  } else {
    stop("This method has been disabled for objects of class 'Self'.")
  }
  
  x
}

Self <- function(.x) {
  
  setX <- function(self, val) {
    self$.x <- val 
    invisible(self)
  }
  
  getX <- function(self) {
    self$.x
  }
  
  retList(c("Self"))

}

inst <- Self(2)
inst$setX(3)$setX(4)
inst$getX()
```

```
## [1] 4
```

```r
inst$.x <- 2
```

```
## Error in `$<-.Self`(`*tmp*`, ".x", value = 2): This method has been disabled for objects of class 'Self'.
```

#### Syntactic Sugar for Return List


```r
Person <- function(.name) {
  
  print <- function(x, ...) {
    cat(paste0("Hello, my name is ", .name, ".\n"))
  }
  
  retList(c("Person", "Print"))
  
}

Employee <- function(.id, ...) {
  
  print <- function(x, ...) {
    cat("Name: ", .name, "\nID:   ", .id)
  }
  
  retList("Employee") %inherit% Person(...)
  
}

"%inherit%" <- function(child, parent) {
  retList(class(child), names(child), super = parent, mergeFun = envCopy, list2env(child))
}

kalle <- Employee("1", "Kalle")
str(kalle)
```

```
## List of 1
##  $ print:function (x, ...)  
##   ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 13 12 15 3 12 3 13 15
##   .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x3389470> 
##  - attr(*, ".self")=<environment: 0x5c3ac68> 
##  - attr(*, "class")= chr [1:5] "Employee" "list" "Person" "Print" ...
```

```r
class(kalle)
```

```
## [1] "Employee" "list"     "Person"   "Print"    "list"
```

