context("retList")

test_that("Rational example with retList", {
  Rational <- function(numer, denom) {
    
    gcd <- function(a, b) if(b == 0) a else Recall(b, a %% b)
    
    g <- gcd(numer, denom)
    numer <- numer / g
    denom <- denom / g
    
    print <- function() cat(paste0(numer, "/", denom, "\n"))
    
    add <- function(that) {
      Rational(numer = numer * that$denom + that$numer * denom,
               denom = denom * that$denom)
    }
    
    neg <- function() {
      Rational(numer = -numer,
               denom = denom)
    }
    
    sub <- function(that) {
      add(that$neg())
    }
    
    # Return everything in this scope:
    retList("Rational")
    
  }
  
  rational <- Rational(2, 3)
  expect_is(rational$add(rational), "Rational")
  expect_is(rational$neg(), "Rational")
  expect_is(rational$sub(rational), "Rational")
  expect_true(inherits(rational, "list"))
  
  # Subtyping 
  
  RationalSub <- function(numer, denom) {
    mult <- function(that) {
      RationalSub(.self$numer * that$numer, .self$denom * that$denom)
    }
    retList("RationalSub", "mult", Rational(numer, denom))
  }
  
  rationalSub <- RationalSub(2, 3)
  expect_equal(class(rationalSub), c("RationalSub", "Rational", "list"))
  expect_equal(sort(names(rationalSub)), sort(c(names(rational), "mult")))
  expect_equal(rationalSub$mult(rationalSub)$numer, 4)
  expect_equal(rationalSub$mult(rationalSub)$denom, 9)
  
})

test_that("funNames and retList", {
  
  Person <- function(name) {
    print <- function() cat("Hi, my name is", name)
    retList(c("Person", "Print"))
  }
  
  Employee <- function(id, ...) {
    super <- Person(...)
    print <- function() cat(super$print(), "and my employee id is", id)
    retList("Employee", funNames(), super)
  }
  
  instance <- Employee(1, "Chef")
  expect_true(all(names(instance) %in% c("print", "name")))
  
})

test_that("Inheritance for retList", {
  
  Super <- function(.x) {
    getX <- function() .x
    getY <- function() 1
    retList("Super", "getX")
  }
  
  inheritFrom <- function(.x) {
    newMethod <- function() getY()
    retList("Child", funNames(), super = Super(1))
  }
  
  super <- Super(2)
  expect_equal(super$getX(), 2)
  expect_null(super$getY)
  
  child <- inheritFrom(3)
  expect_equal(child$getX(), 3)
  expect_equal(child$newMethod(), 1)
  
})


