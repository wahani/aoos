context("S4 generic wrappers")
test_that("Polymorphic classes", {
  
  Poly <- function() {
    generic(x) %g% x
    generic(x = numeric) %m% { x + 1 }
    retList("Poly")
  }
  
  PolyChild <- function() {
    .super <- Poly()
    .super$generic(x = numeric) %m% { x + 2 }
    .super$generic(x = character) %m% { paste(x, "1") }
    retList("PolyChild", super = .super)
  }
  
  poly <- Poly()
  
  expect_equal(poly$generic(2), 3) # numeric-method
  expect_equal(poly$generic("a"), "a") # default
  
  polyChild <- PolyChild()
  
  expect_equal(polyChild$generic(1), 3) # overridden
  expect_equal(polyChild$generic("a"), "a 1") # added
  expect_equal(polyChild$generic(raw(1)), raw(1)) # default
  
})

test_that("generics and methods", {
  
  # standard
  generic(x) %g% { x + 1 }
  expect_equal(generic(1), 2)
  
  # with default
  generic(x = 2) %g% { x + 1 }
  expect_equal(generic(), 3)
  
  # a method
  generic(x = character) %m% { x }
  expect_equal(generic(), 3)
  expect_equal(generic(""), "")
  
  # things with ... and stuff
  generic(x, ...) %g% standardGeneric("generic")
  generic(x = numeric, y = 1, z = 2, a) %m% { x + y + z + a }
  expect_equal(generic(1, a = 2), 6)
  
  generic(x = character, b = "b") %m% paste0(x, b)
  expect_equal(generic("a"), "ab")
  expect_equal(generic("", 2), "2")
  
})
