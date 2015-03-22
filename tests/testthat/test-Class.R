context("Class")

test_that("Class wrapper", {
  suppressWarnings(
    Test <- Class({
      
      Class <- "Test"
      
      x <- "character"
      y <- "numeric"
      
      doSomething <- function() {
        .self$y <- y + 1
        cat(x, "\n")
        invisible(.self)
      }
      
    }))
  
  instance <- new("Test", x = "Working", y = 0)
  
  expect_equal(instance$y, 0)
  expect_equal(instance$x, "Working")
  expect_is(instance$doSomething(), "Test")
  expect_equal(instance$y, 1)
  
  # Inheritance
  
  Character <- setClass("Character", contains = "character")
  
  suppressWarnings({
    SubTest <- Class({
      
      Class <- "SubTest"
      contains <- "Test"
      
      x <- "Character"
      xx <- "Test"
      
      doSomething <- function() {
        .self$y <- y + as.numeric(x)
        invisible(.self)
      }
      
    })
  })
    
  instance <- SubTest(x = Character("2"), y = 5)
  
  expect_equal(instance$y, 5)
  expect_equal(instance$x, Character("2"))
  expect_is(instance$doSomething(), "SubTest")
  expect_equal(instance$y, 7)
  expect_is(instance$xx, "Test")
  
  instance$xx$y <- 0
  instance$xx$x <- "Working"
  expect_equal(instance$xx$y, 0)
  expect_equal(instance$xx$x, "Working")
  expect_is(instance$xx$doSomething(), "Test")
  expect_equal(instance$xx$y, 1)
  
  removeClass("Test")
  removeClass("SubTest")
})



