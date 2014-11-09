context("Basics:")
test_that("publicValue", {
  x <- publicValue()
  expect_is(x(), "NULL")
  expect_equal(x(1), 1)
  expect_equal(x(), 1)
})

test_that("class-markup", {
  
  suppressWarnings(
    test <- defineClass("test", {
      .privateObject <- NULL
      publicObject <- publicValue("BAM!")
      get <- publicFunction(function() .privateObject)
      set <- publicFunction(function(value) .privateObject <<- value)
      doSomethingWithPublicObject <- publicFunction(function() publicObject())
    })
    )
  
  tmp <- test()
  expect_is(tmp$get(), "NULL")
  expect_equal(tmp$publicObject(), "BAM!")
  expect_error(tmp$.privateObject) # how do implement that?
  expect_equal(tmp$set(2), 2)
  expect_equal(tmp$get(), 2)
  
  expect_equal(tmp$publicObject("jiggle"), "jiggle")
  expect_equal(tmp$doSomethingWithPublicObject(), "jiggle")
  
  # New instance with new environment:
  suppressWarnings(tmp2 <- test())
  expect_is(tmp2$get(), "NULL")
  expect_equal(tmp2$publicObject(), "BAM!")
  
  removeClass("test")
})

test_that("default-call", {
  expect_is(suppressWarnings(defineClass("tmp")), "function")
  expect_true(exists("tmp"))
})

test_that("privacy works", {
  suppressWarnings({
    defineClass("class", {
      private <- 2
      get <- publicFunction(function() 1)
    })
  })
  
  tmp <- class()
  
  expect_equal(tmp$get(), 1)
  expect_error(tmp$private)
  expect_error(tmp$something <- 1)
  
  removeClass("class")
})
