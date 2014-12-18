context("Public interface")
test_that("Validity function for set", {
  
  test <- suppressWarnings({
    test <- defineClass("test", {
      x <- public(5, function(x) inherits(x, what = "numeric"))
    })
  })
  
  tmp <- test()
  
  expect_equal(tmp$x(), 5)
  expect_is(tmp$x(), "numeric")
  expect_error(tmp$x("s"))
  
  removeClass("test")
})

test_that("publicValue", {
  x <- publicValue()
  expect_is(x(), "NULL")
  expect_equal(x(1), 1)
  expect_equal(x(), 1)
  expect_is(public(function() 1), "publicFunction")
  expect_is(public(1), "publicValue")
})

test_that("Handling of reference classes as public member", {
  
  refObj <- suppressWarnings({
    defineClass("refObj", {
      method <- public(function() cat("...\n"))
      value <- public(1)
    })
  })
  
  refObj2 <- suppressWarnings({
    defineClass("refObj2", {
      refObj <- public(refObj())
    })
  })
  
  ro <- refObj2()
  
  ro$refObj$method()
  ro$refObj$value()
  ro$refObj$value(2)
  ro$refObj$value()
  is(ro$refObj, "refObj")
  
  
  removeClass("refObj")
  removeClass("refObj2")
})