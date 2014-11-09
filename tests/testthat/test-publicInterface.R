context("Public interface")
test_that("Validity function for set", {
  
  suppressWarnings({
    test <- defineClass("test", {
      x <- publicValue(5, function(x) inherits(x, what = "numeric"))
    })
  })
  
  tmp <- test()
  
  expect_equal(tmp$x(), 5)
  expect_is(tmp$x(), "numeric")
  expect_error(tmp$x("s"))
  
  removeClass("test")
})
