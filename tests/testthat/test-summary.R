context("Summary")
test_that("Summary", {
  suppressWarnings({
    test <- defineClass("test", {
      x <- publicValue(1)
      y <- NULL
      doSomething <- publicFunction(function() {
        y <<- y + 1
        invisible(self)
      })
    })
  })
  
  instance <- test()
  df <- summary(instance)
  expect_is(df, "data.frame")
  expect_equal(nrow(df), 6)
  expect_equal(rownames(df), c("doSomething", "self", "x", "x.validity", "x.x", "y"))
  
  removeClass("test")
})