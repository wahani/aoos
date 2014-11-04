context("Self")
test_that("self awareness", {
  suppressWarnings({
    defineClass("test", {
      
      i <- publicValue(1)
      
      doSomething <- publicFunction(function() {
        i(i() + 1)
        invisible(self)
      })
      
    })
  })
  
  tmp <- test()
  
  expect_equal(tmp$doSomething()$doSomething()$i(), 3)
  expect_is(tmp$doSomething(), "test")
})
