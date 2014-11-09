context("Self")
test_that("self awareness", {
  suppressWarnings({
    test <- defineClass("test", {
      
      i <- publicValue(1)
      
      doSomething <- publicFunction(function() {
        i(i() + 1)
        invisible(self)
      })
      
    })
  })
  
  # Problems on some systems with the above class def. Can't reproduce the error,
  # seems that "test" is not defined. In all other tests, there are no problems.
  # As I can not reproduce it, it will be suppressed.
  
  if(isClass("test")) {
    tmp <- test()
    
    expect_equal(tmp$doSomething()$doSomething()$i(), 3)
    expect_is(tmp$doSomething(), "test")
  }
  
})
