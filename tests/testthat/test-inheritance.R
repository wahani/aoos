context("Inheritance:")

test_that("Inheritance", {
  
  suppressWarnings({
    parent <- defineClass("parent", {
      publicMember <- publicValue("?!:")
      privateMember <- NULL
      get <- publicFunction(function() paste(publicMember(), privateMember))
    })
    
    child <- defineClass("child", contains = "parent", {
      set <- publicFunction(function(value) {
        privateMember <<- value
        invisible(privateMember)
      })
    })
  })
  
  tmp <- child()
  expect_equal(tmp$publicMember(), "?!:")
  expect_error(tmp$privateMember) # don't know how to implement that
  expect_equal(tmp$get(), "?!: ")
  expect_equal(tmp$set("s"), "s")
  expect_equal(tmp$get(), "?!: s")
})

test_that("Replacing fields I", {
  
  suppressWarnings({
    parent <- defineClass("parent", {
      privateMember <- NULL
      get <- publicFunction(function() privateMember)
    })
    
    child <- defineClass("child", contains = "parent", {
      privateMember <- "value"
    })  
  })
    
  tmp <- child()
  expect_is(tmp$get(), "NULL")
})

test_that("Replacing fields II", {
  
  suppressWarnings({
    parent <- defineClass("parent", {
      get <- publicFunction(function() foo())
      foo <- publicFunction(function() 1)
    })
    
    child <- defineClass("child", contains = "parent", {
      foo <- publicFunction(function() 2)
    })
  })
    
  tmp <- child()
  expect_equal(tmp$foo(), 2)
  expect_equal(tmp$get(), 1)
})

