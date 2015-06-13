context("Infix")

test_that("Infix operators for S3", {
  
  Test <- function(.x) {
    force(.x)
    .self <- environment()
    as.environment <- function() .self
    ".+" <- function(e2) Test(getX() + e2)
    ".==" <- function(e2) getX() == e2
    ".>=" <- function(e2) getX() >= e2
    getX <- function() .x
    retList(c("Test", "Infix"))
  }
  
  expect_true(Test(2) + 2 == 4)
  expect_true(Test(2) == 2)
  expect_false(Test(3) == 2)
  expect_true(Test(2) >= 1)
  expect_true(Test(2) >= 2)
  expect_false(Test(2) >= 3)
  
})