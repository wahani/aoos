#' ---
#' title: "aoos vs. R6 vs. reference classes"
#' author: "Seb!"
#' ---

#' This is an adoption from [the R6 performance documentation](http://rpubs.com/wch/17459).
#'
#' ## Definitions

library("R6")
library("aoos")
library("microbenchmark")

R6 <- R6Class("R6",
              public = list(
                x = NULL,
                initialize = function(x = 1) self$x <- x,
                getx = function() self$x,
                inc = function(n = 1) self$x <- x + n
              )
)

RC <- setRefClass("RC", 
                  fields = list(x = "numeric"),
                  methods = list(
                    initialize = function(x = 1) .self$x <- x,
                    getx = function() x,
                    inc = function(n = 1) x <<- x + n
                  )
)

RList <- function(x = 1) {
  self <- environment()
  getx <- function() self$x
  inc <- function(n = 1) self$x <- self$x + n
  out <- list(x = x, getx = getx, inc = inc)
  class(out) <- "RList"
  out
}

RLA <- function(x = 1) {
  getx <- function() .self$x
  inc <- function(n = 1) .self$x <- .self$x + n
  retList("RLA", c("x", "getx", "inc"))
}

AOOS <- defineClass("AOOS", {
  getx <- function() .self$x
  inc <- function(n = 1) .self$x <- .self$x + n
  init <- function(x = 1) .self$x <- x
})

R6Child <- R6Class("R6Child", inherit = R6)
RCChild <- setRefClass("RCChild", contains = "RC")
RLAChild <- function(...) {
  retList("RLAChild", super = RLA(...))
}
AOOSChild <- defineClass("AOOSChild", contains = "AOOS", {})

#' ## Results

microbenchmark(
  AOOS(),
  RC$new(),
  R6$new(),
  RLA(),
  RList()
)

microbenchmark(
  AOOSChild(),
  RCChild$new(),
  R6Child$new(),
  RLAChild()
)

#+ echo=FALSE
# Rprof(tmp <- tempfile())
# out <- replicate(10000, RLAChild(), simplify = FALSE)
# Rprof()
# profileSummary <- summaryRprof(tmp)
# unlink(tmp)
# 
# profileSummary$by.total
