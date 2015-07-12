## ----eval=FALSE----------------------------------------------------------
#  constructor <- function(...) {
#    ...
#    out <- list(...)
#    class(out) <- "constructor"
#    out
#  }

## ----eval=FALSE----------------------------------------------------------
#  constructor <- function(...) {
#    ...
#    retList("constructor")
#  }

## ------------------------------------------------------------------------
library(aoos)

Person <- function(name) {
  
  print <- function() {
    cat(paste0("Hello, my name is ", .self$name, ".\n"))
  }
  
  retList(c("Person", "Print"))
}

## ------------------------------------------------------------------------
ann <- Person("Ann")
ann

## ------------------------------------------------------------------------
Person <- function(.name) {
  
  print <- function(...) {
    cat(paste0("Hello, my name is ", .self$.name, ".\n"))
  }
  
  name <- function(x) {
    if (!missing(x)) .name <<- x
    .name
  }
  
  retList(c("Person", "Print"))
}

p <- Person("Ann")
p
p$name()
p$name("Paul")
p

## ------------------------------------------------------------------------
Queue <- function(...) {
  
  .queue <- list(...)
  
  add <- function(x) {
    .queue <<- c(.queue, list(x))
    x
  }
  
  remove <- function() {
    if (queueIsEmpty()) return(NULL)
    head <- .queue[[1]]
    .queue <<- .queue[-1]
    head
  }
  
  queueIsEmpty <- function() length(.queue) == 0
  
  retList("Queue")
  
}

HistoryQueue <- function(...) {
  
  .head_idx <- 0
  
  remove <- function() {
    if ((length(.queue) - .head_idx) == 0) return(NULL)
    .head_idx <<- .head_idx + 1
    .queue[[.head_idx]]
    }
  
  print <- function(...) {
      cat("Next item is at index", .head_idx + 1, "\n")
      for (i in seq_along(.queue)) {
        cat(i, ": ", .queue[[i]], "\n", sep = "")
      }
  }
  
  retList(c("HistoryQueue", "Print"), super = Queue(...))
}

q <- Queue(5, 6, "foo")
q$remove()

hq <- HistoryQueue(5, 6, "foo")
hq
hq$remove()
hq
hq$remove()
hq

