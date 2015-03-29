## ----, eval=FALSE--------------------------------------------------------
#  vignette("aoosClasses", "aoos")

## ----, echo=FALSE--------------------------------------------------------
removeClass("PersonRC")

## ------------------------------------------------------------------------
library(aoos)

PersonRC <- Class({
  Class <- "PersonRC"
  
  personName <- "character"
  
  initialize <- function(name) {
    .self$personName <- name
    .self$greet()
    }
  
  greet <- function() {
    cat(paste0("Hello, my name is ", .self$personName, ".\n"))
    }
  
  })

ann <- PersonRC("Ann")
ann
ann$personName
ann$personName <- "not Ann"
ann$greet()

## ------------------------------------------------------------------------
PrivatePerson <- Class({
  Class <- "PrivatePerson"
  contains <- "Private"
  
  .personName <- "character"
  
  initialize <- function(name) {
    .self$.personName <- name
    .self$greet()
    }
  
  greet <- function() {
    cat(paste0("Hello, my name is ", .self$.personName, ".\n"))
    }
  
  })

ann <- PrivatePerson("Ann")
ann
stopifnot(inherits(try(ann$.personName, silent = TRUE), "try-error"))
ann$greet()

