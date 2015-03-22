## ----, eval=FALSE--------------------------------------------------------
#  vignette("aoosClasses", "aoos")

## ------------------------------------------------------------------------
mEdit <- Class({
  Class <- "mEdit"
  
  data <- "matrix"
  edits <- "list"

  edit <- function(i, j, value) {
    ## the following string documents the edit method
    'Replaces the range [i, j] of the object by value.'
    backup <- list(i, j, data[i,j])
    data[i,j] <<- value
    edits <<- c(edits, list(backup))
    invisible(value)
  }

  undo <- function() {
    'Undoes the last edit() operation and update the edits field accordingly.'
    prev <- edits
    if(length(prev)) prev <- prev[[length(prev)]]
    else stop("No more edits to undo")
    edit(prev[[1]], prev[[2]], prev[[3]])
    ## trim the edits list
    length(edits) <<- length(edits) - 2
    invisible(prev)
  }
  
  show <- function() {
    'Method for automatically printing matrix editors' 
    cat("Reference matrix editor object of class", 
        classLabel(class(.self)), "\n")
    cat("Data: \n")
    methods::show(data)
    cat("Undo list is of length", length(edits), "\n")
  }
})

xMat <- matrix(1:12,4,3)
xx <- mEdit(data = xMat)
xx$edit(2, 2, 0)
xx
xx$undo()
mEdit$help("undo")
stopifnot(all.equal(xx$data, xMat))

utils::str(xx) # show fields and names of non-trivial methods

## add a method to save the object
mEdit$methods(
  save = function(file) {
    'Save the current object on the file in R external object format.'
         base::save(.self, file = file)
     }
)

tf <- tempfile()
xx$save(tf)

# mv <- Class({
#   Class <- "matrixViewer"
#   contains <- "mEdit"
#   
#   viewerDevice <- viewerFile <- "ANY"
#   
#   view <- function() {
#     dd <- dev.cur(); dev.set(viewerDevice)
#     devAskNewPage(FALSE)
#     matplot(data, main = paste("After",length(edits),"edits"))
#     dev.set(dd)
#   }
#   
#   edit <- function(i, j, value) {
#     callSuper(i, j, value)
#     view()
#   }
# })
# 
# mv$methods(
#   initialize =
#     function(file = "./matrixView.pdf", ...) {
#       viewerFile <<- file
#       pdf(viewerFile)
#       viewerDevice <<- dev.cur()
#       dev.set(dev.prev())
#       callSuper(...)
#     },
#   finalize = function() {
#     dev.off(viewerDevice)
#   })


