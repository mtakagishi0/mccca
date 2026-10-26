#' Summarize an mcccadata Object
#'
#' @description
#' Produces a numerical summary of an \code{mcccadata} object created
#' by \code{\link{create.MCCCAdata}}.
#'
#' @param object An \code{mcccadata} object created by
#'   \code{\link{create.MCCCAdata}}.
#' @param \dots Additional arguments passed to the summary method.
#' 
#' @export
#' @seealso \code{\link{create.MCCCAdata}}
#' @examples
#' # Load the example data
#' data(mealDrink)
#'
#' # Prepare active and external variables
#' active<-c(1,2)
#' external<-c(3,4)
#' dat.act=mealDrink[,active]
#' dat.ext=mealDrink[,external]
#'
#' # Create a mcccadata object
#' mcccadata <- create.MCCCAdata(dat.act, dat.ext)
#'
#' # Show a summary of the created classes
#' summary(mcccadata)
#'



summary.mcccadata<-function(object,...){
  
  cat("The number of classes created by the given external variables:",object$C,"\n")

  cat("\nClass labels and its constituent external variables:\n")
  #which category in external variables each class corresponds to
  print(object$classlab.mat)

  cat("\nThe number of observations in each class:")#\n")
  #print(table(object$classlabel.n.vec))
  #print(table(object$class.label.n.vec))
  print(table(factor(object$class.label.n.vec, levels = object$classlabel)))
}

