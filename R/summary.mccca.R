#' Summarize MCCCA Results
#'
#' @description
#' Produces a numerical summary of an \code{mccca} object created by \code{\link{MCCCA}}.
#'
#' @param object An \code{mccca} object created by \code{\link{MCCCA}}.
#' @param \dots Additional arguments passed to the summary method.
#' 
#' @export
#' @seealso \code{\link{MCCCA}}, \code{\link{plot.mccca}}
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
#' # Specify the numbers of class-specific clusters
#' K.vec <- c(2, 1, 2, 2)
#'
#' # Apply MCCCA
#' res <- MCCCA(mcccadata, K.vec = K.vec)
#'
#' # Display the MCCCA biplot
#' shortlabels <- c("AF", "AM", "JF", "JM")
#' plot(res, classlabel = shortlabels)
#'
#' # Show a numerical summary
#' summary(res)
#'


summary.mccca<-function(object,...){
#
  #cat("Classes that each cluster corresponds to:\n")
  #print(object$cluster.vec.label)
  #object$classlabel[object$cluster.class.index]
  #tabcls=table(object$cluster.class.index)
  cat("The number of clusters specified for each class:\n")
  K.vec2=object$K.vec
  names(K.vec2)=object$classlabel
  print(K.vec2)

  #cat("\nThe number of observations in each cluster:")#:\n")
  #print(table(object$cluster.label.n.vec))
  #print(table(res$cluster.n.vec.label))
  # cluster.n.vec=object$cluster.n.vec
  # tabcls=table(cluster.n.vec)
  # names(tabcls)=names(object$cluster.vec.label)
  # print(tabcls)
  # print(table(cluster.n.vec))
  # K=max(cluster.n.vec)
  # for(kk in 1:K){
  #   cluster.n.vec[cluster.n.vec==kk]=paste("cluster",kk)
  # }
  # print(table(cluster.n.vec))

  #cat("\nObjective criterion value:",round(object$optval,4),"\n")

  #cat("\nCoordinates:\n")
  #cat("Cluster centroids (gamma scaled):\n")
  #print(round(object$Gg,3))
  
  cat("\nCoordinates (gamma scaled) and sizes of each class-specific cluster:\n")
  #Gg=object$Gg
  #CCs=matrix(NA,nrow(Gg),4)
  #CCs[,c(1,2)]=Gg ; CCs[,c(3,4)]=object$csize
  #rownames(CCs)=rownames(Gg)
  #print(round(CCs,3))
  print(round(cbind(object$Gg,object$csize),3))
  

  cat("\nCoordinates of each category (gamma scaled):\n")
  print(round(object$Bg,3))

  #cat("\nInertia:\n")
  #print(round(object$iner.res[,-1],3))
}

