#' Summarize Cluster-Number Selection Results
#'
#' @description
#' Produces a numerical summary of a \code{decideK} object created
#' by \code{\link{decideK}}.
#' 
#' @param object A \code{decideK} object created by \code{\link{decideK}}.
#' @param digits Number of digits to print.
#' @param \dots Additional arguments passed to the summary method.
#' 
#' @export
#' @seealso \code{\link{decideK}}, \code{\link{plot.decideK}}
#' @examples
#' # Load the example data
#' data(accident)
#'
#' # Prepare active and external variables
#' active<-c(1:4)
#' external<-c(5:6)
#' dat.act<-accident[,active]
#' dat.ext<-accident[,external]
#'
#' # Create a mcccadata object
#' mcccadata <- create.MCCCAdata(dat.act, dat.ext)
#' 
#' # Specify the candidate numbers of clusters (2 to 8)
#' Krange=c(2,4)
#' 
#' # Determine the numbers of class-specific clusters using the default KL index
#' # (using a small nstart for a faster example)
#' KLres=decideK(mcccadata,Krange=Krange,nstart=10)
#' 
#' # Show the calculated cluster validity indices
#' summary(KLres)
#' 


summary.decideK <- function(object, digits = 3, ...) {
  
  ind.list <- object$ind.list
  cls.best <- object$cls.best
  C <- length(cls.best)
  classnames <- sub(" class$", "", names(ind.list))
  
  # classes for which the index was not calculated
  skipped <- object$skipped
  if (is.null(skipped)) skipped <- rep(FALSE, C)   # objects from older versions
  
  # candidate numbers of clusters for each class (from column names "clusters=K")
  K.list <- lapply(ind.list, function(x)
    as.integer(sub("clusters=", "", colnames(x))))
  K.all <- sort(unique(unlist(K.list[!skipped])))
  
  # classes x numbers of clusters (rows of skipped classes are left empty)
  tab <- matrix(NA, nrow = C, ncol = length(K.all),
                dimnames = list(classnames, K.all))
  for (cc in which(!skipped)) {
    tab[cc, as.character(K.list[[cc]])] <- ind.list[[cc]][1, ]
  }
  
  cat("The", object$index,
      "index values for each class (rows) and number of clusters (columns):\n")
  print(round(tab, digits), na.print = "")
  
  cat("\nThe best number of clusters for each class:\n")
  print(cls.best)
  
  if (any(skipped)) {
    cat("\nThe index was not calculated for the following classes",
        "(too few observations or only one distinct data point),",
        "so the number of clusters is set to 1:\n",
        paste(classnames[skipped], collapse = ", "), "\n")
  }
  
  invisible(tab)
}