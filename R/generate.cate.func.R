
##
#' Generate (NxJ) categorical data matrix.
#'
#' @description Generate an (NxJ) categorical data matrix given by prop.J.list and true cluster allocation.
#' @param N The number of observations.
#' @param prop.list a list length J, each of which is a vector of length qj giving the proportion for each categories.
#' @return an (NxJ) categorical data matrix.
#' @noRd


generate.cate.list <- function(N=N, prop.list=prop.list){

  if(!is.list(prop.list)){
    prop.list <- list(prop.list)
  }

  if(any(lapply(prop.list,sum)!=1)){
    cat("some prop is not appropriate. prop are",unlist(lapply(prop.list,sum)),".\n")
  }

  J <- length(prop.list)
  dat <- matrix(NA,N, J)

  for(jj in 1:J){
    dat[,jj] <-  sample(length(prop.list[[jj]]),size=N, prob= prop.list[[jj]]
                        ,replace=TRUE)
  }

  dat

}
