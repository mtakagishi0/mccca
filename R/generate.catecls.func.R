#' Generate (NxJ) clustered categorical data matrix.
#'
#' @description Generate an (NxJ) clustered categorical data matrix given by prop.J.list and true cluster allocation.
#' @param N The number of observations.
#' @param J The number of active variables.
#' @param q.vec A vector of length J giving the number of categories for each active variable.
#' @param Ktrue An integer indicating the number of content-based clusters used for CCRS estimation.
#' @param prop.J.list a list of length J, where each list is a (Ktrue x qj) matrix giving the proportion for each qj category in each of the \code{Ktrue} cluster.
#' @param clstr.vec A vector of length N giving true clusters for each observations.
#' @return an (NxJ) clustered categorical data matrix.
#' @noRd

generate.catecls <- function(N=N, J=J, q.vec=q.vec, Ktrue=Ktrue,
                          prop.J.list = prop.J.list, clstr.vec=clstr.vec){

  dat <- matrix(NA,N,J)
  #ncate <- unique(q.vec)
  #browser()

  for(jj in 1:J){
    ncate <- q.vec[jj]
    for(kk in 1:Ktrue){
      cls.ind <- clstr.vec==kk
      n.k<-length(which(cls.ind))
      dat[cls.ind,jj] <-sample(ncate,size=n.k, prob= prop.J.list[[jj]][kk,]
                               ,replace=TRUE)
    }#K
  }#all vari

  dat

}

# generate.cate <- function(N=N, prop.vec=prop.vec){
#
#   if(round(sum(prop.vec),4)!=1){
#     cat("probability is not appropriate. the sum of probability is",sum(prop.vec),".\n")
#   }
#
#   dat=sample(length(prop.vec),size=N, prob= prop.vec
#          ,replace=TRUE)
#
#   dat
#
# }

