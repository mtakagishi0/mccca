#' Generate (NxJ) categorical data matrix.
#'
#' @description Generate (NxJ) categorical data matrix.
#' @usage generate.onedata(N=100,J=5,Ktrue=3,q.vec=rep(3,5),noise.prop=0.3)
#' @param N The number of observations. Default is 100.
#' @param J The number of active variables. Default is 5.
#' @param Ktrue The number of true clusters. Default is 3.
#' @param q.vec A vector of length J giving the number of categories for each active variable. Default is rep(3,5).
#' @param noise.prop A numeric value between 0 and 1 indicating the proportion of noise variables among J variables. Default is 0.3.
#' @return Returns a list with the following elements.
#' \item{\code{data.mat}}{A (NxJ) data frame of categorical data.}
#' \item{\code{clstr0.vec}}{A vector of integers (from 1:Ktrue) length N giving the cluster to which each observation is allocated.}
#' @seealso \code{\link{generate.onedata}}
#' @export
#' @examples
#' # setting
#' N <- 100 ; J <- 5 ; Ktrue <- 2 ; q.vec <- rep(5,J) ; noise.prop <- 0.2
#' extcate.vec=c(2,3) #the number of categories for each external variable
#' 
#' # generate categorical variable data
#' catedata.list <- generate.onedata(N=N,J=J,Ktrue=Ktrue,q.vec=q.vec,noise.prop = noise.prop)
#' data.act=catedata.list$data.mat
#' 
#' # generate external variable data
#' data.ext=generate.ext(N,extcate.vec=extcate.vec)
#' 
#' # create mccca.list to be applied to MCCCA function
#' mccca.data=create.MCCCAdata(data.act,data.ext)

generate.onedata <- function(N=100,J=5,Ktrue=3,q.vec=rep(3,5),
                             noise.prop=0.3){

  clstrtemp <- rep(c(1:Ktrue),times=rep(floor(N/Ktrue),Ktrue))
  clstr0.vec <- c(clstrtemp,rep(c(1:Ktrue),N))[c(1:N)]

  ####noise variable setting####
  nnoise=floor(J*noise.prop)#
  #generate noise variable index
  which.noise <- sample(J,nnoise,replace=FALSE)

  prop.list <- create.prop(J=J, q.vec=q.vec,
                                Ktrue=Ktrue,which.noise=which.noise)#pnoise=length(which.noise),randomstrong=T,highasso=highasso.vari)

  data.mat <- generate.catecls(N=N, J=J, q.vec=q.vec, Ktrue=Ktrue,
                               prop.J.list = prop.list, clstr.vec=clstr0.vec)

  rownames(data.mat)<-names(clstr0.vec)<-paste0("obj",c(1:N))
  colnames(data.mat)=paste0("v",c(1:J))

  data.mat=as.data.frame(data.mat)
  data.mat[] <- lapply(data.mat, as.integer)

  list(data.mat=data.mat,clstr0.vec=clstr0.vec)

}
