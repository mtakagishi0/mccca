#' Calculate chosen cluster index using specified K.
#'
#' @description Calculates a cluster validity index to determine the numbers of class-specific clusters.
#' @param mccca.data A list created in \code{\link{create.MCCCAdata}}.
#' @param cindex A character string specifying the cluster validity index used to determine the numbers of class-specific clusters. One of \code{"KL"}, \code{"CH"}, \code{"S"}, or \code{"DB"}. See details. The default is \code{"KL"}.
#' @param cindexnm An integer specifying the cluster validity index. If specified, this argument is used instead of \code{cindex}. See details.
#' @param Krange An integer vector of length 2, giving the range of candidate number of clusters which are common to all classes.
#' @param Krange.list A list of length C vectors, specifying the ranges of candidate numbers of clusters for each C classes. This is used when you want to use different \code{Kcand} for each class. The default is \code{NULL}.
#' @param subthr An integer. The cluster validity index is not calculated for classes with fewer observation than this value. The default is 20.
#' @param return.clus A logical value indicating whether the clustering results for all candidate numbers of clusters, are returned. The default is \code{FALSE}.
#' @param fulldim A logical value indicating whether k-means is applied to all MCA dimensions. If \code{FALSE} (default), only the first two dimensions are used.
#' @param nstart An integer specifying the number of random initial configurations used in k-means. Larger values generally improve the stability of the solution but increase the computation time. The default is 1000.
#' @return Returns a list with the following elements.
#' \item{\code{cls.best}}{An integer vector of length \code{C}, giving the best number of clusters for each class according to the specified cluster validity index. The names are the class labels. For classes for which the index is not calculated (see \code{skipped}), 1 is returned.}
#' \item{\code{ind.list}}{A list of C vectors, giving the calculated values of the specified cluster validity index for each specified number of clusters.}
#' \item{\code{cindexnm}}{The numeric code of the cluster validity index used.}
#' \item{\code{index}}{The name of the cluster validity index used.}
#' \item{\code{Cls.list}}{A list of C matrices, containing the clustering results for all candidate numbers of clusters in each class. This is returned only when \code{return.clus=TRUE}; otherwise, \code{NULL}.}
#' \item{\code{skipped}}{A logical vector of length \code{C}, indicating the classes for which the index is not calculated, because the number of observations is not more than \code{subthr} or there is only one distinct data point.}
#' @seealso \code{\link{summary.decideK}}, \code{\link{plot.decideK}}
#' @details
#' Four cluster validity indices are available:
#' \itemize{
#'   \item \code{"KL"}: Krzanowski--Lai index
#'   (Krzanowski and Lai 1988),
#'   \item \code{"CH"}: Calinski--Harabasz index
#'   (Cali\'nski and Harabasz 1974),
#'   \item \code{"S"}: Silhouette index
#'   (Kaufman and Rousseeuw 2009),
#'   \item \code{"DB"}: Davies--Bouldin index
#'   (Davies and Bouldin 1979).
#' }
#'
#' Alternatively, the cluster validity index can be specified by
#' \code{cindexnm}, where
#' \code{1 = "KL"},
#' \code{2 = "CH"},
#' \code{3 = "S"}, and
#' \code{4 = "DB"}.
#' 
#' @importFrom MASS mca
#' @importFrom stats kmeans
#' @importFrom clusterSim index.G1 index.S dist.GDM index.DB
#' @export
#' @references Cali\'nski, T. and Harabasz, J. (1974). A dendrite method for cluster analysis. 
#' \emph{Communications in Statistics}, 3(1), pp.1-27.
#'
#' Davies, D. L. and Bouldin, D. W. (1979). A cluster separation measure.
#' \emph{IEEE Transactions on Pattern Analysis and Machine Intelligence}, PAMI-1(2), pp.224-227.
#'
#' Kaufman, L. and Rousseeuw, P. J. (2009). \emph{Finding Groups in Data: An Introduction to Cluster Analysis}.
#' Wiley.
#'
#' Krzanowski, W. J. and Lai, Y. T. (1988). A criterion for determining
#' the number of groups in a data set using sum-of-squares clustering.
#' \emph{Biometrics}, 44(1), pp.23-34.
#' 
#' Takagishi, M. and van de Velden, M. (2022),
#' Visualizing class specific heterogeneous tendencies in categorical data.
#' \emph{Journal of Computational and Graphical Statistics}, 31(3), pp.790-801
#'
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
#' 
#' # Show the calculated cluster validity indices
#' summary(KLres)
#' 
#' 
#' # Plot the cluster validity indices
#' plot(KLres)
#'
#' # Use the numbers of clusters selected by decideK() to fit MCCCA
#' K.vec <- KLres$cls.best
#'
#' # Apply MCCCA (using a small nstart for a faster example)
#' res <- MCCCA(mcccadata, K.vec = K.vec, nstart=5)
#'



decideK<-function(mccca.data,subthr=20,cindex="KL",cindexnm=NULL,
                  Krange=NULL,Krange.list=NULL,return.clus=FALSE,
                  fulldim=FALSE,nstart=1000){
  ###dataset in each class
  data.list=mccca.data$act.list
  C=length(data.list)
  indvec=c("KL","CH","S","DB")

  if(is.null(Krange) & is.null(Krange.list)){
    stop("Specify either Krange or Krange.list.\n")
  }

  if(!is.null(Krange) & !is.null(Krange.list)){
    stop("Dont specify both.\n")
  }

  if(subthr<1){
    stop("subthr should be more than 0.")
  }



  if(!(cindex %in% indvec)){
    stop(sprintf("'%s' is not a valid cindex. Available choices are: %s",
                 cindex,paste(indvec, collapse = ", ")))
  }
  cindexnm0=which(cindex==indvec)

  ####index name check
  if(!is.null(cindexnm)){
    #which(cindexnm==indvec)
    #cindex=which(indvec==cindexnm)
    cindexnm0=cindexnm
    #browser()
  }
  #cat("cindex",cindex)

  # if(!cindex %in% c(1:4)){
  #   stop("cindex should be either 1~4!!")
  # }
  # if(!cindex %in% c("KL","CH","S")){
  #   stop("cindex should be either KL,CH,S!!")#check code&Eng
  # }
  cindexname_long=c("Krzanowski & Lai","Calinski & Harabasz","Silhouette","Davies-Bouldin")
  message(paste0("cluster index '",cindexname_long[cindexnm0],"' is used.\n"))

  ###

  ###check distinct data points in each dataset

  #####for the case Krange is used
  if(!is.null(Krange)){
    #if(length(Krange)==1)stop("The length of Krange should be greater than 1!\n")
    if(length(Krange)!=2)stop("The length of Krange should be 2!\n")
    #if(any(diff(Krange)!=1))stop("Elements in Krange are not increasing one by one!")

    #Krange0=c(min(Krange)-1,Krange,max(Krange)+1)
    if(cindexnm0==1){
      Krange0=c(min(Krange)-1,seq(Krange[1],Krange[2]),max(Krange)+1)
    }else{#if cindexnm0=="CH" or "S" or "DB"
      Krange0=seq(Krange[1],Krange[2])
    }
    if(Krange0[1]==0)Krange0=Krange0[-1]
    Krange0.list=NULL
  }else{#if Krange is not null
    Krange0.list=rep(list(NA),C)
  }

  #Cls.array<-array(NA,c(nsub,nK,C))
  skipped <- rep(FALSE, C)
  Cls.list<-ind.list<-rep(list(NA),C)

  cc=1
  for(cc in 1:C){

    #cat("------------------dat",cc,"---------------------\n")

    if(!is.null(Krange.list)){
      Krange.h=Krange.list[[cc]]
      if(length(Krange.h)!=2)stop("The length of Krange.list[[",cc,"]] should be 2!\n")
      #if(length(Krange.h)==1)stop("The length of Krange.list[[",cc,"]] should be greater than 1!")
      #if(any(diff(Krange.h)!=1))stop("Elements in Krange.list[[",cc,"]] are not increasing one by one!")

      if(cindexnm0==1){
        #Krange0=c(min(Krange.h)-1,Krange.h,max(Krange.h)+1)
        Krange0=c(min(Krange.h)-1,seq(Krange.h[1],Krange.h[2]),max(Krange.h)+1)
      }else{#if cindexnm0==2("CH") or 3("S") or 4("DB")
        Krange0=seq(Krange.h[1],Krange.h[2])
      }
      if(Krange0[1]==0)Krange0=Krange0[-1]
      Krange0.list[[cc]]=Krange0
    }

    dat.d=data.list[[cc]]
    nsub=nrow(dat.d)
    nK=length(Krange0)
    Clsmat=matrix(NA,nsub,nK)
    ind.vec=rep(NA,nK)
    colnames(Clsmat)<-names(ind.vec)<-paste(Krange0,"clusters result")#paste0("cluster",Krange0)

    #if((!nsub<=subthr) & (nrow(unique(dat.d))>1)){
    if(nsub<=subthr){
      warning("In the ",cc,"th class, the number of individual is less than a prespecified threshold. So skip this class\n")
      #  print("some categories are not chosen.")
      #indres<-NULL
      ind.vec=0 ; skipped[cc] <- TRUE
    }else if(nrow(unique(dat.d))==1){
      warning("In the ",cc,"th class, distinct data points is 1 so skip this class.\n")
      ind.vec=0 ; skipped[cc] <- TRUE 
    }else{

      #browser()
      #unique(dat.d)

      if(fulldim){#add 26/6/10
        mca.res <- mca(dat.d,nf=ncol(dat.d))
      }else{
        mca.res <- mca(dat.d)
      }

      #par(mfrow=c(1,1),mai=c(0.4,0.4,0.3,0.1))
      #plot(mca.res)#,rows = FALSE)
      #browser()
      ind.cord <- mca.res$rs #ob

      ##kmeans
      for(kk in 1:nK){
        Kes=Krange0[kk]
        #cat("n=",nrow(unique(ind.cord)),",K=",Kes,"\n")

        #if(nrow(unique(ind.cord))<Kes)stop("In the ",cc,"th class, K=",Kes," is bigger than distinct data points, so use smaller K.\n")
        if(nrow(unique(ind.cord))<Kes)stop("In the ",cc,"th class, K=",Kes-1," is equal to distinct data points, so specify K smaller than K=",Kes-2,".\n")
        clses <- kmeans(ind.cord,centers = Kes,nstart = nstart)$cluster
        #ccares<-clusmca(dat.d, nclus=Kes, ndim=2, method = "clusCA", nstart = 50)
        #clses<-ccares$cluster
        #print(table(clses))#total.init.cca

        #If the number of ppl in the cluster is less than a certain number
        if(length(unique(clses))!=Kes){
          warning("there is an empty cluster.\n")
          #omit that cls, otherwise index cannot calculate.
          if(kk!=1) {pickupK<-c(1:(kk-1)) #Calculated up to the number of clusters less than the number of empty clusters
          }else{pickupK<-1}
          break
        }else if(any(table(clses)==1)){
          #cat("In the ",cc,"th class, theres a cluster of one individual.\n")
          warning("In the",cc,"th class, there is a cluster of one individual when setting K=",Kes,".\n")
          pickupK<-c(1:nK)#keep calculating all cluster's index
        }else{
          pickupK<-c(1:nK)#keep calculating all cluster's index
        }
        Clsmat[,kk] <- clses

      }##kmeans for all K

      ###cls index check!!!!
      Clsmat=Clsmat[,pickupK]
      nK2=length(pickupK)
      #cat("cindexnm0=",cindexnm0,"\n")
      if(cindexnm0==1){
        ###KL####
        for(kk2 in 2:(nK2-1)){
          ind.vec[kk2]<-index.KLed(ind.cord,Clsmat[,c((kk2-1):(kk2+1))])#clusterSim
          #print(KLcheck(ind.cord, Clsmat[,c((kk2-1):(kk2+1))]))
        }
      }else if(cindexnm0==2){
        #####CH#####
        for(kk2 in 1:nK2){
          #browser()
          ind.vec[kk2] <- index.G1(ind.cord,Clsmat[,kk2])
          #ind.vec[kk2]<-index.KLed(ind.cord,Clsmat[,c((kk2-1):(kk2+1))])#clusterSim
        }
      }else if(cindexnm0==3){
        ###si#####
        for(kk2 in 1:nK2){
          d.gdm <- dist.GDM(ind.cord)
          ind.vec[kk2] <- index.S(d.gdm,Clsmat[,kk2])
        }
      }else if(cindexnm0==4){
        ####DB######
        for(kk2 in 1:nK2){
          ind.vec[kk2] <- index.DB(ind.cord, Clsmat[,kk2], centrotypes="centroids")$DB
        }

      }##end all cindex
      #cat("ind.vec=",ind.vec,"\n")

    }###end calculating MCAres & index value

    ###save
    #browser()
    Cls.list[[cc]]=Clsmat
    ind.list[[cc]]=ind.vec

  }##end for K

  cls.best=rep(NA,C)
  #names(cls.best)=rownames(ind.mat)
  names(Cls.list)<-names(cls.best)<-names(ind.list)<-paste(mccca.data$classlabel,"class")#paste0("data",c(1:C))
  #names(Cls.list)<-names(ind.list)<-
  ind.list2=ind.list

  for(cc in 1:C){
    #browser()
    if(!is.null(Krange.list)){
      Krange0=Krange0.list[[cc]]
    }
    #if(all(ind.mat[cc,]==0)){#if # of ind is less than threshold
    #  cat("In the ",cc,"th data, the number of of individual is less than a prespecified threshold.\n")
    #which.nona=which(!is.na(ind.mat[cc,]))
    which.nona=which(!is.na(ind.list[[cc]]))
    ind.nona<-matrix(ind.list[[cc]][which.nona],nrow=1)#use matrix to prevent colname from disappearing
    Krange0.nona=Krange0[which.nona]
    #rownames(ind.nona)=rownames(ind.mat)[cc]
    rownames(ind.nona)=names(cls.best)[cc]
    #colnames(ind.nona)=paste(Krange0.nona,"clusters")
    colnames(ind.nona)=paste0("clusters=",Krange0.nona)
    ind.list2[[cc]]<-ind.nona
    #names(ind.mat[cc,which.nona])<-does not work for 1 col matrix
    if(all(ind.nona==0)){
      cls.best[cc]<-1
    }else{
      #print(Krange0.nona)
      if(cindexnm0==4){##only when DB index, min value is chosen.
        cls.best[cc]<-Krange0.nona[which(ind.nona==min(ind.nona))]
      }else{
        cls.best[cc]<-Krange0.nona[which(ind.nona==max(ind.nona))]
      }
      #print(round(ind.nona,3))
    }
  }

  if(!return.clus)Cls.list<-NULL
  
  # use the class labels (without " class") as names of the returned vectors
  names(cls.best) <- names(skipped) <- mccca.data$classlabel
  
  res=list(cls.best=cls.best,ind.list=ind.list2,Cls.list=Cls.list,
           cindexnm=cindexnm0,index=indvec[cindexnm0],skipped=skipped)
  class(res)="decideK"

  return(res)

}
