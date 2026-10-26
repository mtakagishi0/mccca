#' @importFrom stats kmeans dist



updateUG.MCCCA <- function(data.k=data.k,Ggrp=Ggrp,knownvec=knownvec,
                           class.n.vec=class.n.vec,cluster.vec=cluster.vec,
                                     U0=NULL,use.kmeans=FALSE,#cls.tr.list=NULL,
                                     K.vec=K.vec,n.vec=n.vec,#data.vec=data.vec,
                                     total.init.k=total.init.k){#,Kes=Kes

  if((any(knownvec)) & ((is.null(U0)))) warning("(in update U formula) specify true class for known cluster.")
  #is.null(cls.tr.list) |
  N <- nrow(data.k) ; K=sum(K.vec)
  ndata <- length(K.vec)
  nvari <- ncol(data.k)
  #Kes<-nrow(Ggrp)

  ###make list
  #Ggrp.list<-mat2list.func(data=Ggrp,inputform="matrix",rowvec=rowvec,colvec=colvec)

  ##result is list
  Ugrp.list<-rep(list(NA),ndata)
  U.new=matrix(0,N,K)
  G.new <- Ggrp
  #G.new <- matrix(NA,nrow(Ggrp),ncol(Ggrp))

  ###apply k-means for each data####
  #browser()
  empty.cls <- FALSE
  cc<-1
  for(cc in 1:ndata){
    ##parameter of this data
    Kes<-K.vec[cc]
    n.d<-n.vec[cc]

    ###take up cluster mean
    ck<-ifelse(cc!=1,sum(K.vec[c(1:(cc-1))]),0)
    cc.kvec<-c((ck+1):(ck+K.vec[cc]))
    Ggrp.d<-Ggrp[cc.kvec,]

    ###take up data
    #browser()
    #data.k.d <- data.k[data.vec==cc,]
    #drow<-ifelse(cc!=1,sum(n.vec[c(1:(cc-1))]),0)
    #cc.rowvec<-c((drow+1):(drow+n.vec[cc]))
    #data.k.d<-data.k[cc.rowvec,]
    data.k.d<-data.k[class.n.vec==cc,]
    #print(cc.rowvec)
    #print(cc.kvec)

    Ugrp.list[[cc]]<-matrix(0,n.d,Kes)

    if(!knownvec[cc]){

      if(use.kmeans){
        #browser()
        #if(cc==1) cat("use kmeans\n")
        #kres <- kmeans(data.k.d,centers=Ggrp.d,nstart=total.init.k)
        kres <- try(kmeans(data.k.d,centers=Ggrp.d,nstart=total.init.k))#,
        # silent=TRUE)
        if(inherits(kres, "try-error")){
        #if(class(kres)=="try-error"){
          #browser()
          #kres <- try(kmeans(data.k.d,centers=Ggrp.d,nstart=total.init.k))
          empty.cls <- TRUE
          break
        }else{
          G.new[cc.kvec,] <- kres$centers
          Uc=1.0*outer(kres$cluster, 1:Kes, "==")
          U.new[class.n.vec==cc,cluster.vec==cc]=Uc
          #Ugrp.list[[cc]] <- 1.0 * outer(kres$cluster, 1:Kes, "==")
        }
      }else{ #if not using kmeans

        #browser()
        dimm=nrow(data.k.d);dimm2=ncol(data.k.d)
        dist.mat=matrix(NA,(dimm+nrow(Ggrp.d)),dimm2)
        dist.mat[c(1:dimm),]=data.k.d
        dist.mat[-c(1:dimm),]=Ggrp.d
        Distmat<-as.matrix(dist(dist.mat))

        #Distmat2<-as.matrix(dist(rbind(data.k.d,Ggrp.d)))
        #all(Distmat==Distmat2)
        ##Distmat<-matrix(dist(rbind(data.kd,Ggrp.d)),c(n.d+Kes),c(n.d+Kes))
        clsvec<-apply(matrix(Distmat[c(1:n.d),c((n.d+1):(n.d+Kes))],n.d,Kes),1,which.min)
        #Ugrp.list[[cc]] <- 1.0 * outer(clsvec, 1:Kes, "==")#[cbind(seq(1,n.d),clsvec)]<-1
        Uc=1.0*outer(clsvec, 1:Kes, "==")
        U.new[class.n.vec==cc,cluster.vec==cc]=Uc
        #browser()
        if(length(unique(clsvec))!=Kes) {
          empty.cls <- TRUE
          break
        }
      }

    }else if(knownvec[cc]){
      G.new[cc.kvec,] <- Ggrp.d
      U.new[class.n.vec==cc,cluster.vec==cc]<-U0[class.n.vec==cc,cluster.vec==cc]
      #Ugrp.list[[cc]]<-U0[[cc]]
      #Ugrp.list[[cc]][cbind(seq(1,n.d),cls.tr.list[[cc]])]<-1
    }
  }
  #browser()
  #kres<-kmeans(data.k,centers=Ggrp,nstart = 100)

  list(Ugrp=U.new,Ggrp=G.new,empty.cls=empty.cls)#,Ggrp=Ggrp,objcoord.k=Fn2

}



