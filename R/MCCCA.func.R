#' apply MCCCA for dataset.
#'
#' @description Applies MCCCA to a \code{mcccadata} object created by \code{\link{create.MCCCAdata}}.
#' @param mcccadata A \code{mcccadata} object, a list created by \code{\link{create.MCCCAdata}}.
#' @param K.vec An integer vector of length C (C:the number of classes). Each element corresponds to the number of clusters in each class, which needs to be specified for estimation.
#' @param ndim An integer indicating the dimension of quantification. The default is 2.
#' @param nstart An integer indicating the number of random initial values. The default is 30.
#' @param maxit An integer indicating the maximum number of iterations.
#' @param tol A numeric value indicating the absolute convergence tolerance. The default is 1e-6.
#' @param update.kmeans A logical value, indicating whether a cluster indicator matrix is updated by k-means or referring to the distance (the way described in the reference paper). The default is \code{FALSE} (not using k-means).
#' @param verbose A logical value. If \code{TRUE}, tracing information on the progress of the optimization is produced.
#' @return Returns a list with the following elements.
#' \item{\code{G}}{A (K x \code{ndim}) quantification matrix for all clusters (K: the total number of clusters, i.e.,K=\code{sum(K.vec)}).}
#' \item{\code{Gg}}{Re-scaled version of \code{G}. See details.}
#' \item{\code{B}}{A (Q x \code{ndim}) quantification matrix for all categories (Q: the total number of categories), and \code{q.vec} is given by \code{create.MCCCAdata}).}
#' \item{\code{Bg}}{Re-scaled version of \code{B}.}
#' \item{\code{Fn}}{A (N x \code{ndim}) quantification matrix for all observations.}
#' \item{\code{Fng}}{Re-scaled version of \code{Fn}.}
#' \item{\code{cluster.class.index}}{An integer (from 1:C) vector of length K showing the class indices to which each cluster corresponds.}
#' \item{\code{cluster.index}}{An integer (from 1:K) vector of length K showing the cluster indices to which each cluster corresponds.}
#' \item{\code{cluster.label.n.vec}}{A character vector of length N, where each element represents the estimated cluster labels for each observation that corresponds to the rows of \code{dat.act} (given in \code{mcccadata} object).}
#' \item{\code{cluster.label.n.list}}{A list of C vectors, giving the estimated cluster labels for each observation in each class.}
#' \item{\code{cluster.index.n.vec}}{An integer (from 1:K) vector of length N, showing the same information as \code{cluster.label.n.vec} but organized by cluster indices.}
#' \item{\code{cluster.index.n.list}}{A list of C vectors, showing the same information as \code{cluster.label.n.list} but given by cluster indices.}
#' \item{\code{catename.vec}}{A character vector of length Q with the category names of each categorical variable.}
#' \item{\code{catename.vari.vec}}{A character vector of length Q with \code{catename.vec} plus the name of categorical variable (by default, this is used as the column name of \code{B} and \code{Bg}).}
#' \item{\code{cate.removed}}{If there is a category that is not chosen by any observation, \code{cate.removed} gives which category was removed (given by the index of column in dummy matrix). Otherwise, return \code{NULL}.}
#' \item{\code{q.vec}}{A copy of \code{q.vec} in the \code{mcccadata} list.}
#' \item{\code{K.vec}}{A copy of \code{K.vec} in the argument of \code{MCCCA()}.}
#' \item{\code{classlabel}}{A copy of \code{classlabel} in the \code{mcccadata} list.}
#' \item{\code{inertia}}{A table containing the adjusted inertias and their relative and cumulative values. See details.}
#' \item{\code{inertia.G}}{A table containing the adjusted inertias and their relative and cumulative values based on Greenacre's normalization. See details.}
#' @seealso \code{\link{create.MCCCAdata}}, \code{\link{summary.mccca}}, \code{\link{plot.mccca}}
#' @details
#' \code{Bg},\code{Gg} and \code{Fng} are re-scaled versions of \code{B},\code{G} and \code{Fn} respectively, 
#' where the average squared deviations from the origin for the row and column points is the same 
#' (See section 2.3 in Takagishi, M. and van de Velden, M. (2022) for details).
#' 
#' The first column of both \code{inertia} and \code{inertia.G} contains the Benzécri-adjusted inertias.
#'
#' In \code{inertia}, the second column contains the relative adjusted
#' inertias obtained by dividing each adjusted inertia by the sum of all adjusted inertias, 
#' and the third column contains their cumulative values.
#'
#' In \code{inertia.G}, the second column contains the relative
#' adjusted inertias calculated using Greenacre's normalization, and
#' the third column contains their cumulative values.
#'
#' See Benzécri (1979) and Greenacre (1993) for details of these inertia corrections.
#' @importFrom stats kmeans
#' @export
#' @references Takagishi, M. and van de Velden, M. (2022), Visualizing class specific heterogeneous tendencies in categorical data. \emph{Journal of Computational and Graphical Statistics}, 31(3), pp.790-801.
#' 
#' Benzécri, J.-P. (1979). Sur le calcul des taux d'inertie dans l'analyse d'un questionnaire. \emph{Cahiers de l'Analyse des Données}, 4, pp.377-378.
#' 
#' Greenacre, M. J. (1993). \emph{Correspondence Analysis in Practice}. Academic Press.
#' 
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




MCCCA <- function(mcccadata,K.vec=NULL,nstart=300,maxit=50,ndim=2,tol=1e-6,
                       verbose=FALSE,update.kmeans=FALSE){

  nstart.k=500 ; grpbase <- FALSE ; count.inc=0 ; trace=1

  paraname=c("B","cluster allocation & cluster center")
  updateorder=c(1,2)

  if(trace>1) cat("iteration: nstart=",nstart,", maxit=",maxit,".\n")

  if(!is.null(mcccadata)){
    #browser()
    data.list<-mcccadata$act.list
    #data.mat=mcccadata$data.mat
    dat=mcccadata$dat.act
    mcccadata$act.list<-mcccadata$dat.act<-NULL#reduce memory
    N.vec<-mcccadata$N.vec #; Ktrue.vec<-mcccadata$Ktrue.vec
    class.index.n.vec<-mcccadata$class.index.n.vec
    q.vec=mcccadata$q.vec
    classlabel<-mcccadata$classlabel
  }else{
    stop("mcccadata is NULL")
  }

  if(is.null(K.vec)){
    stop("Specify K.vec (An integer vector of length C (the number of cluster), giving the number of cluster for each class.))")
  }

  N <- sum(N.vec) ; K <- sum(K.vec) ; C <- length(N.vec)
  J <- ncol(dat)
  #J=length(q.vec)# # of vari for cate
  #Jnum=J-Jc
  Jn=sum(q.vec==0)#q.vec=0 means num vari
  Jc=J-Jn
  
  # if(Jn!=0){
  #   stop("Numeric variables are included!\n")
  # }

  #######check###########
  if(C!=length(K.vec)){
    stop("Specified K.vec (the number of clusters for each class) is inappropriate.\n The length of K.vec should be the same as the number of class.")
  }

  ##delete knowncluster.list (23/11/21)

  #####check if K.vec is appropriate######
  known.vec=rep(FALSE,length(K.vec))

  if(any(N.vec==1)){
    message("Some classes have only one subject, so the clusters of that class are set to be known.")
    K.vec[N.vec==1]=1 ; known.vec[N.vec==1]=TRUE
    #print(K.vec)
  }
  #consider the case that any specified Kc (the number of the cth class-specific cluster) is
  #more than Nc (the number of observation in the cth class).
  moreKthanN=sapply(c(1:C),function(x){K.vec[x]>N.vec[x]})
  if(any(moreKthanN)){
    #whcls=which(moreKthanN)
    stop("inappropriate K.vec (some specified number of class-specific cluster is more than the number of observaton in that class)")
  }

  #consider the case Kc is equal to Nc
  KequalN=sapply(c(1:C),function(x){K.vec[x]==N.vec[x]})
  ##if K=n, set known.vec to be TRUE
  if(any(KequalN)){
    whcls=which(KequalN)#warning("")
    known.vec[whcls]=TRUE
  }

  ##if K=1, set known.vec to be TRUE
  if(any(K.vec==1)){
    known.vec[K.vec==1]=TRUE
  }
  #####end check of K.vec######

  if(all(known.vec)){
    if(all(K.vec==1)){
      message("Averaging approach")
    }else{
      message("all known cluster")
    }
    paraname<-c("B & cluster center")
    updateorder<-1 ; maxit <- 1 ; nstart <- 1
  }

  ###########define several commands###########
  known.vec.up <- known.vec
  cluster.vec<-rep(seq(1,C),times=K.vec)
  names(cluster.vec)=paste("cluster",c(1:K))
  if(is.null(classlabel))classlabel <- paste0(seq(1,C),"class")
  #Jn <- diag(N)-(1/N)*(rep(1,N)%*%t(rep(1,N)))
  Mn=create_Mn(N)


  ####check which variable is numeric or categorical#####
  #browser()
  #if(!is.data.frame(data.mat))data.mat=as.data.frame(data.mat)
  #data.mat=data.frame(data.mat, stringsAsFactors = TRUE)
  
  ##delete 26/7/14
  #dat=data.frame(as.matrix(dat), stringsAsFactors = TRUE)
  #(24/5/30)use as.matrix to use factor variables (otherwise those remain character variables)
  ##delete 26/7/14
  # for(j in 1:ncol(dat)){
  #   if(is.character(dat[,j])){
  #     dat[,j]=as.factor(dat[,j])
  #   }
  # }
  dat <- droplevels(dat)#remove necessary levels
  
  if (!all(vapply(dat, is.factor, logical(1)))) {
    stop("All active variables in mcccadata must be factors.")
  }

  #####transform for numeric#####
  #anychara=sapply(dat, is.character)
  #dat[,anychara]=sapply(dat[.anychara],as.factor)  ##does not work
  numvars <- rep(FALSE, ncol(dat)) #sapply(dat, is.numeric)
  anynum <- FALSE #any(numvars)
  catvars <- rep(TRUE, ncol(dat))#sapply(dat, is.factor)
  anyfact <- TRUE #any(catvars)
  #browser()
  #browser()
  
  ##for mixed
  if(anynum) stop("\nNumeric variables are included in data!!\n")
  # if (!anynum)
  #   cat("\nNo numeric variables in data.\n")
  # if (!anyfact)####for mixed
  #   cat("\nNo categorical (factor) variables in data!. \n")
    #cat("\nNo categorical (factor) variables in data. \n")
  #browser()
  # if(!anynum & !anyfact){##for mixed
  #   stop("error! Neither variable is considered as numeric or categorical.")
  #   #browser()
  # }

  ####create data matrix for numeric and categorical variables (Xtilde)####

  if(anyfact){
    #browser()
    data.cate=dat[,catvars,drop=FALSE]
    dummy.res=dummy.data.frame.ed(data.cate,sep=":", dummy.classes = "ALL")
    dummy.mat=as.matrix(dummy.res$df)
    #browser()
    catename.vec=dummy.res$category.vec
    rm(dummy.res)

    #browser()
    ##make dummy.diag(block diag)
    dummy.list=rep(list(NA),ncol(data.cate))
    #for(j in 1:J){
    for(j in 1:ncol(data.cate)){
      #as.matrix is needed for block.diag function
      dummy.list[[j]]=as.matrix(dummy.data.frame.ed(data.cate[,j,drop=FALSE],sep=":", dummy.classes = "ALL")$df)
    }
    dummy.diag=do.call(args=dummy.list,what=block.diag)
    rm(dummy.list)#to reduce memory
    #browser()

    catename.vari.vec=colnames(dummy.mat)

    ###########check missing categories###########
    #if theres any col vector that is all 0 (meaning nobody choses the category)
    dummycheck <- apply(dummy.diag,2,function(x)all(x==0))

    if(any(dummycheck)){
      whichzero<-which(dummycheck)
      warning("there are categories no one chooses, so remove those from data.")
      #warning(paste(whichzero,collapse = ","),"th colvec is all 0 (meaning nobody choses the category)\n")
      #browser()
      #if(remove.miss){
        dummy.mat <- dummy.mat[,-whichzero]
        dummy.diag <- dummy.diag[,-whichzero]

        ncatesum <- sapply(c(1:Jc),function(x){sum(q.vec[c(1:x)])}) #+ 1
        ncatesum <- c(0,ncatesum)
        zerovari <- rep(NA,length(whichzero))
        jz <- 1
        for(jz in 1:length(whichzero)){
          #choose the biggest elements in ncatesum among those which are smaller than whichzero[jz]
          whichbig <- which(whichzero[jz] > ncatesum)
          #if whichbig is NULL, this means whichzero[jz] is in the 1st vari.
          #zerovari[jz] <- ifelse(length(whichbig)!=0,whichbig,1)
          zerovari[jz] <- max(which(whichzero[jz] > ncatesum))
          #whichvari <- ; cate.rem <- whichzero[jz]-ncatesum[whichvari] #which category in that variable
        }
        #cat(paste(whichzero,collapse = ","),"th colvec is all 0 (meaning nobody choses the category\n")
        #cat("cate in the ",zerovari,"vari is removed\n")
        #browser()
        q.vec[catvars] <- q.vec[catvars] - table(factor(zerovari,levels=c(1:Jc)))
        #q.vec[]
        catename.vec <- catename.vec[-whichzero]
        catename.vari.vec <- catename.vari.vec[-whichzero]
        #cat(q.vec,"\n")
      #}
      cate.removed <-whichzero #TRUE
    }else{cate.removed <- NULL}#if !any(dummycheck) for remove.miss

    ###q.vec only for categorical
    q.vec.cate=q.vec[catvars]

    #####calculate Ddiag.sq (used for update B)#####
    #browser()
    Ddiag.sq=calc_Ddiag_sq(dummy_diag = dummy.diag)
    X.cate=calc_MnZD(Mn=Mn,dummy_mat = dummy.mat,Ddiag_sq=Ddiag.sq)
  }else{##end anyfact
    X.cate<-catename.vari.vec<-dummy.diag<-catename.vec<-cate.removed<-NULL
    Ddiag.sq<-q.vec.cate<-NULL
  }

  if(anynum){
    #browser()
    X.num=scale(dat[,numvars])
    colnum=colnames(dat)[numvars]
    which.num=c(1:ncol(X.num))
    q.vec.num=rep(0,ncol(X.num))
  }else{
    X.num<-colnum<-which.num<-q.vec.num<-NULL
    }
  #browser()
  #Xtil=X.cate#
  Xtil=cbind(X.num,X.cate)
  #which.num=c(1:ncol(X.num))
  colnames(Xtil)=c(colnum,catename.vari.vec)
  q.vec2=c(q.vec.num,q.vec.cate)#q.vec after sorted
  #browser()

  #print("Xtil")
  #print(Xtil)

  #######prepare for iteration of init########
  allstep<-(maxit+1)*length(paraname)
  #OB.mat<-matrix(0,nstart,allstep)
  #down.para.mat<-matrix(0,nstart,allstep)
  res.list<-rep(list(NA),nstart)
  optval.vec <- stepconv.vec <- count.inc.vec <-rep(0,nstart)
  tot.list <- sapply(c(1:nstart),list)

  #######start iteration for initial values########
  #for(ti in 1:nstart){
  oneinit.func <- function(ti){

    #if(trace>1) cat("  ",ti,"th initial value.\n")
    if(trace>1) cat("  running the ",ti,"/",maxit,"th initial value.\n")
    #cat("running the ",ti,"/",maxit,"th initial value.\n")

    kmeans.initial.ti<-ifelse(ti==1,update.kmeans,FALSE)
    known.vec.up<-known.vec

    ######paralist
    Ugrp.para <- G.para <- B.para <- Distmat <- rep(list(NA),maxit)

    #browser()
    ###initial
    #Ugrp1.list<-rep(list(NA),C)
    U.new=matrix(0,N,K) #U1->U.new(22/9/12)
    cc<-1
    for(cc in 1:C){
      Kc<-K.vec[cc] ; Nc<-N.vec[cc]
      #Ugrp1.list[[cc]]<-matrix(0,Nc,Kc)
      #browser()
      if(known.vec[cc]){
        if(Kc==1){
          Uc<-matrix(1,Nc,Kc)
        }else if(Kc==Nc){
          Uc=diag(Nc)
        }#delete the case that knowncluster.list is known (23/11/21)
        U.new[class.index.n.vec==cc,cluster.vec==cc]=Uc
      }else{ #if its not known

        if(kmeans.initial.ti){
          #browser()
          #kres=kmeans(dummy.mat[class.index.n.vec==cc,],Kc,nstart = nstart.k)
          kres=kmeans(Xtil[class.index.n.vec==cc,],Kc,nstart = nstart.k)
          cls.init <- kres$cluster

        }else if(!kmeans.initial.ti){##end kmeans.initial
          #remove while (5/23)
          #browser()
          kinit.moto <- rep(c(1:Kc),Nc)[c(1:Nc)]
          cls.init <- kinit.moto[sample(Nc,Nc)]
          #cls.init<-sample(Kc,Nc,replace=TRUE)
        }
        Uc=1.0*outer(cls.init, 1:Kc, "==")
        U.new[class.index.n.vec==cc,cluster.vec==cc]=Uc
      }###end k is unknown
    }#end all data
    ##make b-diag
    #Ugrp.para[[1]]<-U1 #com out(22/9/12)

    #browser()
    break.ite<-FALSE ; OB.vec<-rep(0,allstep)

    #############start iteration##############
    ite.ob<-1
    ite.cate<-1 ; ite.u<-1 ; ite.g<-1
    ite<-1
    for(ite in 1:maxit){
      U.old=U.new
      if(ite>1){B.old=B.new;G.old=G.new}

      if(trace>2) message("  ite=",ite,".\n")
      #browser()
      paran<-updateorder[1]
      for(paran in updateorder){

        if(trace>2) message("    ite.cate=",ite.cate,",ite.u=",ite.u,",ite.g=",ite.g,"\n")

        if(paran==1){# B update
          ####update quantification of categories###
          #if(trace>3) message(paste("update",paraname[paran]),".")
          ite.cate<-ite+1
          #U=Ugrp.para[[ite.u]]

          # updateQcate.res<-updateQcate.MCCCA(dummy.mat=dummy.mat,Ddiag.sq=Ddiag.sq
          #                                           ,Ugrp=U,m=J,lowdim=p,
          #                                           #grpbase=grpbase,
          #                                           printcheck=trace,ncatevec=q.vec)
          # lambda.vec<-updateQcate.res$lambda.vec
          #
          # if(is.complex(lambda.vec)){
          #   cat("WARN:theres complex values.\n")
          #   B.para[[ite.cate]] <- B.para[[ite.cate-1]]
          #   break.ite <- TRUE
          #   #break
          # }else{
          #   B.para[[ite.cate]]<-updateQcate.res$Qcate.mat
          # }
          #browser()
          #B.new=updateB(dummy_mat=dummy.mat,Ddiag_sq=Ddiag.sq
          #        ,U=U.old,Jn=Jn,m=J,lowdim=p)
          B.new=updateB(Xtil=Xtil,U=U.old,J=J,lowdim=ndim)#Bstar
          #delete code memo for mixed(24/6/7)

          #####update cluster center#####
          ite.g<-ite+1
          #JZB <- Jn%*%dummy.mat%*%B.para[[ite.cate]]
          #JZB=calc_JnDumB(Jn=Jn,dummy_mat = dummy.mat,B=B.new)
          XB=Xtil%*%B.new
          #G.para[[ite.g]] <- (1/J)*(solve(t(U.old)%*%U.old)%*%t(U.old)%*%JZB) #com out(22/9/12)
          #G.new <- (1/J)*(solve(t(U.old)%*%U.old)%*%t(U.old)%*%JZB)
          #G.new=calc_updateG(U=U.old, X=JZB,J=J)
          G.new=calc_updateG(U=U.old, X=XB,J=J)
          #browser()
          #print("G")
          #print(G.new)

        }else if(paran==2){# cluster allpcation

          #if(trace>3) message(paste("update",paraname[paran]),".")

          #####update cluster center#####
          #browser()
          ite.u<-ite+1
          updateCluster.res <- updateUG.MCCCA(data.k=XB,#((1/J)*(JZB)),
                                              class.n.vec=class.index.n.vec,cluster.vec=cluster.vec,
                                              Ggrp=G.new,knownvec=known.vec.up,
                                       #Ggrp=G.para[[ite.g]],knownvec=known.vec.up,
                                       U0=U.old,K.vec=K.vec,n.vec=N.vec,
                                       #U0=Ugrp.para[[ite.u-1]],K.vec=K.vec,n.vec=N.vec,
                                       total.init.k=nstart.k,use.kmeans = update.kmeans)

          if(updateCluster.res$empty.cls){
            #message("  theres an empty clusters.")
            #message("In ",ite,"th update, error occured in kmeans. The update of U is skipped.")
            if(update.kmeans){
              #message("The update of the indicator matrix is skipped.")
            }else{
              #message("The update of the indicator matrix is skipped because there are empty clusters.")
            }
            U.new=U.old
            #Ugrp.para[[ite.u]]<-Ugrp.para[[ite.u-1]] #com out(22/9/12)
            #Ggrp.para[[ite.g]]<-Ggrp.para[[ite.u]]
          }else{
            #G.para[[ite.g]]<-updateCluster.res$Ggrp #com out(22/9/12)
            #Ugrp.para[[ite.u]]<-updateCluster.res$Ugrp
            G.new<-updateCluster.res$Ggrp
            U.new=updateCluster.res$Ugrp
          }

          #print(G.new)

          #U=Ugrp.para[[ite.u]]

        }##end cluster allocation

        #if(trace) cat(table(apply(Ugrp.diag,1,which.max)))
        #if(trace>3){
        #  message("  # of ind for each clusters.") ; cat("  ",colSums(U),"\n")
        #}
        if(any(colSums(U.new)==0)){
          #if(length(table(apply(Ugrp.diag,1,which.max)))<sum(K.vec)){

          #the data which yields 0 cluster is set to fix
          which0<-which(colSums(U.new)==0)
          known.vec.up[cluster.vec[which0]]<-TRUE

          #if(trace>0) {
            message("# of cluster has decreased.")
            if(all(known.vec.up))message("all known.vec is TRUE!")
          #}

          #K.vec
          ###only for that data, previous res is used.
          for(ll in 1:length(which0)){
            ll.d<-cluster.vec[which0[ll]]
            #Ugrp.para[[ite.u]][,cluster.vec==which0[ll]]=Ugrp.para[[ite.u-1]][,cluster.vec==which0[ll]] #com out(22/9/12)
            U.new[,cluster.vec==which0[ll]]=U.old[,cluster.vec==which0[ll]]
            #Ugrp.para[[ite.u]][[ll.d]]<-Ugrp.para[[ite.u-1]][[ll.d]]
          }
          #U=Ugrp.para[[ite.u]] #com out(22/9/12)
          #U.new=U.new
          #if(trace>3) cat("  colsum(U) is ",colSums(U),"\n") #check
          #if(trace>3) cat("  known.vec",known.vec.up,"\n")
          # break.ite<-T
          # break
        }

      }#####end paran######

      ite.ob=ite

      ####calculate obj only after updating UG
      #com out(22/9/12)->removed 22/12/15
      #if(trace>4) cat("calculate obj function\n")
      #OB.vec[ite.ob]<-obj(dummy_mat=dummy.mat,dummy_diag=dummy.diag,
      #                   U=U,G=G.para[[ite.g]], B=B.para[[ite.cate]], Jn=Jn)
      #OB.vec[ite.ob]<-obj(dummy_mat=dummy.mat,dummy_diag=dummy.diag,
      #                  U=U.new,G=G.new, B=B.new, Jn=Jn)
      OB.vec[ite.ob]<-obj(XB=XB,U=U.new,J=J)#dummy_diag=dummy.diag)

      #ite.ob<-ite.ob+1
      #browser()
      #com out(22/9/12)-> removed (12/15)
      if(verbose) message("Start", formatC(ti, width = 5, digits = 0, mode = "integer"),
                      "Iter", formatC(ite, width = 5, digits = 0, mode = "integer"),
                      "Loss", formatC(OB.vec[ite.ob], width = 10, digits = 8, format = "f"),
                      "Diff", formatC(ifelse(ite==1,0,OB.vec[ite.ob]-OB.vec[ite.ob-1])
                      #"Diff", formatC(ifelse((ite==1 & paran==updateorder[1]),0,OB.vec[ite.ob]-OB.vec[ite.ob-1])
                                      #"Diff", formatC(ifelse((ite==1 & paran==updateorder[1]),0,OB.vec[1]-OB.vec[ite.ob-1])
                                      , width = 10, digits = 12, format = "f"))

      ##for package (com out 22/12/15)
      #if(verbose) cat("Start", formatC(ti, width = 3, digits = 0, mode = "integer"),
      #                "Iteration", formatC(ite, width = 3, digits = 0, mode = "integer"),
      #                "\n")

      if(ite.ob>1){

        if(round(OB.vec[ite.ob]-OB.vec[ite.ob-1],6)>0){
        #if((OB.vec[ite.ob]-OB.vec[ite.ob-1])>0){
          #cat("obj is increased!!!!")
          count.inc=count.inc+1
          #cat("\n",count.inc,"\n")
          #browser()
        }

        if(abs(OB.vec[ite.ob]-OB.vec[ite.ob-1])<tol){ #com out(22/9/12)
        #if(sum(abs(B.new-B.old))<tol){
          #browser()
          #if(trace>2) cat(paste("  ",ite,"th ite converge at",paraname[paran],"update.\n"))
          #OB.vec[(ite.ob+1):length(OB.vec)]<-OB.vec[ite.ob]
          #convergence<-TRUE
          #break
          ####calculate obj only after iteration converged
          #if(trace>4) cat("calculate obj function\n")
          #com out 22/12/15
          #optval <- obj(dummy_mat=dummy.mat,dummy_diag=dummy.diag,
          #                    U=U.new,G=G.new, B=B.new, Jn=Jn)
          optval=OB.vec[ite.ob]

          break.ite<-TRUE
          break
        }#else{
        # convergence<-FALSE
        #
      }

      ##if all cluster is known, calculaate obj value after the 1st (=last) iteration has finished
      if(maxit==1 & ite.ob==1){
        OB.vec[ite.ob]<-optval<-obj(XB=XB,U=U.new,J=J)
        #cat("conerge2!!!!\n")
        break.ite<-TRUE
        break
      }

      #ite.ob<-ite.ob+1

      ###old####
      # para.list.now<-list(dummy.mat=dummy.mat,dummy.diag=dummy.diag,Qcate=B.para[[ite.cate]]
      #                     ,Ugrp=U,Ggrp=G.para[[ite.g]],ncatevec=q.vec,grpbase=grpbase)
      # obcheck.res<-objcheck.func(para.list=para.list.now,ite=ite,OB.vec=OB.vec,printcheck=trace
      #                            ,paraname.p=paraname[paran],ite.ob=ite.ob,obcheck.start=1,e.cri=tol)
      # OB.vec<-obcheck.res$OB.vec

      #down.para.vec[ite.ob]<-obcheck.res$down.para.save
      #ite.ob<-obcheck.res$ite.ob
      #only first cat, Dif is 0, otherwise print obval0-obval.

      # if(obcheck.res$convergence){
      #   break.ite<-TRUE
      #   break
      # }

      if(break.ite)  {
        #if(trace>2) cat("  stop it!!!\n")
        break
      }


    }###################end iteration#########################

    #if(trace>1) cat("  increased para:",paste(unique(down.para.vec),collapse="/"),"\n")
    if(trace>1) cat("  ",ti,"th init, converged at",(ite),"th iteration.\n")

    #optval<-OB.vec[ite.ob-1] #com out(22/9/12)

    #############################name of row etc#########################
    ##Bstar (mainly used for numeric variables, but save for both variables)
    Bp_star=B.new #; Bp=Bmat
    #rownames(Bp)<-catename.vari.vec#stringr::str_sub(catevec,start=4)#,end=shortlab.ps)
    #catevec#rep(0,nrow(Bp))

    ##Bmat for categorical variable
    #browser()
    #Bp_cate=sqrt(N*J)*(Ddiag.sq%*%B.new)#til 23/11/16
    rownames(Bp_star)<-colnames(Xtil)
    colnames(Bp_star)<-paste0("dim",c(1:ndim))

    if(anynum & anyfact){#if mixed data
      Bp_cate=sqrt(N*J)*(Ddiag.sq%*%B.new[-which.num,drop=FALSE,])
      B_num<-Bp_star[which.num,drop=FALSE,]
      #Bp_cate=Bp_cate[-which.num,drop=FALSE,]
      rownames(Bp_cate)<-colnames(Xtil)[-which.num]
      colnames(Bp_cate)<-colnames(B_num)<-paste0("dim",c(1:ndim))
    }else if(!anynum & anyfact){#if theres no numeric
      Bp_cate=sqrt(N*J)*(Ddiag.sq%*%B.new)
      rownames(Bp_cate)<-colnames(Xtil)
      colnames(Bp_cate)<-paste0("dim",c(1:ndim))
      B_num<-NULL
    }else if(anynum & !anyfact){#if there is no categorical
      Bp_cate=NULL
      B_num=Bp_star
    }
    #browser()
    #colnames(Bp_cate)<-colnames(B_num)<-

      #else{
    #  Bp_cate=sqrt(N*J)*(Ddiag.sq%*%B.new)#[-which.num,drop=FALSE,])
    #  rownames(Bp_cate)<-colnames(Xtil)#[-which.num]
    #}

    #browser()

    ##create U and G and Q
    #Ugrp.p<-Ugrp.para[[ite.u]] ; Gp<-G.para[[ite.g]] #com out(22/9/12)
    #Ugrp.p=U.new ; #Gp=G.new

    ####################finish all for that initial value####################
    return(list(G=G.new,B=Bp_cate,B_num=B_num, Bstar=Bp_star,U=U.new,#Fn=Fp,#Qnum=Qp_num,
                #cluster.n.vec=cluster.n.vec,cluster.n.list=cluster.n.list,
                optval=optval,stepconv=ite,count.inc=count.inc))


  }#######################end initial value func#########################

  ## Apply algorithm over all starts
  res.list <- lapply(X = tot.list, FUN = oneinit.func)

  ti <- 1
  for(ti in 1:nstart){
    optval.vec[ti] <- res.list[[ti]]$optval
    #OB.mat[ti,]<-res.list[[ti]]$OB.vec
    #down.para.mat[ti,]<-res.list[[ti]]$down.para.vec
    stepconv.vec[ti] <- res.list[[ti]]$stepconv
    count.inc.vec[ti]<- res.list[[ti]]$count.inc
  }



  ######decide minimum obvalue######
  res.list.min<-res.list[[which.min(optval.vec)]]
  
  
  #####enter row and col names#####
  #objscale<-1/J #before mixed
  objscale<-1 #if using B*, B should not require scaling(22/12/30)
  Bstar=res.list.min$Bstar
  G=res.list.min$G
  Fn=Xtil%*%Bstar
  U=res.list.min$U
  
  rownames(G)<-rep(classlabel,times=K.vec)
  
  #Qp<-objscale*(Jn%*%dummy.mat%*%Bp)
  ###22/12/23 tempolary com out
  #####cate
  # if(anyfact){
  #   if(anynum){#if there're both variables
  #     Qp<-objscale*calc_JnDumB(Jn=Jn,dummy_mat = dummy.mat,B=Bp[-which.num,drop=FALSE,])
  #   }else{
  #     Qp<-objscale*calc_JnDumB(Jn=Jn,dummy_mat = dummy.mat,B=Bp)
  #   }
  # }else{
  #   Qp=NULL
  # }
  
  # if(anynum){
  #   Qp_num=X.num%*%Bp[which.num,drop=FALSE,] #X-UGB'(nxp matrix)
  # }else{
  #   Qp_num=NULL
  # }
  #rownames(Qp)<-rownames(Ugrp.p)<-rownames(dummy.mat)#paste0("obj",c(1:N))#rep("",N)
  #Qp=NULL
  cluster.index.n.list<-rep(list(NA),C) ; cluster.index.n.vec<-rep(NA,N)
  names(cluster.index.n.list)=classlabel
  names(cluster.index.n.vec)<-rownames(dat)

  for(cc in 1:C){
    #print(cc)
    #browser()
    
    ###G
    ddk<-ifelse(cc!=1,sum(K.vec[c(1:(cc-1))]),0)
    dvec<-c((ddk+1):(ddk+K.vec[cc]))
    #rownames(Gp)[dvec]<-sapply(seq(1,K.vec[cc]),function(x)paste(cc,x,sep="-"))
    rownames(G)[dvec]<-clstlabel<-sapply(seq(1,K.vec[cc]),function(x)paste0(classlabel[cc],x))#,sep="-"))
    #seq(1,K.vec[cc])
    
    ###U
    #rownames(Ugrp.p[[cc]])<-sapply(seq(1,N.vec[cc]),function(x)paste(cc,x,sep="-"))
    #seq(1,N.vec[cc])
    Uc=U[class.index.n.vec==cc,cluster.vec==cc,drop=FALSE]
    cluster.index.n.list[[cc]]<-apply(Uc,1,which.max)+ifelse(cc!=1,sum(K.vec[c(1:(cc-1))]),0)
    cluster.index.n.vec[class.index.n.vec==cc]=cluster.index.n.list[[cc]]
    #cluster.n.vec.label[class.index.n.vec==cc]<-
    #if(cc==2)browser()
  }
  
  
  
  ####colnames
  colnames(G)<-colnames(Fn)<-paste0("dim",c(1:ndim))
  
  ##cluster.n.list
  #browser()
  
  #res.list.min$Fn=Fn
  
  ######sort cluster quantifications######
  #the class-specific cluster numbers are sorted in the order of cluster size.
  #browser()
  #levels(clses.f)[c(1,2)]<-c(2,1)
  cluster.index.n.vec.save <- cluster.index.n.vec#<-res.list.min$cluster.n.vec
  #cluster.n.list.save<-cluster.n.list
  G.save <- G#<- res.list.min$G
  
  #browser()
  
  dd<-1
  for(dd in 1:C){
    #cls.d <- cluster.vec[cluster.vec==dd]
    which.cls <- which(cluster.vec==dd)
    
    if(length(which.cls)!=1){
      #  browser()
      G.d <- G.save[which.cls,]
      #clses.vec[which(which.cls %in% clses.vec)]
      clses.d <- cluster.index.n.vec.save[which(cluster.index.n.vec.save %in% which.cls)]
      
      tabcls <- as.numeric(table(clses.d))
      srt <- sort(tabcls,index.return=TRUE,decreasing = TRUE)
      sortind <- srt$ix
      table(clses.d)[sortind]
      clses.f <- factor(clses.d)
      levels(clses.f)[sortind] <- levels(clses.f)
      table(clses.f) #biggest size cluster has index 1.
      
      ######sort cluster mean
      G[which.cls,] <- G.d[sortind,]
      #browser()
      #cluster.n.vec[which(cluster.n.vec.save %in% which.cls)] <- as.numeric(as.character(clses.f))
      cluster.index.n.list[[dd]] <- cluster.index.n.vec[which(cluster.index.n.vec.save %in% which.cls)] <- as.numeric(as.character(clses.f))
    }
  }##end for data

  cluster.label.n.vec<-rownames(G)[cluster.index.n.vec]
  names(cluster.label.n.vec)<-names(cluster.index.n.vec)
  cluster.label.n.list<-cluster.index.n.list
  
  for(cc in 1:C){
    cluster.label.n.list[[cc]]<-rownames(G)[cluster.index.n.list[[cc]]]
    names(cluster.index.n.list[[cc]])<-names(cluster.label.n.list[[cc]])<-row.names(data.list[[cc]])
    #print(table(cluster.n.list[[cc]]))
    #print(table(cluster.n.list.label[[cc]]))
  }
  #browser()
  
  # res.list.min$cluster.n.vec<-cluster.n.vec#.save
  # res.list.min$cluster.n.list<-cluster.n.list
  # res.list.min$cluster.n.vec.label<-cluster.n.vec.label
  # res.list.min$cluster.n.list.label<-cluster.n.list.label
  res.list.min$cluster.index.n.vec<-cluster.index.n.vec#.save
  res.list.min$cluster.index.n.list<-cluster.index.n.list
  res.list.min$cluster.label.n.vec<-cluster.label.n.vec
  res.list.min$cluster.label.n.list<-cluster.label.n.list
  
  res.list.min$G<-G
  res.list.min$U<-NULL

  #####gamma scale##########
  #if (scale.gamma) {
  #Fn<-res.list.min$Fn
  #G<-res.list.min$G
  distG <- sum(diag(t(G)%*% G))

  ##for cate
  #browser()
  #print(res.list.min$Bg)
  if(anyfact){
    B<-res.list.min$B#[-which.num,drop=FALSE,]
    distB <- sum(diag(t(B)%*%  B))
    gam<- ((nrow(G)/nrow(B))* distB/distG)^.25

    res.list.min$Bg = (1/gam)*B
    res.list.min$Gg = gam*G
    res.list.min$Fng = gam*Fn #12/23com out->remove

    res.list.min$Bnum_g=NULL#nenno tame
  }else{#if theres no categorical, calculate gam using B_num & G
    B_num<-res.list.min$B_num#[-which.num,drop=FALSE,]
    distB_num <- sum(diag(t(B_num)%*%  B_num))
    gam_num<- ((nrow(G)/nrow(B_num))* distB_num/distG)^.25

    res.list.min$Bnum_g=(1/gam_num)*B_num
    res.list.min$Gg = gam_num*G
    res.list.min$Fng = gam_num*Fn

    res.list.min$Bg=NULL#nenno tame

  }


    #browser()
    #res.list.min$Qnumg = gam*Qnum

    ##for num
    #Bstar<-res.list.min$Bstar[which.num,drop=FALSE,]
    #if(anynum){
    #  res.list.min$B_num<-res.list.min$Bstar[which.num,drop=FALSE,]
    #}else{#if there's no numeric
    #  res.list.min$B_null=NULL
    #  Bstar<-res.list.min$Bstar#[which.num,drop=FALSE,]
    #}

    ###Bstar without gamma
    #res.list.min$Bstar=Bstar

    #Qnum=res.list.min$Qnum
    #distBstar <- sum(diag(t(Bstar)%*%  Bstar))
    #gam_star <- ((nrow(G)/nrow(Bstar))* distBstar/distG)^.25

    #res.list.min$Bg_num = (1/gam_star)*Bstar
    #res.list.min$Gg_num = gam_star*G
    #res.list.min$Qg_num = gam_star*Q
    #browser()


  #}

  #res.list.min$class.n.vec <- class.n.vec
  #res.list.min$cluster.vec <- cluster.vec
  #res.list.min$OB.mat <- OB.mat
  res.list.min$optval.vec <- optval.vec
  res.list.min$stepconv.vec <- stepconv.vec

  ###save for result
  names(cluster.vec)=rownames(G)
  res.list.min$cluster.class.index <- cluster.vec
  #browser()
  #cluster.vec.label=classlabel[cluster.vec]#(24/7/2)cluster.vec.label->cluster.label
  #cluster.label=rownames(G)
  #names(cluster.label)=names(cluster.vec)
  #res.list.min$cluster.label=cluster.label
  cluster.index=c(1:K)#rownames(G)
  names(cluster.index)=rownames(G)
  res.list.min$cluster.index=cluster.index
  
  ####added25/7/13,table of cluster sizes and relative one
  Cs=matrix(NA,K,2)
  #browser()
  #Cs[,1]=table(cluster.label.n.vec)
  Cs[,1] <- table(factor(cluster.label.n.vec, levels = rownames(G)))
  #Cs[,2]=Cs[,1]/sum(Cs[,1])
  Cs[,2]=Cs[,1]/rep(mcccadata$N.vec,K.vec)
  rownames(Cs)=rownames(G)
  #colnames(Cs)=c("cluster sizes","relative sizes within each class")
  colnames(Cs)=c("sizes","relative sizes")
  res.list.min$csize=Cs

  #for plot
  res.list.min$K.vec<-K.vec
  res.list.min$q.vec<-q.vec
  #res.list.min$q.vec2<-q.vec2 #q.vec after sorting
  res.list.min$classlabel <- classlabel
  #res.list.min$classlabel.short <- classlabel.short
  res.list.min$catename.vec=catename.vec
  res.list.min$catename.vari.vec=catename.vari.vec
  res.list.min$cate.removed=cate.removed

  #rm(res.list.min$)

 # if(save.allinit) res.list.min$allinit.list <- res.list

  #cat("obj increased",sum(count.inc.vec),"times")

  ####calculate adjusted inertia####
  iner.res=calc.inertia.func(dat)
  res.list.min$inertia=iner.res$intab
  res.list.min$inertia.G=iner.res$intab.G
  #res.list.min$iner.res=calc.inertia.func(dat)


  #class(res.list.min)<-"mixedmccca"#mixed
  class(res.list.min)<-"mccca"
  res.list.min

}



