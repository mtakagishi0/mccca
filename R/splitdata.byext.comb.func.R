
splitdata.byext.comb <- function(dat,ext=ext){#,shortlab.ps=3){

  verbose <- TRUE
  #J <- ncol(dat)
  H <- ncol(ext) # the number of external variable
  if(is.null(H)){
    ext=as.matrix(ext,ncol=1)
    H=1 #if H is NULL,it means ext is not matrix but a vector with a length n,
    #so set the number of external vari as 1.
  }
  #browser()
  J=ncol(dat) ; N=nrow(dat)

  ######transform ext to data.frame of factor type


  ##########split data##########
  #ext<-matrix(as.numeric(data.all[,which.ext]),nrow(data.all),next)
  #dat<-matrix(as.numeric(data.all[,-which.ext]),ncol = (ncol(data.all)-next),byrow=F)

  ##the number of categories for each external variables
  extcate.vec<-sapply(seq(1,H),function(x){length(unique(ext[,x]))})
  extcate.list=rep(list(NA),H)
  for(hh in 1:H){
    #if(is.data.frame(dat))
    #browser()
    extcate.list[[hh]]=sort(unique(ext[,hh]))#levels(data.ext[,hh])#assume factor
    #if all is numeric, sort categories
    #=> use sort func even for characteristic (24/7/4), otherwise The order of the letters is disjointed.
    #(ex. a vector having "a","b" or "c" could be unique(vec)="a","c" "b")
    #if(all(is.numeric(extcate.list[[hh]])))extcate.list[[hh]]=sort(extcate.list[[hh]])
    extcate.vec[hh]=length(extcate.list[[hh]])
  }
  #browser()
  #when just speparating data
  #ndata<-sum(datavec.sup)
  #combining data
  C<-prod(extcate.vec)
  #browser()

  #take combination of categories in each external variable
  #extcomb <- expand.grid(lapply(c(1:H),function(x){extcate.list[[x]]}))
  #use "rev" to order 1a,1b,1c,2a,2b,2c (without rev, 1a,2a,1b,2b,1c,2c)
  extcomb <- rev(expand.grid(rev(lapply(c(1:H),function(x){extcate.list[[x]]}))))
  
  #extcate.list
  #ncomb <- nrow(extcomb)
  if(nrow(extcomb)!=C){
    # the number of combinations(=nrow(extcomb)) should be equal to C.
    #stop("something is wrong.")
    stop("external variable matrix might be inappropriate.")
  }

  #####define classlabel#####
  classlab.mat=matrix(NA,C,H+1)
  rownames(classlab.mat)=paste0(c(1:C),"th class")

  colname.ext=colnames(ext)
  if(is.null(colname.ext))colname.ext=paste0("external vari",c(1:H))
  colnames(classlab.mat)=c(colname.ext,"classlabel")#,"classlabel(short)")
  #1-Hth col:combinations of external categories,
  #H+1th col:label name
  ##add use abbreviated column names when using character class name.
  classlab.mat[,c(1:H)]=as.matrix(extcomb)#use "as.matrix" otherwise its gonna be a list
  classlab.mat[,(H+1)]=apply(extcomb,1,function(x){paste(x,collapse="&")})
  #short
  #classlab.mat[,(H+2)]=apply(extcomb,1,function(x){paste(stringr::str_sub(x,start=1,end=shortlab.ps),collapse="&")})

  #motodata.vec may not be needed... (19/6)
  #how many 1given data is repeated and how each is divided.
  #when dividing data by gender(M/W) and age(Y/M/O),
  #motodata.vec=c(1,1,2,2,2)
  #motodata.vec <- rep(seq(1,length(datavec.sup)),times=datavec.sup)

  #######save data################
  X.list<-obs.index.n.list <-rep(list(NA),C)#clstr0.list <- clstr.list <- 
  names(X.list)<-names(obs.index.n.list) <-classlab.mat[,(H+1)]#names(clstr0.list) <- names(clstr.list) <- 
  class.index.n.vec <- rep(NA,N)#clstr.vec<-
  names(class.index.n.vec)=rownames(dat)
  N.vec <- rep(NA,C)#Ktrue.vec<-
  names(N.vec) <-classlab.mat[,(H+1)]#names(Ktrue.vec)<-
  #data.vec <- seq(1,nrow(dat))#oriid.vec <-

  ####for check
  #datadis <- rep(NA,H)
  #for(tt in 1:length(datavesc.sup)){
  #for(tt in 1:C){
  #  datadis[tt] <- sum(datavec.sup[c(1:tt)])
  #}

  ###which is empty class
  emptyclass=rep(FALSE,C)

  #browser()
  cc <- emp<-1
  for(cc in 1:C){

    #comb <- as.vector(extcomb[cc,])
    comb <- extcomb[cc,] #22/9/2
    as.character(extcomb[cc,])

    ###pick up sub having the "comb" combinations of ext vari.
    pickupsub <- apply(ext,1,function(x){all(x==comb)})
    N.vec[cc] <- Nc <-sum(pickupsub)

    #if(printcheck) cat(cc,"th data, n=",sum(pickupsub),"\n")#print(paste("data",datalabel[ss]))
    #if(verbose) cat(cc,"th class:","(",paste(as.matrix(comb),collapse=","),") data, n=",Nc,"\n")
    if(verbose) message(paste0(cc,"th class: ","(",paste(as.matrix(comb),collapse=","),") data, n=",Nc,"\n"))
    #as.matrix enables "comb" to be treated as character.
    if(Nc==0){
      message("the # of observation for (",paste(comb,collapse=","),") data is 0. So skip this class.\n")
      emptyclass[cc]=TRUE
      #clstr0.list[[cc]] <- clstr.list[[cc]]<-"(empty class)"
      #Ktrue.vec[cc]=0
      #obs.index.n.list[[cc]]=NULL
      #browser()
      #emp=emp+1
      #next

    }else{#when not Nc==0
      #X.list[[cc]]<-matrix(dat[pickupsub,],Nc,J)
      #by using "matrix", this is always matrix even though Nc=1.
      #=>stop using "matrix" because strange things happen for dataframe
      #X.list[[cc]]<-dat[pickupsub,]

      X.list[[cc]]<-as.data.frame(as.matrix(dat[pickupsub,]),stringsAsFactors=TRUE)
      #using "as.matrix" to make "stringsAsFactors" work (it doesnot work for dataframe variable).
      class.index.n.vec[pickupsub]=cc
      #browser()
      obs.index.n.list[[cc]]=which(pickupsub)
      #it should work for Nc=1 case

      names(obs.index.n.list[[cc]])=NULL
      #remove name cuz name is the same as the vector value itself


      # if(!is.null(clstr0.vec)){
      #   clstr0.list[[cc]] <- clstr0.vec[pickupsub]
      #   Ktrue.vec[cc]=length(unique(clstr0.list[[cc]]))
      #   #clstr.list[[cc]]=clstr0.list[[cc]]+ifelse(cc==1,0,sum(Ktrue.vec[c(1:(cc-1))]))#sum(K.vec[c(1:cc)])
      #   #clstr.vec[obs.index.n.list[[cc]]]=clstr.list[[cc]]
      # }

    }
    #if(printcheck)print(paste("data",ss))
    #ss.de <- min(which(ss<=datadis))
    #ss.d <- ss - ifelse(ss.de==1,0,sum(extcate.vec[c(1:(ss.de-1))]))
    #pickupsub <- ext[,ss.de]==ss.d

    #print(table(ext[pickupsub,ss.de]))

    #cat("ss:",ss,"\n")
    #cat("ss.de:",ss.de,",ss.d=",ss.d,"\n")
    #,",",ss.de2
    #cat("insert data\n")
    #browser()

    #cls.tr.vec[data.vec==ss]<-
   # browser()
    #is true cluster is given, cluster lists for each classes are also made.
    #oriid.list[[cc]] <- oriid.vec[pickupsub]
    #data.vec[pickupsub] <- cc

    ####label
    # if(!is.null(extcate.label)){
    #
    # }

  }###end for class
  #browser()

  ###remove class skipped because N is 0.
  ded=1 ; class.index.n.vec.old=class.index.n.vec
  for(emp in which(emptyclass)){
    whichlist=(which(names(X.list)==classlab.mat[emp,H+1]))
    X.list[[whichlist]]<-obs.index.n.list[[whichlist]]<-NULL #clstr0.list[[whichlist]] <- 
    #X.list[[emp]]<-clstr0.list[[emp]] <- clstr.list[[emp]]<-obs.index.n.list[[emp]]<-NULL
    class.index.n.vec[emp<class.index.n.vec.old]=class.index.n.vec.old[emp<class.index.n.vec.old]-ded#mae ni tsumeru
    ded=ded+1
  }

  if(any(emptyclass)){
    classlab.mat=classlab.mat[-which(emptyclass),]
    N.vec<-N.vec[-which(emptyclass)] #; Ktrue.vec<-Ktrue.vec[-which(emptyclass)]
  }

  ###after removing empty class, redefine the class index.
  C2=nrow(classlab.mat)
  rownames(classlab.mat)<-paste0(c(1:C2),"th class")
  names(N.vec)<-mapply(function(x,y){paste(x,y,sep=":")},c(1:C2),classlab.mat[,(H+1)])#names(Ktrue.vec)<-
  names(obs.index.n.list)=mapply(function(x,y){paste(x,y,sep=":")},c(1:C2),names(obs.index.n.list))
  names(X.list)=mapply(function(x,y){paste(x,y,sep=":")},c(1:C2),names(X.list))
  #browser()
  
  ####(25/7/13added)
  if(verbose) message("The total number of classes:",C2,"\n")

  ##create character ver of class.index.n.vec
  #browser()
  class.label.n.vec=classlab.mat[class.index.n.vec,(H+1)]
  names(class.label.n.vec)=rownames(dat)
  #class.label.n.vec=mapply(function(x,y){paste(x,y,sep=":")},class.index.n.vec,class.label.n.vec)

  # if(is.null(clstr0.vec)){
  #   clstr0.list <- Ktrue.vec<-NULL#clstr.list <- clstr.vec <- NULL
  # }

  list(data.list=X.list,N.vec=N.vec,classlab.mat=classlab.mat,#,clstr0.list=clstr0.list,Ktrue.vec=Ktrue.vec#clstr.list=clstr.list,clstr.vec=clstr.vec,#,
       class.index.n.vec=class.index.n.vec,class.label.n.vec=class.label.n.vec,
       obs.index.n.list=obs.index.n.list)#,extcate.list=extcate.list)
       #data.vec=data.vec)#,motodata.vec=motodata.vec)


}
