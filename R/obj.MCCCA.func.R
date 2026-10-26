OBJfunc<-function(para.list=para.list){

  dummy.mat<-para.list$dummy.mat
  dummy.diag<-para.list$dummy.diag
  Qcate<-para.list$Qcate
  #Qobj<-para.list$Qobj
  #Qobj.k<-para.list$Qobj.k
  Ggrp<-para.list$Ggrp
  Ugrp<-para.list$Ugrp
  ncatevec<-para.list$ncatevec
  grpbase<-para.list$grpbase

  #print(Ugrp)

  n<-nrow(Ugrp)
  m<-nrow(dummy.diag)/n
  #dont use Ggrp,since its not there when updating B for the first time.
  #m<-ncol(Ggrp)
  Jn<-diag(n)-(1/n)*(rep(1,n)%*%t(rep(1,n)))

  if(grpbase){
    obvalvec<-rep(0,m)
    for(j in 1:m){#Ugrp%*%Ggrp #Qobj
      #Zj<-dummy.list[[j]]
      jjp<-ifelse(j!=1,sum(ncatevec[c(1:(j-1))]),0)
      #   #print(c((jjp+1):(jjp+ncatevec[j])))
      Zj<-dummy.mat[,c((jjp+1):(jjp+ncatevec[j]))]
      Bj<-Qcate[c((jjp+1):(jjp+ncatevec[j])),]
      obvalvec[j]<-sum((Ugrp%*%Ggrp-Jn%*%Zj%*%Bj)^2)
      #   #obvalvec[j]<-norm((Ugrp%*%Ggrp-Zj%*%Bj),type="F")^2
    }
    # obvalvec
    (obval<-(1/(n*m))*sum(obvalvec))
    #if clusterca, 1/Nm, if groupals,1/m.and ncca is 1 (cca) or N(grpals)
  }else{ #cluster CA
    PP<-t(Qcate)%*%t(dummy.mat)%*%Jn%*%Ugrp%*%solve(t(Ugrp)%*%Ugrp)%*%t(Ugrp)%*%Jn%*%dummy.mat%*%Qcate
    obval<-(1/(n*(m^2)))*(sum(diag(PP)))
    (obval<-(-1)*obval) #to make it minimization problem
  }

   #browser()
  #CardW <- t(Ugrp) %*% matrix(1,nrow(Ugrp),ncol(Qcate))
  #Uinv<-(CardW + as.numeric(CardW == 0))^-1
  #PP<-t(Qcate)%*%t(dummy.mat)%*%Jn%*%Ugrp%*%(Uinv*t(Ugrp)%*%Jn%*%dummy.mat%*%Qcate)

  list(obval=obval)

}
