# Calculates (adjusted) inertia.
#' @noRd

calc.inertia.func=function(dat){
  
  #transform to factor
  dat=data.frame(as.matrix(dat), stringsAsFactors = TRUE)
  J=ncol(dat)
  
  dummy.s=dummy.data.frame.ed(dat,sep=":", dummy.classes = "ALL")$df
  dummy.s=as.matrix(dummy.s)

  N=sum(dummy.s)
  P=dummy.s/N
  cvec=colSums(P) ; rvec=rowSums(P)
  Dc=diag(cvec) ; Dr=diag(rvec)

  #browser()

  #DPD=solve(sqrt(Dr))%*%(P-rvec%*%t(cvec))%*%solve(sqrt(Dc))
  #Dr.sqsol=solve(sqrt(Dr))
  #Dc.sqsol=solve(sqrt(Dc))
  #Dr.sqsol2=calc_Dsqsolve(D=Dr)
  #Dc.sqsol2=calc_Dsqsolve(D=Dc)
  Dr.sqsol=diag(1/sqrt(rvec))
  Dc.sqsol=diag(1/sqrt(cvec))
  #all(Dc.sqsol==Dc.sqsol2)
  #all(Dr.sqsol==Dr.sqsol2)
  #Dc.sqsol2=calc_Ddiag_sq(Dc)
  DPD=calc_3mat(A=Dr.sqsol,B=(P-rvec%*%t(cvec)),C=Dc.sqsol)
  svdP=svd(DPD)
  eig.vec=(svdP$d)^2 #same as MCAvariants package result

  #Benzecri(1979)ver(same as package)
  adjustedB.func=function(eig.vec,J){
    p=length(eig.vec)
    adB=rep(NA,p)
    for(j in 1:p){
      if(eig.vec[j]>(1/J)){
        adB[j]=((J/(J-1))*(eig.vec[j]-1/J))^2
      }else{
        adB[j]=0
      }
    }
    #browser()
    return(list(adi=adB,adi.r=adB/sum(adB)))
    #adB/sum(adB)
  }

  #browser()
  adB=adjustedB.func(eig.vec,J)

  ##Greenacre's inertia
  Q=ncol(dummy.s)
  totG=(J/(J-1))*(sum(eig.vec^2)-((Q-J)/(J^2)))
  adinerG.r=adB$adi/totG
  #browser()

  ###create table
  intab=matrix(NA,Q,3)
  intab.G=matrix(NA,Q,3)
  #browser()
  #intab[,1]=eig.vec
  intab[,1]<-intab.G[,1]<-adB$adi
  intab[,2]=adB$adi.r
  intab[,3]=cumsum(adB$adi.r)
  intab.G[,2]=adinerG.r
  intab.G[,3]=cumsum(adinerG.r)

  #browser()

  #colnames(intab) <- c("inertias","adjusted_inertias","cum_adjusted_inertias","cum_adjusted_inertias(G)")
  colnames(intab) <- c("adInertias","relative_adInertias","cum_adInertias")
  colnames(intab.G) <- c("adInertias","relative_adInertias(G)","cum_adInertias(G)")
  rownames(intab)<-rownames(intab.G)<-paste("dim",c(1:Q))
  ##cut the rest of rows where all values does not change
  whichrep=which(intab[,1]==0)[1]-1
  intab2=intab[c(1:whichrep),]
  intab2.G=intab.G[c(1:whichrep),]
  # whichrep=rep(NA,5)
  # whichrep[1]=which(intab[,1]==0)[1]
  # whichrep[2]=which(intab[,2]==0)[1]
  # whichrep[3]=which(intab[,3]==1)[1]+1
  # whichrep[4]=which(intab[,4]==0)[1]
  # whichrep[5]=which(intab[,5]==max(intab[,5]))[1]+1
  
  #intab=rbind(intab,colSums(intab))
  #rownames(intab)[Q+1]="sum"

  #print(round(intab,3))
  #return(intab2)
  return(list(intab=intab2,intab.G=intab2.G))
  #return(round(intab2,10))

  #list(inertia=eig.vec,ad.inertia=adB$adi,ad.inertia.T=adB$adi.t,ad.inertia.T.G=adinerG.t)
}
