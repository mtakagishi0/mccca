#' @importFrom RColorBrewer brewer.pal
#' @importFrom grDevices rainbow colorRampPalette


list2mat.func<-function(data=data,inputform=c("list")#,ndata=ndata
                        ,outputform="matrix",whichsame="col"){
  ##check all dim
  ndata <- length(data)
  rowvec<-rep(0,ndata)
  colvec<-rep(0,ndata)

  for(dd in 1:ndata){
    data.d<-data[[dd]]
    rowvec[dd]<-nrow(data.d)
    colvec[dd]<-ncol(data.d)
  }

  ###para
  row.all<-sum(rowvec)
  col.all<-sum(colvec)

  data.diag<-matrix(0,row.all,col.all)

  if("matrix" %in% outputform){
    if(whichsame=="row"){
      data.mat<-matrix(0,rowvec[1],col.all)
      if(any(rowvec[1]!=rowvec))warning("To combine matrix, all row needs to be the same.")

    }else if(whichsame=="col"){
      data.mat<-matrix(0,row.all,colvec[1])
      if(any(colvec[1]!=colvec)) warning("To combine matrix, all column needs to be the same.")
    }}


  dd<-1
  for(dd in 1:ndata){
    ##row
    drow<-ifelse(dd!=1,sum(rowvec[c(1:(dd-1))]),0)
    dd.rowvec<-c((drow+1):(drow+rowvec[dd]))

    dcol<-ifelse(dd!=1,sum(colvec[c(1:(dd-1))]),0)
    dd.colvec<-c((dcol+1):(dcol+colvec[dd]))

    if("matrix" %in% outputform){

      if(whichsame=="row"){
        data.mat[,dd.colvec]<-data[[dd]]
      }else if(whichsame=="col"){
        data.mat[dd.rowvec,]<-data[[dd]]
      }

    }
    if("diag" %in% outputform){
      data.diag[dd.rowvec,dd.colvec]<-data[[dd]]
    }
  }##ite data

  #####delete unecessary one (to save memory)
  if(("matrix" %in% outputform)==FALSE){
    data.mat<-NULL}
  if(("diag" %in% outputform)==FALSE){
    data.diag<-NULL}

  list(data.mat=data.mat,data.diag=data.diag
       ,rowvec=rowvec,colvec=colvec)


}

is_integer <- function(vari) {
  all(vari %% 1 == 0)
}


create.color.func<-function(ncolor=ncolor,transpare=FALSE,printcheck=FALSE
                            ,degreeTra=80,palet1="Dark2",palet2="Paired",use.package=TRUE){

  if(use.package){
    if(ncolor<=8){#8 is max of dark2
      palet<-palet1
      cols<-brewer.pal(max(3, ncolor),palet)
    }else if(ncolor<=12){#12 is max of paired
      palet<-palet2
      #cols<-brewer.pal(12,palet)[1:ncolor]
      cols<-brewer.pal(max(3, ncolor),palet)[1:ncolor]
      #cols<-rep(brewer.pal(12,palet), ceiling(ncolor/12))[1:ncolor]
    }else if(ncolor>12){
      cols<-brewer.pal(12,palet2)
      cols=colorRampPalette(cols)(ncolor)

      #cols2<-brewer.pal(8,palet1)[c(4,8)] #pink,gray
      #cols<-rep(c(cols1,cols2), ceiling(ncolor/20))[1:ncolor]
      #if(ncolor>20)message("ncolor is >20. so same color is repeated.")
    }
  }else{
    #cols<-heat.colors(ncolor)
    #rainbow except red
    cols<-rainbow(ncolor+1)[-1]
    palet<-"defalut"
  }

  #if(printcheck) print(paste("use palette",palet))

  colvec<-cols#[seq(1,ncolor)]#c(seq((Gtrue+1),(Gtrue+sum(Kdatavec[c(1:ndata.self)]))),rep(seq(1,Gtrue),ncate.ori))
  if(transpare) colvec<-sapply(cols,function(x){paste(x,degreeTra,sep="")})

  colvec


}


.medoid<-function(x,d)
{
  minj<-0
  minsumdist<-sum(d)
  if(is.null(dim(x)) && is.null(dim(d))){
    dim(x)<-c(1,length(x))
    x
  }
  else{
    if(is.null(dim(d))){
      dim(d)<-c(1,1)
    }
    if(is.null(dim(x))){
      dim(x)<-c(length(x),1)
    }
    for(j in 1:nrow(d)){
      if (sum(d[j,])<=minsumdist){
        #minj<-row.names(d)[j]
        minj<-j
        minsumdist<-sum(d[j,])
      }
    }
    resul<-as.matrix(x[minj,])
    resul
  }
}


.is_box_overlap <- function(x, y, width, height, boxes) {
  if (length(boxes) == 0) {
    return(FALSE)
  }

  for (box in boxes) {
    x2 <- box[1]
    y2 <- box[2]
    width2 <- box[3]
    height2 <- box[4]

    if (x < x2) {
      overlap <- (x + width) > x2
    } else {
      overlap <- (x2 + width2) > x
    }

    if (y < y2) {
      overlap <- overlap && ((y + height) > y2)
    } else {
      overlap <- overlap && ((y2 + height2) > y)
    }

    if (overlap) {
      return(TRUE)
    }
  }

  FALSE
}




checkfactor<- function(x) {
  
  ## Keep factors unchanged
  if (is.factor(x)) {
    return(x)
  }
  
  ## Convert character variables to factors
  if (is.character(x)) {
    return(as.factor(x))
  }
  
  ## Treat logical variables as categorical
  if (is.logical(x)) {
    return(as.factor(x))
  }
  
  ## Numeric variables are accepted only when all values are integers
  if (is.numeric(x)) {
    is.whole <- all(
      is.na(x) |
        abs(x - round(x)) < sqrt(.Machine$double.eps)
    )
    
    if (!is.whole) {
      stop(
        "Active variables must be categorical. ",
        "Numeric variables containing non-integer values were found."
      )
    }
    
    return(as.factor(x))
  }
  
  stop("Unsupported variable type found in dat.act.")
}
