#' plot \code{mccca} object.
#'
#' @description Shows a biplot using quantifications estimated by \code{\link{MCCCA}}.
#' @param x A \code{mccca} object, a list created by \code{\link{MCCCA}}.
#' @param main A character string giving the title of biplot. The default is "MCCCA result".
#' @param xlim A numeric vector of length 2 giving the plotting range for the x (horizontal) axis. If \code{NULL}, the range is automatically determined.
#' @param ylim A numeric vector of length 2 giving the the plotting range for the y (vertical) axis (the same role as \code{xlim}).
#' @param plot.ind If \code{TRUE}, coordinates of observations are also plotted. The default is \code{FALSE}.
#' @param classlabel A character vector of length C (C:the number of classes) giving labels for each class to be displayed on the biplot. If \code{NULL}, \code{classlabel} saved in \code{mcccadata} is used.
#' @param classlabel.legend A character vector of length C giving labels for each classes to be shown on the legend. If \code{NULL}, \code{classlabel} is used for the legend.
#' @param catelabel A character vector of length Q (Q: the total number of categories) giving labels for each category to be displayed on the biplot. If \code{NULL}, \code{rownames(B)} is used.
#' @param variname A character vector of length J (J:the number of active variables) giving labels for each variable to be displayed on the biplot. If \code{NULL}, the variable names included in the row names of \code{B} or \code{Bg} are used.
#' @param include.variname If \code{TRUE}, the variable name is included in the category label on the biplot (e.g., the category point named "male" for the variable named "v1" is displayed as "v1:male" on the biplot). The default is \code{TRUE}.
#' @param show.onlyminmax If \code{TRUE}, only the bubbles corresponding to the maximum and minimum cluster sizes are displayed on the legend. See details. The default is \code{FALSE}.
#' @param break.size A numeric vector that adjusts the size of bubble displayed on the legend. See details.
#' @param scale.gamma If \code{TRUE}, quantifications are scaled such that the average squared deviation from the origin of the row and column points is the same. See details. The default is \code{TRUE}.
#' @param scatter.level A numeric value that adjusts the scatter of points on the biplot. Larger values spread the labels farther apart to reduce overlap. The default is 2.
#' @param txtsize A numeric value that adjusts the text size of labels on the biplot. The default is 3.8.
#' @param alp.trans A numeric value from 0 to 1 which adjusts the transparency of the bubble point. The default is 0.4.
#' @param show.inertia If \code{TRUE}, the axis labels show the percentages of adjusted inertia obtained by applying MCA to the original data. The default is \code{FALSE}.
#' @param \dots Additional arguments passed to \code{\link{print}}.
#' @seealso \code{\link{MCCCA}}, \code{\link{summary.mccca}}
#' @details
#' By default, the breaks in the cluster-size legend are determined
#' automatically. Use \code{break.size} to specify the cluster sizes
#' displayed in the legend. Alternatively, if \code{show.onlyminmax = TRUE}, only the minimum and maximum cluster
#' sizes are displayed.
#'
#' The class-specific cluster points and category points may have
#' different spreads in the original coordinate system. By default,
#' the coordinates are rescaled so that the average squared distances
#' from the origin are equal for the two sets of points. To use the
#' original coordinates, set \code{scale.gamma = FALSE}.
#'
#' @importFrom ggplot2 waiver ggplot aes geom_text geom_point geom_segment geom_abline scale_color_manual scale_size labs coord_equal xlim ylim guides guide_legend theme element_text theme_bw geom_vline geom_hline arrow unit scale_size_area
#' @importFrom stringr str_sub str_detect
#' @importFrom grDevices palette dev.cur dev.off rainbow
#' @importFrom graphics par
#' @importFrom colorspace rainbow_hcl
#' @method plot mccca
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

#' @export
#' @references Takagishi, M. and van de Velden, M. (2022), Visualizing class specific heterogeneous tendencies in categorical data. \emph{Journal of Computational and Graphical Statistics}, 31(3), pp.790-801.
# @examples


##for mixed
# (at)param varilabel.cate A characteristic vector of length Q giving labels for all categories to be displayed on the biplot (Q=\code{sum(q.vec)}). If \code{NULL}, \code{rownames(B)} are used.
# (at)param varilabel.num A characteristic vector of length J1 (the number of numeric variables) giving labels for all numeric variables to be displayed on the biplot. If \code{NULL}, \code{rownames(B_num)} are used.


plot.mccca=function(x,main="MCCCA result",xlim=NULL,ylim=NULL,plot.ind=FALSE,
                         classlabel=NULL,classlabel.legend=NULL,catelabel=NULL,#varilabel=NULL,#varilabel.cate=NULL,#varilabel.num=NULL,
                         include.variname=TRUE,variname=NULL,show.onlyminmax=FALSE,break.size=NULL,
                         scale.gamma = TRUE,scatter.level=2,txtsize=3.8,alp.trans=0.4,show.inertia=FALSE,...){
                       #plot.setting=list(alp.trans=0.3,alp.seg=0.8,txtsize=3.8,txtsize.legend=10),...){#,
  #sort.clssize=TRUE ;
  #print(dev.cur())
  fontfamily="sans" ; bigger=0.2#Helvetica
  legendname="class-specific clusters" ; add.numlabel=TRUE

  ##for mixed
  #varilabel.cate=varilabel #for only categorical,use "varilabel(catelabel)"variable as "varilabel.cate"
  varilabel.cate=catelabel
  varilabel.num=NULL

  ####setting
  #plot.setting$xlimm
  #alp.trans=plot.setting$alp.trans ; alp.seg=plot.setting$alp.seg
  #txtsize=plot.setting$txtsize ; txtsize.legend=plot.setting$txtsize.legend
  #alp.trans=0.3 ;
  alp.seg=0.8 ; txtsize.legend=10

  #----------------------------------------------------#
  #####1,data preparation####
  #----------------------------------------------------#

  if(is.null(x)) stop("mccca class object is NULL!\n")

  #####data preparation####
  q.vec<-x$q.vec ; K.vec=x$K.vec
  #####para define
  K<-sum(K.vec) ; C<-length(K.vec) ; J <- length(q.vec)
  Jn=sum(q.vec==0) ; Jc=J-Jn ; Q=sum(q.vec)

  #print("not null")
  if(scale.gamma){
    Obj.cord<-x$Fng #Fng
    Cls.cord<-x$Gg#*(0.1)

    #for cate (if only numeric, Bg is NULL)
    Cate.cord<-x$Bg
    #for num (if only categorical, both is NULL)
    if(Jc!=0){#if there is categorical (meaning its not only numeric data)
      Num.cord=x$B_num
    }else{
      Num.cord=x$Bnum_g
    }

  }else{
    Obj.cord<-x$Fn
    Cls.cord<-x$G

    ##for cate (if only numeric, B is NULL)
    Cate.cord<-x$B
    ##for num (if only categorical, B_num is NULL)
    Num.cord<-x$B_num
  }
  #browser()

  if(ncol(Cls.cord)!=2){
    stop("Cordinates are not 2dim (This plot function works only for 2dim!)")
  }

  #rownames(Cls.cord) <- seq(1,nrow(Cls.cord))
  cluster.index.n.vec<-x$cluster.index.n.vec
  #Kdatavec.es<-x$Kdatavec.es
  #rownames(Cls.cord) <- seq(1,nrow(Cls.cord))

  if(!include.variname & Jc!=0){
    rownames(Cate.cord)=x$catename.vec
  }

  ####edit after modifying for mixed
  if(!is.null(variname)){
    if(length(variname)!=Jc)stop("Inappropriate variname!")
    #browser()
    variname.p=rep(variname,times=q.vec[q.vec!=0])
    rownames(Cate.cord)<-sapply(c(1:length(variname.p)),function(i){paste(variname.p[i],x$catename.vec[i],sep=":")})
  }


  if(is.null(classlabel))classlabel=x$classlabel#.short
  
  if(is.null(classlabel.legend))classlabel.legend <- x$classlabel#datalabel
  #without setting NULL, names(classlabel.legend) will be plotted in legend
  names(classlabel.legend)=NULL

  #the class-specific cluster numbers are sorted in the order of cluster size.
  #=>move to MCCCA function (24/7/1)

  ###prepare matrix####
  Cate.d<-as.data.frame(Cate.cord)
  Cls.d <- as.data.frame(Cls.cord)
  Num.d <- as.data.frame(Num.cord)
  Obj.d <- as.data.frame(Obj.cord)#com out 12/
  #Obj.d.n <- as.data.frame(Obj_n.cord)

  ######category plot########
  ###create CC.d matrix used for a plot
  #CC.d <- rbind(Cls.d,Cate.d,Num.d)#til 23/11/16
  CC.d <- rbind(Cls.d,Num.d,Cate.d)
  #Cls.d:cluster coordinates, Num.d:numeric cord, Cate.d:category cord
  colnames(CC.d)=c("dim1ori","dim2ori")

  #clsind_fac:First, The class index is repeated for the corresponding number of clusters (1,1,2,2..),
  #Next, variable index is repeated for the number of categories
  #(the maximum number of clsind_fac,C, is added so that the numbers themselves do not cover clsind_fac)
  clsind_fac=as.factor(rep(seq(1,C),K.vec))
  #clsind2<-as.factor(clsind)

  #####for mixed
  #browser()
  if(Jn==0){#if there's no numeric variable
    CC.d$item=as.factor(c(clsind_fac,rep(c(1:Jc),times=q.vec[q.vec!=0])+C))
    CC.d$varitype=c(rep(0,nrow(Cls.d)),rep(2,nrow(Cate.d)))#0:class,1:num,2:cate
  }else if(Jc==0){#if there's no categorical variable
    CC.d$item=as.factor(c(clsind_fac,c(1:Jn)+C))
    CC.d$varitype=c(rep(0,nrow(Cls.d)),rep(1,nrow(Num.d)))
  }else{# if mixed
    CC.d$item=as.factor(c(clsind_fac,c(1:Jn)+C
                          ,rep(c(1:Jc),times=q.vec[q.vec!=0])+C+Jc))
    CC.d$varitype=c(rep(0,nrow(Cls.d)),rep(1,nrow(Num.d)),rep(2,nrow(Cate.d)))
  }
  tabcls<-as.numeric(table(cluster.index.n.vec))
  #the number of clusters, categories and missing values for the number of numeric variables)
  CC.d$tabcls=c(tabcls,rep(NA,(Q+Jn)))

  ######category, class label#####
  ###cate label
  if(is.null(varilabel.cate)) varilabel.cate <- rownames(Cate.d)
  if(is.null(varilabel.num)) varilabel.num <- rownames(Num.d)

  ###cluster label####
  #if(is.null(clslabel)){###21ed
  #if(add.numlabel){###21ed
    classlabel.num <- str_sub(rownames(Cls.d),start=-1)
    #for slide and paper
    clsname <- rep(classlabel,K.vec)
    #browser()
    #check if the last character of classname is the number
    lastchara=str_sub(classlabel,start=-1)
    if(any(str_detect(lastchara,"(1|2|3|4|5|6|7|8|9|0)"))){
    #if(any(!is.na(as.numeric(lastchara)))){
      #if there is a number in the last characters of the classlabel, include "-" between classname and cluster index in cluster labels.
      classlabel.all <- mapply(function(x,y){paste(x,y,sep="-")},clsname,classlabel.num)
    }else{#if there is no number, not include "-"
      classlabel.all <- mapply(function(x,y){paste(x,y,sep="")},clsname,classlabel.num)
    }

  #labelvec<-c(classlabel.all,varilabel.cate,varilabel.num)
  labelvec<-c(classlabel.all,varilabel.num,varilabel.cate)
  CC.d$labelvec=labelvec
  #browser()

  #########color######
  col.cls <- rainbow_hcl(C)#23/10/5#, alpha = 2)

  #browser()
  ##use.defaultcol is FALSE
  #if(is.null(col.pal)) {
    col.pal<-create.color.func(J)[c(1:J)]
    if(J==1){
      col.pal<-create.color.func(2)[2]
    }
    #col.pal <- rep(1,length(col.pal))
  #}
    #browser()

  colvec <- c(col.cls,col.pal)
  CC.d$col <- colvec[CC.d$item]
  #browser()

  #browser()
  ######break.size(4/16 delete original func)#####
  if(is.null(break.size)){
    size.label <- break.size <- waiver()
  }else{
    size.label <- break.size
  }
  #print(tabcls);print(break.size)

  #----------------------------------------------------#
  #####2,use textplot(wordcloud) to avoid overlap configuration####
  #----------------------------------------------------#
  #browser()
  #cat("before on.exit",dev.cur(),"\n")
  
  ######til 26/7/16
  if (dev.cur() > 1) {
    oldpar <- par(no.readonly = TRUE)
    oldpar$new <- NULL
    on.exit(par(oldpar), add = TRUE)
  }

  txtplot.list=textplot.cord(CC.d$dim1ori,CC.d$dim2ori,labelvec,cex=scatter.level
                             ,units="figure")#,new=TRUE)
  


  Xcor=txtplot.list$cord
  lineend=txtplot.list$lineend
  Xcor.ori=txtplot.list$cord.ori

  #----------------------------------------------------#
  #####5,prepare for main plot####
  #----------------------------------------------------#
  #####data for cluster#####
  # CC.d.cls=data.frame(dim1=Xcor[,1],dim2=Xcor[,2],dim1ori=CC.d$dim1,dim2ori=CC.d$dim2
  #                     ,tabcls=CC.d$tabcls,item=CC.d$item, col=CC.d$colvec)#CC.d$item[c(1:length(tabcls))])
  CC.d$dim1=Xcor[,1] ; CC.d$dim2=Xcor[,2]

  ###data for original place of category####
  #CC.both=CC.d
  #CC.both$dim1.es=Xcor[,1]#CC.d.es$dim1
  #CC.both$dim2.es=Xcor[,2]#CC.d.es$dim2

  #----------------------------------------------------#
  #####6,main plot####
  #----------------------------------------------------#
  ###start plot
  g2=ggplot(CC.d,aes(x=dim1,y=dim2))#,shape=item,colour=item))

  #connect cord
  which.drawseg=which(!is.na(lineend[,1]))
  CC.seg=CC.d[which.drawseg,]
  g2=g2+geom_segment(data=CC.seg,aes(x=dim1ori, y=dim2ori, xend=dim1, yend=dim2)#,alpha=0.6)
                     ,color=CC.seg$col,alpha=alp.seg)

  #browser()
  ###cluster bubble#data=CC.d.cls,
  CC.d.cls <- CC.d[CC.d$varitype == 0, ]
  g2 = g2 + geom_point(data = CC.d.cls,
    aes(x = dim1, y = dim2, size = tabcls, colour = item),
    alpha = alp.trans)

  ##26/6/27made
  # g2=g2+geom_point(aes(x = dim1, y = dim2, size = tabcls,colour=item)#,shape=item)
  #                  ,alpha=alp.trans)#, show.legend = TRUE)#,shape=clusterlabel)#[CC.d.es$item])
  #data=CC.d.cls,

  ##original points of category and numeric
  #->com out(23/11/17)! plot only categorical coordinates!
  #CC.d.catenum=CC.d[-c(1:length(tabcls)),]#remove cluster points
  #CC.d.catenum=CC.d[CC.d$varitype!=0,]
  #CC.d.catenum$shape=rep(c(17,15),times=c(nrow(Cate.d),nrow(Num.d)))
  #browser()
  #g2=g2+geom_point(data=CC.d.catenum,aes(x = dim1ori, y = dim2ori)
  #                 ,color=CC.d.catenum$col,size=txtsize
  #Removing it from the function aes() will reflect the numerical value change in the txtsize(21/2/6).
  #                 , show.legend = FALSE,shape=CC.d.catenum$shape,alpha=alp.trans+0.15)#,#,shape=17

  #plot only categorical coordinates
  #browser()
  CC.d.cate=CC.d[CC.d$varitype==2,]
  CC.d.cate$shape=rep(17,nrow(Cate.d))
  #browser()
  g2=g2+geom_point(data=CC.d.cate,aes(x = dim1ori, y = dim2ori)
                   ,color=CC.d.cate$col,size=txtsize
                   , show.legend = FALSE,shape=CC.d.cate$shape,alpha=alp.trans+0.15)#,#,shape=17


  ##arrow of numeric variables
  #browser()
  #CC.d.num=CC.d[-c(1:(K+Q)),]
  CC.d.num=CC.d[CC.d$varitype==1,]#extract numeric variables
  CC.d.num$slope=CC.d.num$dim2ori/CC.d.num$dim1ori
  ##segment for num vari
  g2=g2+geom_abline(data=CC.d.num,aes(intercept=0,slope=slope),colour=CC.d.num$col,alpha=.5)
  #arrow for num vari (if theres no categorical???)
  #without arrows, the coordinates of numeric variables are unclear. so put it even in mixed plot(23/11/19.)
  #if(Jc==0){
    g2=g2+geom_segment(data=CC.d.num,aes(x=0,y=0,xend=dim1ori,yend=dim2ori)
                       ,colour=CC.d.num$col,alpha=.5,
                       arrow=arrow(length=unit(.15,"inches")))
  #}
  #browser()

  #text
  g2=g2+geom_text(data=CC.d,aes(x = dim1, y = dim2,label = labelvec)
                  ,color=CC.d$col,size=txtsize
                  , show.legend = FALSE,fontface="bold",family=fontfamily)#,

  ##obj plot
  if(plot.ind){
    #browser()
    g2=g2+geom_point(data=Obj.d,aes(x = dim1, y = dim2)#, size = tabcls,colour=item)#,shape=item)
                     ,alpha=max(alp.trans-0.1,0.1),size=1)#size:default1.5
  }

  #browser()
  #g2=g2+geom_text(data=Obj.d,aes(x = V1, y = V2),label=c(1:n)#, size = tabcls,colour=item)#,shape=item)
  #                 ,alpha=alp.trans+0.1)
  #print(g2)


  g2 <- g2 + scale_color_manual(values=colvec,#[c(1:C)]
                                name=legendname,breaks=seq(1,C),
                                #labels=c(classlabel.legend,"a","b"))
                                labels=classlabel.legend)

  #geom_point(aes(x=0,y=0, color=factor(1)), shape='X', size=5) +
  #range:actual bubble size (not the range of # of cluster size)
  #browser()
  #g2 <- g2 + scale_size(range = c(3, 10), name="cluster size",
  #                      breaks=break.size, labels=size.label)
  #browser()
  if(show.onlyminmax){
    break.vec=c(min(CC.d$tabcls,na.rm = TRUE),max(CC.d$tabcls,na.rm = TRUE))
  }else{
    break.vec=size.label
  }
  g2 <- g2 + scale_size_area(max_size=12, name="cluster size",
                             breaks=break.vec)
                      #breaks=c(110,150))#, labels=size.label)
  #print(g3)

  if(is.null(xlim)){
    if(plot.ind){
      cate.max1<-max(c(CC.d$dim1,Obj.d$dim1))#ifelse(plot.cate,max(abs(Cate.cord)),0)
      cate.min1<-min(c(CC.d$dim1,Obj.d$dim1))
    }else{
      cate.max1<-max(CC.d$dim1)#ifelse(plot.cate,max(abs(Cate.cord)),0)
      cate.min1<-min(CC.d$dim1)
    }
    xlim.c=c(cate.min1-bigger,cate.max1+bigger)
  }else{
    xlim.c=xlim
  }

  if(is.null(ylim)){
    if(plot.ind){
      cate.max2<-max(c(CC.d$dim2,Obj.d$dim2))#ifelse(plot.cate,max(abs(Cate.cord)),0)
      cate.min2<-min(c(CC.d$dim2,Obj.d$dim2))
    }else{
      cate.max2<-max(CC.d$dim2)#ifelse(plot.cate,max(abs(Cate.cord)),0)
      cate.min2<-min(CC.d$dim2)
    }
    ylim.c=c(cate.min2-bigger,cate.max2+bigger)
  }else{
    ylim.c=ylim
  }

  #g2 <- g2 + labs(x="dim 1",y="dim 2",title=paste0(main))#,"(categorical)"))
  ###adjusted inertia
  if(show.inertia){
    adine=round(x$inertia[c(1,2),2],3)*100
    g2 <- g2 + labs(x=paste0("dim1 (",adine[1],"%)"),y=paste0("dim2 (",adine[2],"%)"),title=main)#,"(categorical)"))
  }else{
    g2 <- g2 + labs(x="dim1",y="dim2",title=main)#,"(categorical)"))
  }
    #g2 <- g2 + coord_equal() #
  #g2 <- g2 + xlim(xlim.c) + ylim(ylim.c)
  g2 <- g2 + coord_equal(xlim = xlim.c,ylim = ylim.c)
  #g2 <- g2 + theme(legend.key=element_blank(), legend.title=element_blank())
  g2 <- g2+ guides(colour = guide_legend(override.aes = list(alpha = 0.7,size=5),order=1),
                   size=guide_legend(override.aes = list(alpha = 0.5),order=2),#delete(9/22)
                   linetype=guide_legend(order=3))#,reverse=TRUE))
  ##legend
  #g2 <- g2+ theme(legend.direction = "horizontal", legend.position = "bottom", legend.box = "vertical")

  g2 <- g2 + theme(text=element_text(face="bold") ,plot.title = element_text(hjust = 0.5))#+theme(plot.title = element_text(hjust = 0.5))
  g2 <- g2 + theme_bw(base_family = fontfamily) + geom_vline(xintercept=0) + geom_hline(yintercept=0)

  #legend
  g2 <- g2+ theme(legend.title = element_text(face="bold",size=txtsize.legend),#
                  legend.text =element_text(face="bold",size=txtsize.legend) )

  print(g2,...)

  #return(g2)#for paper


  #----------------------------------------------------#
  #####for numerical####
  #----------------------------------------------------#
  ##delete (4/16)


  # if(output.coord){
  #   rownames(CC.d)=labelvec
  #   K=length(classlabel.all)
  #   Cocls.mat=CC.d[c(1:K),c(1:4)]
  #   #Cocls.ori.mat=CC.d.cls[c(1:K),c(3:4)]
  #   Cocate.mat=CC.d[-c(1:K),c(1:4)]
  #   #Cocate.ori.mat=CC.d.cls[-c(1:K),c(3:4)]
  #   return(list(Cocls.mat=Cocls.mat,Cocate.mat=Cocate.mat))
  #   #return(list(Cocls.mat=Cocls.mat,Cocls.ori.mat=Cocls.ori.mat,
  #    #           Cocate.mat=Cocate.mat,Cocate.ori.mat=Cocate.ori.mat))
  # }

}
