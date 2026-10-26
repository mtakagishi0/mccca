#' Create a list (class : mcccadata) which is later applied to MCCCA.
#'
#' @description Creates a \code{mcccadata} object which is later applied to \code{\link{MCCCA}}.
#' @usage create.MCCCAdata(dat.act,dat.ext)
#' @param dat.act An (N x J) dataframe (matrix) of active categorical variables (N:the number of observations, J:the number of active variables). If \code{rownames(dat.act)} is \code{NULL}, \code{c(obj1,..,objN)} are used as \code{rownames(dat.act)}. Missing values (\code{NA}) are not allowed, and each active variable must have at least two categories.
#' @param dat.ext An (N x H) dataframe (matrix) of external variables (H : the number of external variables). Missing values (\code{NA}) are not allowed.
#' @return Returns a list with the following elements.
#' \item{\code{C}}{The number of classes created from in this function.}
#' \item{\code{dat.act}}{A copy of \code{dat.act}.}
#' \item{\code{act.list}}{A list of C data frames, each of which is obtained by dividing \code{dat.act} into C classes.}
#' \item{\code{N.vec}}{An integer vector of length C giving the number of observations belonging to each of the C classes.}
#' \item{\code{q.vec}}{An integer vector of length J giving the number of categories in each of the J active variables.}
#' \item{\code{classlabel}}{A character vector of length C giving the class label for each of the C classes.}
#' \item{\code{class.label.n.vec}}{A character vector vector of length N indicating the class label for each observation belongs to. The order is the same as the order of the rows of \code{dat.act} (i.e., \code{names(class.label.n.vec)}=\code{rownames(dat.act)}).}
#' \item{\code{class.index.n.vec}}{An integer (with values from 1:C) vector of length N, giving the same information as \code{class.label.n.vec} but represented by class indices.}
#' \item{\code{obs.index.n.list}}{A list of C integer (with values from 1:N) vectors, with the observation indices corresponding to each of the C classes.}
#' \item{\code{classlab.mat}}{A (Cx(H+1)) table showing the correspondence between each class and the external variables. The rows indicate the classes, while the first H columns indicate the external category of each external variable that constitutes the class, and the last column indicates the class label.}
#' @export
#' @seealso \code{\link{summary.mcccadata}}, \code{\link{MCCCA}}
#' @references Takagishi, M. and van de Velden, M. (2022), Visualizing class specific heterogeneous tendencies in categorical data. \emph{Journal of Computational and Graphical Statistics}, 31(3), pp.790-801.
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
#' # Show a summary of the created classes
#' summary(mcccadata)
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

create.MCCCAdata <- function(dat.act,dat.ext){

  if (NROW(dat.act) != NROW(dat.ext)) {
    stop("dat.act and dat.ext must have the same number of rows (",
         NROW(dat.act), " and ", NROW(dat.ext), ").", call. = FALSE)
  }

  ###data seibi before splitting data
  if(is.null(rownames(dat.act))){
    rownames(dat.act)<-rownames(dat.ext)<-paste0("obj",c(1:nrow(dat.act)))}
  if(!is.data.frame(dat.act))dat.act=as.data.frame(dat.act)
  if(!is.data.frame(dat.ext))dat.ext=as.data.frame(dat.ext)

  ## Check the input data (26/10/26)
  check.mccca.input(dat.act, dat.ext)

  if(any(rownames(dat.act)!=rownames(dat.ext))){
    stop("active data and external data should have the same rownames!")
  }
  
  ## Check and convert active variables (26/7/14)
  dat.act[] <- lapply(dat.act,checkfactor)
  dat.ext[] <- lapply(dat.ext,checkfactor)

  ######first split data by the combinations of categories of external variables.
  split.list=splitdata.byext.comb(dat.act,ext=dat.ext)#,clstr0.vec =clstr0.vec )
  data.list=split.list$data.list
  #clstr.list=split.list$clstr.list
  #clstr0.list=split.list$clstr0.list
  #clstr.vec=split.list$clstr.vec
  class.index.n.vec=split.list$class.index.n.vec
  class.label.n.vec=split.list$class.label.n.vec
  obs.index.n.list=split.list$obs.index.n.list
  N.vec=split.list$N.vec #; Ktrue.vec=split.list$Ktrue.vec

  H=ncol(dat.ext)
  classlab.mat=split.list$classlab.mat
  #browser()
  classlabel=classlab.mat[,c(H+1)]
  rm(split.list)

  C <- length(data.list) ; J <- ncol(data.list[[1]])
  #browser()
  q.vec=sapply(c(1:J),function(x){length(unique(dat.act[,x]))})
  
  ##for mixed
  #numvars <- sapply(dat.act, is.numeric)
  #if (any(numvars))####for mixed
  #  stop("\nNumeric variables are included in data!!. \n")
  #q.vec[numvars]=0 #for numeric vectors, use 0

  ###output
  res <- list(C=C,dat.act=dat.act,act.list=data.list,#clstr.list=clstr.list,#clstr.vec=clstr.vec,
              N.vec=N.vec,q.vec=q.vec,#Ktrue.vec=Ktrue.vec,#q.vec.J=q.vec.J,
       class.index.n.vec=class.index.n.vec,classlabel=classlabel,classlab.mat=classlab.mat
       ,class.label.n.vec=class.label.n.vec,obs.index.n.list=obs.index.n.list)
  class(res) <- "mcccadata"

  res

}



# Check the input of create.MCCCAdata() (internal)
#
# Stops with an informative error when
#  - dat.act or dat.ext contains missing values (NA), or
#  - an active variable has only one category.
# @noRd
check.mccca.input <- function(dat.act, dat.ext) {

  # missing values
  na.msg <- function(dat, argname) {
    n.na <- vapply(dat, function(x) sum(is.na(x)), numeric(1))
    if (any(n.na > 0)) {
      vars <- paste0(names(n.na)[n.na > 0], " (", n.na[n.na > 0], ")",
                     collapse = ", ")
      paste0("  ", argname, ": ", vars)
    } else {
      NULL
    }
  }
  msg <- c(na.msg(dat.act, "dat.act"), na.msg(dat.ext, "dat.ext"))
  if (length(msg) > 0) {
    stop("Missing values (NA) are not allowed. ",
         "Variables with missing values (number of NAs):\n",
         paste(msg, collapse = "\n"),
         "\nRemove the observations with missing values first, for example ",
         "by keeping only the rows for which ",
         "complete.cases(dat.act, dat.ext) is TRUE.", call. = FALSE)
  }

  # active variables with only one category
  n.cate <- vapply(dat.act, function(x) length(unique(x)), integer(1))
  if (any(n.cate < 2)) {
    stop("Each active variable must have at least two categories. ",
         "The following active variable(s) have only one category: ",
         paste(names(n.cate)[n.cate < 2], collapse = ", "),
         ". Remove them from dat.act.", call. = FALSE)
  }

  invisible(TRUE)
}
