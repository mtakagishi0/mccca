##(21/12/25):dummy and dummy.data.frame func edited to get category label

#' @importFrom stats model.matrix model.frame

dummy.ed=function (x, data = NULL, sep = "", drop = TRUE, fun = as.integer,
                   verbose = FALSE)
{
  if (is.null(data)) {
    name <- as.character(sys.call(1))[2]
    name <- sub("^(.*\\$)", "", name)
    name <- sub("\\[.*\\]$", "", name)
  }
  else {
    if (length(x) > 1)
      stop("More than one variable provided to produce dummy variable.")
    name <- x
    x <- data[, name]
  }
  if(drop == FALSE && inherits(x, "factor")){
  #if (drop == FALSE && class(x) == "factor") {
    x <- factor(x, levels = levels(x), exclude = NULL)
  }
  else {
    x <- factor(x, exclude = NULL)
  }
  if (length(levels(x)) < 2) {
    if (verbose)
      warning(name, " has only 1 level. Producing dummy variable anyway.")
    return(matrix(rep(1, length(x)), ncol = 1, dimnames = list(rownames(x),
                                                               c(paste(name, sep, x[[1]], sep = "")))))
  }
  #browser()
  mm <- model.matrix(~x - 1, model.frame(~x - 1))#, contrasts = FALSE)
  #contrasts comment out (22/8/23)
  colnames.mm <- colnames(mm)
  if (verbose)
    message(" ", name, ":", ncol(mm), "dummy varibles created\n")
  mm <- matrix(fun(mm), nrow = nrow(mm), ncol = ncol(mm), dimnames = list(NULL,
                                                                          colnames.mm))
  ###only category name
  category.vec=sub("^x", "", colnames(mm))
  colnames(mm) <- sub("^x", paste(name, sep, sep = ""), colnames(mm))
  if (!is.null(row.names(data)))
    rownames(mm) <- rownames(data)
  return(list(mm=mm,category.vec=category.vec))
}

dummy.data.frame.ed=function (data, names = NULL, omit.constants = TRUE, dummy.classes = getOption("dummy.classes"),
                              all = TRUE, ...) {
  df <- data.frame(row.names = row.names(data))
  category.list=rep(list(NA),length(names(data)))
  new.attr <- list()
  #browser()
  for (nm in names(data)) {#colnames
    n.ind=which(names(data)==nm)
    old.attr <- attr(df, "dummies")
    if (nm %in% names || (is.null(names) && (dummy.classes ==
                                             "ALL" || class(data[, nm]) %in% dummy.classes))) {
      #dummies <- dummy.ed(nm, data, ...)
      dum.list <- dummy.ed(nm, data, ...)
      dummies=dum.list$mm ; category.list[[n.ind]]=dum.list$category.vec
      if (ncol(dummies) == 1 & omit.constants) {
        dummies <- matrix(nrow = nrow(data), ncol = 0)
      }
      if (ncol(dummies) > 0)
        new.attr[[nm]] <- (ncol(df) + 1):(ncol(df) +
                                            ncol(dummies))
    }
    else {
      if (!all)
        (next)()
      dummies <- data[, nm, drop = FALSE]
    }
    df <- cbind(df, dummies)
    #browser()
  }
  ##bind list
  #category.list
  category.vec=unlist(category.list)
  #browser()
  attr(df, "dummies") <- new.attr
  return(list(df=df,category.vec=category.vec))
}


# dummy.data.frame=function (data, names = NULL, omit.constants = TRUE, dummy.classes = getOption("dummy.classes"),
#           all = TRUE, ...)
# {
#   df <- data.frame(row.names = row.names(data))
#   new.attr <- list()
#   for (nm in names(data)) {
#     old.attr <- attr(df, "dummies")
#     if (nm %in% names || (is.null(names) && (dummy.classes ==
#                                              "ALL" || class(data[, nm]) %in% dummy.classes))) {
#       dummies <- dummy(nm, data, ...)
#       if (ncol(dummies) == 1 & omit.constants) {
#         dummies <- matrix(nrow = nrow(data), ncol = 0)
#       }
#       if (ncol(dummies) > 0)
#         new.attr[[nm]] <- (ncol(df) + 1):(ncol(df) +
#                                             ncol(dummies))
#     }
#     else {
#       if (!all)
#         (next)()
#       dummies <- data[, nm, drop = FALSE]
#     }
#     df <- cbind(df, dummies)
#   }
#   attr(df, "dummies") <- new.attr
#   return(df)
# }
