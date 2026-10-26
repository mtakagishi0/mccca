
#' @importFrom wordcloud wordlayout
#' @importFrom graphics lines points text


textplot.cord=function (x, y, words, cex = 1, new = TRUE, show.lines = TRUE,
          ...)
{
  #browser()
  #if (new) plot(x, y, type = "n", ...)
  #if(dev.cur()==1)plot(x, y, type = "n", ...)
  #if (new)plot.new()
  #lay <- wordlayout(x, y, words, cex, ...), units="inches"
  lay <- wordlayout(x, y, words, cex, ...)
  #lay <- wordlayout.ed(x, y, words, cex, ...)
  if (show.lines) {
    lineend<-cord.ori<-matrix(NA,length(x),2)
    for (i in 1:length(x)) {
      xl <- lay[i, 1]
      yl <- lay[i, 2]
      w <- lay[i, 3]
      h <- lay[i, 4]
      if (x[i] < xl || x[i] > xl + w || y[i] < yl || y[i] >
          yl + h) {
        #points(x[i], y[i], pch = 16, col = "red", cex = 0.5)#plot
        nx <- xl + 0.5 * w
        ny <- yl + 0.5 * h
        #lines(c(x[i], nx), c(y[i], ny), col = "grey")#plot
        lineend[i,1]=nx ; lineend[i,2]=ny
        cord.ori[i,1]=x[i] ; cord.ori[i,2]=y[i]
      }
    }
  }
  cord=matrix(NA,length(x),2)
  cord[,1]=lay[, 1] + 0.5 * lay[, 3]
  cord[,2]=lay[, 2] + 0.5 * lay[, 4]
  #text(cord,words, cex = cex, ...)#to avoid plot texts
  #text(lay[, 1] + 0.5 * lay[, 3], lay[, 2] + 0.5 * lay[, 4],
  #     words, cex = cex, ...)
  return(list(cord=cord,cord.ori=cord.ori,lineend=lineend))
}
