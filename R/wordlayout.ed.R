#' @importFrom stats sd
#' @importFrom graphics strwidth strheight

wordlayout.ed <- function (x, y, words, cex = 1, rotate90 = FALSE,
               xlim = c(-Inf,Inf), ylim = c(-Inf, Inf), tstep = 0.1, rstep = 0.1, ...)
{
  tails <- "g|j|p|q|y"
  n <- length(words)
  sdx <- sd(x, na.rm = TRUE)
  sdy <- sd(y, na.rm = TRUE)
  if (sdx == 0)
    sdx <- 1
  if (sdy == 0)
    sdy <- 1
  if (length(cex) == 1)
    cex <- rep(cex, n)
  if (length(rotate90) == 1)
    rotate90 <- rep(rotate90, n)
  boxes <- list()
  for (i in 1:length(words)) {
    rotWord <- rotate90[i]
    r <- 0
    theta <- runif(1, 0, 2 * pi)
    x1 <- xo <- x[i]
    y1 <- yo <- y[i]
    #browser()
    wid <- strwidth(words[i], cex = cex[i], ...)
    ht <- strheight(words[i], cex = cex[i], ...)
    if (grepl(tails, words[i]))
      ht <- ht + ht * 0.2
    if (rotWord) {
      tmp <- ht
      ht <- wid
      wid <- tmp
    }
    isOverlaped <- TRUE
    while (isOverlaped) {
     # if (!wordcloud::is_overlap(x1 - 0.5 * wid, y1 - 0.5 * ht, wid,
      if (!.is_box_overlap(x1 - 0.5 * wid, y1 - 0.5 * ht, wid,
                      ht, boxes) && x1 - 0.5 * wid > xlim[1] && y1 -
          0.5 * ht > ylim[1] && x1 + 0.5 * wid < xlim[2] &&
          y1 + 0.5 * ht < ylim[2]) {
        boxes[[length(boxes) + 1]] <- c(x1 - 0.5 * wid,
                                        y1 - 0.5 * ht, wid, ht)
        isOverlaped <- FALSE
      }
      else {
        theta <- theta + tstep
        r <- r + rstep * tstep/(2 * pi)
        x1 <- xo + sdx * r * cos(theta)
        y1 <- yo + sdy * r * sin(theta)
      }
    }##end while
  }#end for
  result <- do.call(rbind, boxes)
  colnames(result) <- c("x", "y", "width", "ht")
  rownames(result) <- words
  result
}
