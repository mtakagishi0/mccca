#' Plot a decideK Object
#'
#' @description
#' Displays the cluster-number selection index for each class.
#'
#' By default, up to four plots are displayed on each page in a
#' 2-by-2 arrangement. If more than four plots are available,
#' press Enter to display the next page.
#' 
#' Classes for which the index was not calculated (see \code{skipped} in
#' \code{\link{decideK}}) are not plotted.
#'
#' @param x A \code{decideK} object, a list created by
#'   \code{\link{decideK}}.
#' @param onebyone Logical. If \code{TRUE}, plots are displayed one at
#'   a time. Press Enter to display the next plot. If \code{FALSE},
#'   up to four plots are displayed on each page in a 2-by-2
#'   arrangement. The default is \code{FALSE}.
#' @param \dots Additional arguments. Currently not used.
#'
#' @method plot decideK
#' @export
#'
#' @seealso
#' \code{\link{decideK}}, \code{\link{summary.decideK}}
#'
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_text
#'   labs coord_cartesian scale_x_continuous theme_bw theme
#'   element_text margin
#' @importFrom gridExtra grid.arrange
#'
#' @examples
#' # Load the example data
#' data(accident)
#'
#' # Prepare active and external variables
#' active <- 1:4
#' external <- 5:6
#' dat.act <- accident[, active]
#' dat.ext <- accident[, external]
#'
#' # Create a mcccadata object
#' mcccadata <- create.MCCCAdata(dat.act, dat.ext)
#'
#' # Specify the candidate numbers of clusters
#' Krange <- c(2, 4)
#'
#' # Determine the numbers of class-specific clusters
#' # using a small nstart for a faster example
#' KLres <- decideK(
#'   mcccadata,
#'   Krange = Krange,
#'   nstart = 10
#' )
#'
#' # Plot the cluster validity indices
#' plot(KLres)
#'
plot.decideK <- function(x, onebyone = FALSE, ...) {
  
  ind.list <- x$ind.list
  cindex <- x$cindexnm
  index.name <- x$index
  
  plot.list <- vector(
    mode = "list",
    length = length(ind.list)
  )
  
  ## classes for which the index was not calculated
  skipped <- x$skipped
  if (is.null(skipped)) skipped <- rep(FALSE, length(ind.list))  # older objects
  
  for (cc in seq_along(ind.list)) {
    
    ## Skip classes for which the index was not calculated
    if (skipped[cc]) next
    
    ind.mat <- ind.list[[cc]]
    
    y <- as.numeric(ind.mat[1, ])
    
    k <- as.numeric(
      gsub(
        pattern = "clusters=",
        replacement = "",
        x = colnames(ind.mat)
      )
    )
    
    ## Determine the best number of clusters
    if (cindex == 4) {
      
      ## DB index: smaller is better
      best.id <- which(
        y == min(y, na.rm = TRUE)
      )[1]
      
    } else {
      
      ## KL, CH and Silhouette indices:
      ## larger is better
      best.id <- which(
        y == max(y, na.rm = TRUE)
      )[1]
    }
    
    ## Add margin to the x-axis
    xrange <- range(k, na.rm = TRUE)
    xmargin <- diff(xrange) * 0.10
    
    if (xmargin == 0) {
      xmargin <- 0.5
    }
    
    xlim2 <- c(
      xrange[1] - xmargin,
      xrange[2] + xmargin
    )
    
    ## Add margin to the y-axis
    yrange <- range(y, na.rm = TRUE)
    ymargin <- diff(yrange) * 0.15
    
    if (ymargin == 0) {
      ymargin <- abs(yrange[1]) * 0.15
    }
    
    if (ymargin == 0) {
      ymargin <- 1
    }
    
    if (cindex == 3) {
      
      ## Silhouette index
      ylim2 <- c(
        max(-1, yrange[1] - ymargin),
        min(1, yrange[2] + ymargin)
      )
      
    } else {
      
      ## KL, CH and DB indices are non-negative
      ylim2 <- c(
        0,
        yrange[2] + ymargin
      )
    }
    
    ## Determine the horizontal position of the Best K label
    if (k[best.id] == max(k, na.rm = TRUE)) {
      
      ## Put the label to the left of the point
      label.hjust <- 1.1
      
    } else if (k[best.id] == min(k, na.rm = TRUE)) {
      
      ## Put the label to the right of the point
      label.hjust <- -0.1
      
    } else {
      
      ## Centre the label horizontally
      label.hjust <- 0.5
    }
    
    
    plot.data <- data.frame(
      k = k,
      y = y
    )
    
    best.data <- data.frame(
      k = k[best.id],
      y = y[best.id],
      label = paste0(
        "Best K = ",
        k[best.id]
      )
    )
    
    offset <- 0.10 * diff(ylim2)
    best.data$y.text <- best.data$y + offset
    
    plot.list[[cc]] <-
      ggplot2::ggplot(
        data = plot.data,
        mapping = ggplot2::aes(
          x = base::get("k"),
          y = base::get("y")
        )
      ) +
      ggplot2::geom_line(
        linewidth = 0.5
      ) +
      ggplot2::geom_point(
        size = 2.2
      ) +
      ggplot2::geom_point(
        data = best.data,
        mapping = ggplot2::aes(
          x = base::get("k"),
          y = base::get("y")
        ),
        inherit.aes = FALSE,
        size = 4
      ) +
      ggplot2::geom_text(
        data = best.data,
        mapping = ggplot2::aes(
          x = base::get("k"),
          y = base::get("y.text"),
          label = base::get("label")
        ),
        inherit.aes = FALSE,
        hjust = label.hjust,
        size = 3.3
      ) +
      ggplot2::labs(
        x = "Number of clusters",
        y = index.name,
        title = names(ind.list)[cc]
      ) +
      ggplot2::scale_x_continuous(
        breaks = k
      ) +
      ggplot2::coord_cartesian(
        xlim = xlim2,
        ylim = ylim2,
        clip = "off"
      ) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(
          hjust = 0.5
        ),
        plot.margin = ggplot2::margin(
          t = 8,
          r = 12,
          b = 8,
          l = 8,
          unit = "pt"
        )
      )
  }
  
  ## Remove the skipped classes from the list of plots
  plot.list <- plot.list[!skipped]
  
  if (any(skipped)) {
    message(
      "The index was not calculated for the following classes, ",
      "so they are not plotted: ",
      paste(sub(" class$", "", names(ind.list)[skipped]), collapse = ", ")
    )
  }
  
  if (length(plot.list) == 0) {
    return(invisible(plot.list))
  }
  
  if (onebyone) {
    
    ## Display one plot at a time
    for (cc in seq_along(plot.list)) {
      
      if (
        cc > 1 &&
        interactive()
      ) {
        readline(
          "Press <Enter> to display the next plot: "
        )
      }
      
      ## Start a new grid page on the same device
      grid::grid.newpage()
      
      grid::grid.draw(
        ggplot2::ggplotGrob(
          plot.list[[cc]]
        )
      )
    }
    
  } else {
    
    ## Divide the plots into groups of four
    page.number <- ceiling(
      seq_along(plot.list) / 4
    )
    
    page.list <- split(
      plot.list,
      page.number
    )
    
    ## Display each group in a 2-by-2 arrangement
    for (page.id in seq_along(page.list)) {
      
      if (
        page.id > 1 &&
        interactive()
      ) {
        readline(
          "Press <Enter> to display the next page: "
        )
      }
      
      ## Start a new grid page on the same graphics device
      grid::grid.newpage()
      
      gridExtra::grid.arrange(
        grobs = page.list[[page.id]],
        nrow = 2,
        ncol = 2,
        newpage = FALSE
      )
    }
  }
  
  invisible(plot.list)
}