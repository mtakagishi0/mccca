# Block-diagonal matrix (internal; replaces magic::adiag)
#
# Places the matrices given in ... on the diagonal of a larger matrix and fills
# the remaining entries with 0. For example, for an (2 x 3) matrix A and a
# (4 x 2) matrix B, block.diag(A, B) returns the (6 x 5) matrix
#   | A 0 |
#   | 0 B |
#
# @param ... Numeric or logical matrices (vectors, scalars and data frames
#   with numeric or logical columns are converted by as.matrix()).
# @return A numeric matrix whose numbers of rows and columns are the sums of
#   those of the input matrices. Row and column names are not kept. With no
#   input, a 0 x 0 matrix is returned.
# @noRd
block.diag <- function(...) {
  mats <- list(...)

  # check the input
  is_null <- vapply(mats, is.null, logical(1))
  if (any(is_null)) {
    stop("block.diag(): input ", paste(which(is_null), collapse = ", "),
         " is NULL.", call. = FALSE)
  }
  mats <- lapply(mats, as.matrix)
  is_num <- vapply(mats, function(m) is.numeric(m) || is.logical(m),
                   logical(1))
  if (!all(is_num)) {
    stop("block.diag(): input ", paste(which(!is_num), collapse = ", "),
         " is not a numeric or logical matrix.", call. = FALSE)
  }

  nr <- vapply(mats, nrow, integer(1))
  nc <- vapply(mats, ncol, integer(1))

  out <- matrix(0, nrow = sum(nr), ncol = sum(nc))
  r0 <- cumsum(c(0, nr))  # row offset of each block
  c0 <- cumsum(c(0, nc))  # column offset of each block

  for (i in seq_along(mats)) {
    if (nr[i] > 0 && nc[i] > 0) {
      out[r0[i] + seq_len(nr[i]), c0[i] + seq_len(nc[i])] <- mats[[i]]
    }
  }
  out
}
