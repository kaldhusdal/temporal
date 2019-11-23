#' @name Successions
#' @rdname successions
#' 
#' @title Count successions
#' 
#' @description Counts the lengths of successions of identical values in a vector.
#' 
#' @param x An atomic vector.
#'
#' @return A list containing the indices of the successions (\code{index}), the total number of successions (\code{successions}), the unique value of each succession (\code{value}) and the lengths of the successions (\code{length})
#'
#' @examples
#' set.seed(7)
#' x <- sample(LETTERS[1:3], 10, replace=TRUE, prob = c(0.2, 0.2, 0.6))
#' x
#' successions(x)
#'
#' @useDynLib temporal
#' @importFrom methods getPackageName
#' @export
successions <- function (x) {
  n <- length(x)
  xdouble <- if (typeof(x) == "double") 1 else 0
  xint <- if (typeof(x) == "integer") 1 else 0
  if (!inherits(x, "factor")) x <- factor(x, levels = unique(x))
  out <- .Fortran("successions",
                  x = as.integer(x),
                  n = n,
                  z = rep(as.integer(NA), n),
                  m = rep(as.integer(NA), n),
                  PACKAGE = getPackageName(),
                  NAOK = TRUE)
  s <- sum(!is.na(out$z))
  out <- list(index = 1:s,
              successions = s,
              value = levels(x)[out$z[1:s]],
              length = as.vector(out$m[1:s]))
  if (xdouble) out$value <- as.numeric(out$value)
  if (xint) out$value <- as.integer(out$value)
  return(out)
}
