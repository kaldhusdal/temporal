#' @name Rolling aggregate functions
#' @rdname rolling
#' 
#' @title Rolling aggregate functions
#' 
#' @description Functions for calculating different rolling aggregates.
#' 
#' @param x An atomic vector of class numeric.
#' @param w An integer value specifying the width of the windows over which to calculate the rolling aggregate.
#' @param type Either one of "lag" (window ends with current observation), "mid" (current observation is in the middle of the window) or "lead" (window starts with current observation).
#' @param na.rm A logical value indicating whether \code{NA} values should be ignored when calculating the rolling aggregates.
#'
#' @details 
#' \itemize{
#' \item If \code{na.rm} is set to \code{TRUE} and there are no valid values inside a window, \code{rolling.sum} will return the value 0.
#' \item \code{rolling.var} returns the unbiased sample estimator \eqn{Var(x) = \frac{1}{n-1} \sum_i^n (x_i - \bar{x})^2}{Var(x) = (1 / (n - 1)) * sum_i (x_i - mu)^2}.
#' \item \code{rolling.sd} simply calls \code{rolling.var} and returnes the square root of the rolling variances.
#' \item The skewness measures the direction and degree of a distrubution's asymmetry.  A symmetric distribution has a skewness of zero, while a "left-skewed" (or "left-tailed") distribution (where the arithmetic mean is less than the median) has a negative skewness.  The Fisher-Pearson standardised moment coefficient, as discussed by Joanes and Gill (1998) is used to calculate the rolling skewness:
#' \deqn{\frac{\sqrt{n(n-1)}}{n(n-2)} \left( \frac{\sum_i^n (x_i-\bar{x})^3}{\left( \frac{1}{n} \sum_i^n (x_i-\bar{x})^2 \right)^{3/2}} \right)}{(sqrt(n(n - 1))/(n(n - 2)) ((sum_i (x_i - mu)^3)/((1 / n) sum_i (x_i - mu)^2)^(3 / 2))}
#' \item The kurtosis describes the "tailedness" of a distribution.  Using \eqn{s2 = \sum_i^n (x_i-\bar{x})^2}{s2 = sum_i (x_i - mu)^2}, \eqn{s4 = \sum_i^n (x_i-\bar{x})^4}{s4 = sum_i (x_i - mu)^4} and \eqn{Var(x) = \frac{s2}{n-1}}{Var(x) = s2 / (n - 1)}, the kurtosis is defined as follows:
#' \deqn{kurtosis = \frac{n(n-1)}{(n-1)(n-2)(n-3)} \left( \frac{s4}{Var(x)^2} \right) -3 \frac{(n-1)^2}{(n-2)(n-3)}.}{kurtosis = (n(n - 1) / ((n - 1)(n - 2)(n - 3))) (s4 / Var(x)^2) - 3(((n - 1)^2 / ((n - 2)(n - 3)))).}
#' The formula is can be found in Sheskin (2000) and yields an expected kurtosis of zero for a Gaussian distribution.
#'  }
#'
#' @references
#' Sheskin, D.J. (2000) Handbook of Parametric and Nonparametric Statistical Procedures, Second Edition. Boca Raton, Florida: Chapman & Hall/CRC.
#'
#' Joanes, D. N. and Gill, C. A. (1998), Comparing measures of sample skewness and kurtosis. Journal of the Royal Statistical Society: Series D (The Statistician), 47: 183-189. doi:10.1111/1467-9884.00122
#'
#' @examples
#' x <- rnorm(100)
#' rolling.sum(x = x, w = 10)
#'
#' @useDynLib temporal
#' @importFrom methods getPackageName
#' @export
rolling.sum <- function (x, w, type = "lag", na.rm = FALSE) {
  check_data_is_atomicvector_and_integer(x)
  n <- length(x)
  check_windowsize_is_scalar_and_integer(w)
  check_windowsize_greater_equal_data(w, n)
  check_windowsize_is_whole_number(w)
  w <- as.integer(w)
  check_type(type)
  check_windowsize_for_type_mid(w, type)
  type <- switch(type, "lag" = 1, "mid" = 2, "lead" = 3)
  if (w < 2) {
    if (w == 0) {
      stop("Windows size specified in argument 'w' must be strictly positive.")
    } else {
      if (w == 1) return(x)
    }
  }
  check_narm_is_logical_and_scalar(na.rm)
  storage.mode(x) <- "double"
  storage.mode(type) <- "integer"
  out <- .Fortran("rolling_sum", x, n, w, type, na.rm, out = rep(as.numeric(NA), n), PACKAGE = getPackageName(), NAOK = TRUE)
  return(out$out)
}

# rolling.mean(x = x, w = 10)
# rolling.var(x = x, w = 10)
# rolling.sd(x = x, w = 10)
# rolling.skewness(x = x, w = 10)
# rolling.kurtosis(x = x, w = 10)


##' @rdname rolling
##' @export
#rolling.mean <- function (x, w, na.rm = FALSE) {
#  if (!is.vector(x) || !is.numeric(x)) stop("Argument 'x' must be an atomic vector of class numeric or integer.")
#  n <- length(x)
#  if (length(w) != 1 || !is.numeric(w)) stop("Argument 'w' must be an integer value of length one.")
#  if (!is.integer(w) && has.decimals(w)) warning("Argument 'w' was converted to class integer.")
#  w <- as.integer(w)
#  if (w < 2) {
#    if (w == 0) {
#      stop("Windows size specified in argument 'w' must be strictly positive.")
#    } else {
#      if (w == 1) return(x)
#    }
#  }
#  if (length(na.rm) != 1 || !is.logical(na.rm)) stop("Argument 'na.rm' must be a logical value of length one.")
#  storage.mode(x) <- "double"
#  out <- .Fortran("rolling_mean", x, n, w, na.rm, out = rep(as.numeric(NA), n), PACKAGE = "misc", NAOK = TRUE)
#  return(out$out)
#}

##' @rdname rolling
##' @export
#rolling.var <- function (x, w, na.rm = FALSE) {
#  if (!is.vector(x) || !is.numeric(x)) stop("Argument 'x' must be an atomic vector of class numeric or integer.")
#  n <- length(x)
#  if (length(w) != 1 || !is.numeric(w)) stop("Argument 'w' must be an integer value of length one.")
#  if (!is.integer(w) && has.decimals(w)) warning("Argument 'w' was converted to class integer.")
#  w <- as.integer(w)
#  if (w < 2) {
#    if (w == 0) {
#      stop("Windows size specified in argument 'w' must be strictly positive.")
#    } else {
#      if (w == 1) return(x)
#    }
#  }
#  if (length(na.rm) != 1 || !is.logical(na.rm)) stop("Argument 'na.rm' must be a logical value of length one.")
#  storage.mode(x) <- "double"
#  out <- .Fortran("rolling_var", x, n, w, na.rm, out = rep(as.numeric(NA), n), PACKAGE = "misc", NAOK = TRUE)
#  return(out$out)
#}

##' @rdname rolling
##' @export
#rolling.sd <- function (x, w, na.rm = FALSE) {
#  sqrt(rolling.var(x, w, na.rm))
#}

##' @rdname rolling
##' @export
#rolling.skewness <- function (x, w, na.rm = FALSE) {
#  if (!is.vector(x) || !is.numeric(x)) stop("Argument 'x' must be an atomic vector of class numeric or integer.")
#  n <- length(x)
#  if (length(w) != 1 || !is.numeric(w)) stop("Argument 'w' must be an integer value of length one.")
#  if (!is.integer(w) && has.decimals(w)) warning("Argument 'w' was converted to class integer.")
#  w <- as.integer(w)
#  if (w < 2) {
#    if (w == 0) {
#      stop("Windows size specified in argument 'w' must be strictly positive.")
#    } else {
#      if (w == 1) return(x)
#    }
#  }
#  if (length(na.rm) != 1 || !is.logical(na.rm)) stop("Argument 'na.rm' must be a logical value of length one.")
#  storage.mode(x) <- "double"
#  out <- .Fortran("rolling_skewness", x, n, w, na.rm, out = rep(as.numeric(NA), n), PACKAGE = "misc", NAOK = TRUE)
#  return(out$out)
#}

##' @rdname rolling
##' @export
#rolling.kurtosis <- function (x, w, na.rm = FALSE) {
#  if (!is.vector(x) || !is.numeric(x)) stop("Argument 'x' must be an atomic vector of class numeric or integer.")
#  n <- length(x)
#  if (length(w) != 1 || !is.numeric(w)) stop("Argument 'w' must be an integer value of length one.")
#  if (!is.integer(w) && has.decimals(w)) warning("Argument 'w' was converted to class integer.")
#  w <- as.integer(w)
#  if (w < 2) {
#    if (w == 0) {
#      stop("Windows size specified in argument 'w' must be strictly positive.")
#    } else {
#      if (w == 1) return(x)
#    }
#  }
#  if (length(na.rm) != 1 || !is.logical(na.rm)) stop("Argument 'na.rm' must be a logical value of length one.")
#  storage.mode(x) <- "double"
#  out <- .Fortran("rolling_kurtosis", x, n, w, na.rm, out = rep(as.numeric(NA), n), PACKAGE = "misc", NAOK = TRUE)
#  return(out$out)
#}










##check_args <- function (restrictions, args) {

##}


##make_restrictions <- function () {
##  list("x" = list(type = c("numeric"), values = )
##values:
##  1:5
##  5
##  1::5
##}



##check_logical <- function (x) is.logical(x)
##check_numeric <- function (x) is.numeric(x)
##check_wholenumber <- function (x) !(x %% 1)
##check_scalar <- function (x) length(x) == 1
