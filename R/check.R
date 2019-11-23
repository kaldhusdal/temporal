check_data_is_atomicvector_and_integer <- function (x) {
  if (!is.vector(x) || !is.numeric(x)) stop("Argument 'x' must be an atomic vector of class numeric or integer.")
}

check_windowsize_is_scalar_and_integer <- function (w) {
  if (length(w) != 1 || !is.numeric(w)) stop("Argument 'w' must be an integer value of length one.")
}

check_windowsize_is_whole_number <- function (w) {
  if (!is.integer(w) && ((w - ceiling(w)) < 0)) warning("Argument 'w' was converted to class integer.")
}

check_windowsize_greater_equal_data <- function (w, n) {
  if (w > n) stop("Argument 'w' must be a value smaller than or equal to the lenght of the data provided in 'x'.")
}

check_windowsize_for_type_mid <- function (w, type) {
  if ((type == "mid") && !(w %% 2)) stop("Argument 'w' must be an odd number when setting the argument 'type' to 'mid'.")
}

check_type <- function (type) {
  if (!type %in% c("lag", "mid", "lead")) stop("Argument 'type' must be either one of 'lag', 'mid' or 'lead'.")
}

check_narm_is_logical_and_scalar <- function (na.rm) {
  if (length(na.rm) != 1 || !is.logical(na.rm)) stop("Argument 'na.rm' must be a logical value of length one.")
}
