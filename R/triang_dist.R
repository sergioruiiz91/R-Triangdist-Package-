# a = min, b = max, c = mode

validate_triangular_params <- function(min, max, mode) {
  if (any(is.na(min) | is.na(max) | is.na(mode))) {
    stop("Parameters min, max, and mode must not be NA")
  }
  if (any(min >= max | mode < min | mode > max)) {
    stop("Parameters must satisfy min <= mode <= max and min < max")
  }
}

#' Density function for the triangular distribution
#'
#' @param x Vector of quantiles
#' @param min Lower limit of the distribution
#' @param max Upper limit of the distribution
#' @param mode Mode of the distribution
#' @return Vector of densities
#' @export
#' @importFrom stats runif
dtriang <- function(x, min, max, mode) {
  validate_triangular_params(min, max, mode)
  ifelse(
    x < min | x > max, 0,
    ifelse(
      x <= mode,
      2 * (x - min) / ((max - min) * (mode - min)),
      2 * (max - x) / ((max - min) * (max - mode))
    )
  )
}

#' Cumulative distribution function for the triangular distribution
#'
#' @param q Vector of quantiles
#' @param min Lower limit of the distribution
#' @param max Upper limit of the distribution
#' @param mode Mode of the distribution
#' @return Vector of probabilities
#' @export
ptriang <- function(q, min, max, mode) {
  validate_triangular_params(min, max, mode)
  ifelse(
    q < min, 0,
    ifelse(
      q <= mode,
      (q - min)^2 / ((max - min) * (mode - min)),
      ifelse(
        q < max,
        1 - (max - q)^2 / ((max - min) * (max - mode)),
        1
      )
    )
  )
}

#' Quantile function for the triangular distribution
#'
#' @param p Vector of probabilities
#' @param min Lower limit of the distribution
#' @param max Upper limit of the distribution
#' @param mode Mode of the distribution
#' @return Vector of quantiles
#' @export
qtriang <- function(p, min, max, mode) {
  validate_triangular_params(min, max, mode)
  if (any(p < 0 | p > 1)) {
    stop("p must be between 0 and 1")
  }
  threshold <- (mode - min) / (max - min)
  ifelse(
    p <= threshold,
    min + sqrt(p * (max - min) * (mode - min)),
    max - sqrt((1 - p) * (max - min) * (max - mode))
  )
}

#' Random generation for the triangular distribution
#'
#' @param n Number of observations
#' @param min Lower limit of the distribution
#' @param max Upper limit of the distribution
#' @param mode Mode of the distribution
#' @return Vector of random values
#' @export
rtriang <- function(n, min, max, mode) {
  validate_triangular_params(min, max, mode)
  qtriang(runif(n), min, max, mode)
}
