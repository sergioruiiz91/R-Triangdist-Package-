# a = min, b = max, c = mode

# First we are going to create the density function with a variables check
dtriang <- function(x, a, b, c) {
  if (a >= b || c < a || c > b) {
    stop("Parameters must satisfy a <= c <= b and a < b")
  }
  if (x < a || x > b) {
    return(0)
  } else if (x <= c) {
    return(2 * (x - a) / ((b - a) * (c - a)))
  } else {
    return(2 * (b - x) / ((b - a) * (b - c)))
  }
}

# Secondly, we are going to create the cumulative distribution function with a variables check
ptriang <- function(q, a, b, c) {
  if (a >= b || c < a || c > b) {
    stop("Parameters must satisfy a <= c <= b and a < b")
  }
  if (q < a) {
    return(0)
  } else if (q <= c) {
    return((q - a)^2 / ((b - a) * (c - a)))
  } else if (q < b) {
    return(1 - (b - q)^2 / ((b - a) * (b - c)))
  } else {
    return(1)
  }
}

# Thirdly, we are going to create the quantile function with a variables check
qtriang <- function(p, a, b, c) {
  if (a >= b || c < a || c > b) {
    stop("Parameters must satisfy a <= c <= b and a < b")
  }
  if (p < 0 || p > 1) {
    stop("p must be between 0 and 1")
  }
  threshold <- (c - a) / (b - a)
  if (p <= threshold) {
    return(a + sqrt(p * (b - a) * (c - a)))
  } else {
    return(b - sqrt((1 - p) * (b - a) * (b - c)))
  }
}

# Finally, we are going to create the random generation function with a variables check
rtriang <- function(n, a, b, c) {
  if (a >= b || c < a || c > b) {
    stop("Parameters must satisfy a <= c <= b and a < b")
  }
  qtriang(runif(n), a, b, c)
}
