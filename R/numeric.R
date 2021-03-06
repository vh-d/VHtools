#' @export
isPosDef <- function(M) { 
  if (!all.equal(M, t(M))) return(FALSE)
  if (all(eigen(M)$values > 0)) TRUE else FALSE
} 

#' @export
isSemiPosDef <- function(M) { 
  if (!all.equal(M, t(M))) return(FALSE)
  if (all(eigen(M)$values >= 0)) TRUE else FALSE
} 

#' @export
pequal <- function(x, y) {
  abs(x - y) < sqrt(.Machine$double.eps)
}

#' dirty alternative to round()
#' @export
round2 <- function(x, ...) {
  round(x + 100*.Machine$double.eps, ...)
}

