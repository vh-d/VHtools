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
