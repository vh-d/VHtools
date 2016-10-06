#' alias for as.Date function for converting dates from dd.mm.yyyy format
#' @param x A string vector containing date encoded as dd.mm.yyyy
#' @export
as.Date2 <- function(x, ...) {
  as.Date(x, "%d.%m.%Y", ...)
}