#' cleans strings for use as a file name -> for saving charts with names based on chart titles
#' @details replaces spaces, special chars and and end-of-line charactgers by underscore characters
#' @export
clSpecialChars <- function(x, from = "", to = "ASCII//TRANSLIT") {
  # replace spaces and special chars
  tmpstr <- gsub(pattern = "[[:space:][:punct:]]+", 
                 replacement = "_", 
                 x)
  # drop underscore at the end of the string
  tmpstr <- gsub(pattern = "[_]$", 
                 replacement = "", 
                 tmpstr)
  
  # clean diacritics etc...
  tmpstr <- iconv(tmpstr, from = from, to = to)
  return(tmpstr)
}

# --------------- clDiacr -------------------
#' convert special letters to some normal equivalent characters
#' @export
clDiacr <- function(x, from = "", to = "ASCII//TRANSLIT") {
  #   convChars = list(    'Ĺ '='S', 'Ĺˇ'='s', 'Ĺ˝'='Z', 'Ĺľ'='z', 'Ă€'='A', 'Ă'='A', 'Ă‚'='A', 'Ă'='A', 'Ă„'='A', 'Ă…'='A', 'Ă†'='A', 'Ă‡'='C', 'Ă'='E', 'Ă‰'='E',
  #                             'ĂŠ'='E', 'Ă‹'='E', 'ĂŚ'='I', 'ĂŤ'='I', 'ĂŽ'='I', 'ĂŹ'='I', 'Ă‘'='N', 'Ă’'='O', 'Ă“'='O', 'Ă”'='O', 'Ă•'='O', 'Ă–'='O', 'Ă'='O', 'Ă™'='U',
  #                             'Ăš'='U', 'Ă›'='U', 'Ăś'='U', 'Ăť'='Y', 'Ăž'='B', 'Ăź'='Ss', 'Ă '='a', 'Ăˇ'='a', 'Ă˘'='a', 'ĂŁ'='a', 'Ă¤'='a', 'ĂĄ'='a', 'Ă¦'='a', 'Ă§'='c',
  #                             'Ă¨'='e', 'Ă©'='e', 'ĂŞ'='e', 'Ă«'='e', 'Ă¬'='i', 'Ă�'='i', 'Ă®'='i', 'ĂŻ'='i', 'Ă°'='o', 'Ă±'='n', 'Ă˛'='o', 'Ăł'='o', 'Ă´'='o', 'Ăµ'='o',
  #                             'Ă¶'='o', 'Ă¸'='o', 'Ăą'='u', 'Ăş'='u', 'ĹŻ'='u' 'Ă»'='u', 'Ă˝'='y', 'Ă˝'='y', 'Ăľ'='b', 'Ăż'='y' )
  #   tmpstr <- chartr(paste(names(unwanted_array), collapse=''),
  #          paste(unwanted_array, collapse=''),
  #          tmpstr)
  tmpstr <- iconv(x, from = from, to = "ASCII//TRANSLIT")
  return(tmpstr)
}

#' recode vector of strings using a convertion table
#' @param \code{input} input vector (usually strings)
#' @param \code{conversionTable} data frame of at least two (from and to) columns
#' @param \code{from,to} indeces of from/to columns
#' @param \code{replaceNA} if TRUE all values that couldn't be matched will remain the origianl values, otherwise left as NAs
#' @details ...
#' @export
recodeStrings <- function(input, conversionTable, from = 1, to = 2, replaceNA = T) {
  result <- conversionTable[, to][match(input, conversionTable[, from])]
  if (replaceNA) {
    result[is.na(result)] <- input[is.na(result)]
  }
  return(result)
}

#' shorten strings by cutting characters from the middle 
#' @param x character object to be shortened
#' @param max maximum number of letters of the resulting characters
#' @export
shortenString <- function(x, max) {
  where <- nchar(x) > max
  x[where] <- paste(substr(x[where], 
                           start = 1, 
                           stop = floor(max/2) - 1),
                    "..",
                    substr(x[where], 
                           start = nchar(x[where]) - (ceiling(max/2) - 2), 
                           stop = nchar(x[where])),
                    sep = "")
  
  return(x)
}

#' shorten strings by cutting characters from the middle - alias for shortenString()
#' @export
str_shrt <- shortenString


#' Merge two datasets based on both exact and approximate keys
#' @description \code{amerge} merges two datasets by exact keys and filter out rows that do not approximately match by given string keys.
#' @param x A data.frame or data.table.
#' @param aby name of a string key for approximate matching
#' @param y A data.frame or data.table.
#' @param method Method for distance calculation.
#' @param tol tolerance of dissimilarity between string keys in matched datasets
#' @param p see \code{stringdist} package
#' @param ... other parameters passed to \code{\link{merge}}
#' @seealso \code{\link{merge}}, \code{\link{stringdist}} and \code{\link{stringsim}}
#' @examples 
#' 
#' dt1 <- data.table(id   = c(1,      2,             3,      4,                  5),
#'                   name = c("Aple", "Google inc.", "ABB",  "Sun",              "Sony"),
#'                   value = rnorm(5))
#' 
#' dt2 <- data.table(id   = c(1,       2,             3,     4,                  5),
#'                   name = c("Apple", "Google",     "BBC",  "Sun Microsystems", "Sony Corporation"),
#'                   value = rnorm(5))
#' 
#' amerge(dt1, dt2, by = "id", aby = "name", tol = 0.3)
#' @export
amerge <- function(x, y, 
                   aby, 
                   method = "jw", 
                   tol = 0.1, 
                   p = 0.1, 
                   ...) {
  lim <- 1-tol
  
  if (!("package:stringdist" %in% search())) {
    tryCatch(require(stringdist), error = function(x) {warning(x); cat("Cannot load stringdist package \n")})
    on.exit(detach("package:stringdist", unload=TRUE))
  }
  
  dt <- merge(x = x, 
              y = y,
              ...)
  
  similarity <- stringsim(a = x[[aby]],
                          b = y[[aby]],
                          method = method,
                          p = p)
  dt <- dt[similarity > lim]
  
  return(dt)
}


find_first_match <- function(xx, l, no.match) {
  for (i in 1:length(l)) {
    if (xx %in% l[[i]]) return(names(l)[i])
  }
  
  return(if (is.null(no.match)) xx else no.match)
}


find_first_match_r <- function(xx, l, no.match) {
  for (i in 1:length(l)) {
    if (any(str_detect(string = xx, pattern = l[[i]]))) return(names(l)[i])
  }
  
  return(if (is.null(no.match)) xx else no.match)
}


#' Collapse vector into groups.
#' @description For each value in a vector \code{group_vec} finds first match in list of vectors and returns name of the vector.
#' @param x a character vector
#' @param l a (usually named) list of character vectors
#' @param no.match value that should be returned if no match is found. Usually NA or "other". If NULL (default) the original value itself is returned.
#' @param USE.NAMES logical passed to sapply indicating whether a result should be a named vector (for character input vectors only).
#' @param fixed logical; allows match based on regular expressions
#' @export
group_vec <- function(x, l, no.match = NULL, USE.NAMES = TRUE, fixed = TRUE) {
  # l has to be a named list
  if (!is.list(l)) stop("l has to be a list.")
  if (is.null(names(l))) {
    warning("l has has no names, order indeces will be used instead")
    names(l) <- 1:length(l)
  }
  
  if (fixed) {
    return(
      sapply(X   = x, 
             FUN = find_first_match, 
             l = l, no.match = no.match, USE.NAMES = USE.NAMES))
  } else {
    require(stringr)
    return(
      sapply(X   = x, 
             FUN = find_first_match_r, 
             l = l, no.match = no.match, USE.NAMES = USE.NAMES))
  }
}


#' Wrap long texts
#' @details 
#' A vectorized version of \code{strwrap()}
#' @export
wrap_text <- function(text, width) {
  result <- sapply(text, function(x) paste(strwrap(x, width = width), 
                                          collapse=" \n "))
  return(result)
}


#' Concatenation operators
#' @details   
#' \describe{
#'   \item{\%pp\%}{\code{a \%pp\% b} corresponds to \code{paste(a, b)}}
#'   \item{\%p\%}{\code{a \%p\% b} corresponds to \code{paste0(a, b)}}
#'   \item{\%pc\%}{\code{x \%pc\% c} corresponds to \code{paste(x, collapse = c)}}
#' }
#' @rdname concatenation
#' @export
`%pp%`<- function(a, b) {
  paste(a, b)
}

#' @rdname concatenation
#' @export
`%p%` <- function(a, b) {
  paste0(a, b)
}

#' @rdname concatenation
#' @export
`%pc%` <- function(x, c) {
  paste(x, collapse = c)
}
