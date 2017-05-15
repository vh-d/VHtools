## --------------- mergeAll -------------------
#' merges multiple data.frames or data.tables at once
#' 
#' \code{mergeAll} merges multiple data.frames with the same id vars
#' @param datasets is a list of data.frames or data.tables...
#' @return a single data.frame/data.table
#' @export
mergeAll <- function(datasets, all = T, ...) {
  if (length(datasets) == 2) {
    return(merge(datasets[[1]], datasets[[2]], all = all, ...))
  } else {
    return(merge(datasets[[1]], mergeAll(datasets[-1]), all = all, ...))
  }
}

# --------------- omitNALines -------------------
#' deletes all-NAs rows and columns (deprecated, see dropAllNA)
#' @export
omitNALines <- function(data, mar = c(1, 2), id = NULL) {
  isna <- is.na(data)
  datadim <- dim(data)
  return(data[rowSums(isna) < datadim[2], colSums(isna) < datadim[1]])
}


# --------------- dropAllNA -------------------
#' Takes an array and return a copy without cols/rows that are NAs-only
#' @param \code{data} matrix or data.frame
#' @param \code{mar} margin (1 for rows, 2 for columns)
#' @param \code{id} a character vecor with column names that are id/keys and should by skipped when testing for missingness
#' @return an array of the same type as the input \code{data} object.
#' @examples tmp <- data.frame(a = 1:5, b = c(34, 45, 56, NA, NA), c = c(2, 3, 4, 5, NA))
#' deleteAllNA(tmp)
#' deleteAllNA(tmp, id = "a")
#' deleteAllNA(tmp, id = c("a", "b"))
#' @export
dropAllNA <- function(data, mar = 1, id = NULL) {
  # todo: validate data class and dimensions
  
  # skip ID columns
  id_index <- dropNA(match(id, colnames(data)))
  
  # find all NAs
  if (length(id_index)) {
    isna <- is.na(data[,-id_index, drop = F])
  } else {
    isna <- is.na(data)
  }
  
  # find rows that are not full of NAs
  datadim <- dim(isna)
  
  rows_to_keep <- T
  cols_to_keep <- T
  
  if (1 %in% mar) rows_to_keep <- rowSums(isna) < datadim[2]
  if (2 %in% mar) cols_to_keep <- colSums(isna) < datadim[1]
  
  return(data[ rows_to_keep, cols_to_keep ])
}



# --------------- allCombApply -------------------
#' apply a function to all combination of columns
#' 
#' \code{allCombApply} applies a function to all combination of columns
#' @details \code{FUN} should have two (vector) parameters such as \code{function(v1, v2) {cor(v1, v2)}}
#' @return data.frame (or vector) with (n*(n-1)/2) columns (or elements)
#' @examples
#' allCombApply(matrix(c(1, 2, 3, 4, 5, 6, 7, 8), nrow = 2), function(v1, v2) {v1-v2})
#' @export
allCombApply <- function(inmat, FUN, ...) {
  #   if (!any(class(inmat) %in% c("matrix", "data.frame"))) stop(paste0("Not a valid class for allCombApply: ", class(inmat)))
  tempFUN <- function(x) {
    a <- subset(inmat, select = x[1])
    b <- subset(inmat, select = x[2])
    cc <- complete.cases(a, b)
    return(FUN( a[cc], b[cc], ...))
  }
  
  combinate <- combn(1:dim(inmat)[2], 2)
  #   inmat <- inmat[ complete.cases(inmat), ]
  return(apply(combinate, 2, FUN = tempFUN))
}

# --------------- allCombApplyM -------------------
#' applies given function to all combinations of matrix columns
#' 
#' @details \code{FUN} should have two (vector) parameters such as \code{function(v1, v2) {cor(v1, v2)}}
#' @return symmetric matrix
#' @examples
#' allCombApplyM(matrix(rnorm(25), nrow = 5), FUN = cor)
#' @export
allCombApplyM <- function(inmat, FUN, ...) {
  n = seq_len(ncol(inmat))
  ff = function(a, b) {
    cc <- complete.cases(inmat[,a], inmat[,b])
    if (sum(cc) >= 2) {
      return(FUN(inmat[cc,a], inmat[cc, b], ...))
    } else {
      return(NA)
    }
  }
  outcome <- outer(n, n, Vectorize(ff))
  rownames(outcome) <- colnames(inmat)
  colnames(outcome) <- colnames(inmat)
  return(outcome)
}

#' @export
cleanUp <- function(pattern = "temp", ign_case = T, env = ".GlobalEnv") {
  remove(list = grep(pattern, objects(envir = as.environment(env)), ignore.case = ign_case, value = T), envir = as.environment(env))
}

#' return a copy of a vector omitting NA values
#' @param \code{vect} a vector of any type.
#' @return a vector of the same type as \code{vect}.
#' 
#' @export
dropNA <- function(vect){
  if (is.vector(vect)) {
    return(vect[!is.na(vect)])
  } else {
    stop(paste(as.list(match.call())["vect"], "is not a vector!"))
  }
}

# VECTOR ARITHMETIC OPERATIONS WITHOUT NAs ------------------------------------------

#' Addition of two numbers/vectors/matrices while replacing all NAs by zeros.
#' @rdname NAoperators
#' @examples c(1, 2, 4) %+n% c(3, 6, NA)
#' @export
'%+n%' <- function(a, b) {
  a[is.na(a)] <- 0
  b[is.na(b)] <- 0
  return(a + b)
}

#' Deduct two numbers/vectors/matrices while replacing all NAs by zeros.
#' @rdname NAoperators
#' @details the two objects of appropriate class types.
#' @examples c(1, 2, 4) %-n% c(3, 6, NA)
#' @export
'%-n%' <- function(a, b) {
  a[is.na(a)] <- 0
  b[is.na(b)] <- 0
  return(a - b)
}

#' Product of two numbers/vectors/matrices while replacing all NAs by zeros.
#' @rdname NAoperators
#' @examples c(1, 2, NA) %*n% 3
#' @export
'%*n%' <- function(a, b) {
  a[is.na(a)] <- 0
  b[is.na(b)] <- 0
  return(a * b)
}

#' Division of two numbers/vectors/matrices while replacing all NAs by zeros.
#' @rdname NAoperators
#' @examples c(3, 6, NA) %/n% 3
#' @export
'%/n%' <- function(a, b) {
  a[is.na(a)] <- 0
  b[is.na(b)] <- 0
  return(a / b)
}

#' seq function on vectors
#' @param \code{from,to} the starting and (maximal) end values vectors of the sequences.
#' @param \code{simplify} logical: whether the function returns a matrix/list (depending on reqularity of \code{from-to}) (\code{TRUE}) a vector (\code{FALSE}) .
#' @details If the \code{from - to} vector is not constant vector and \code{simplify = F}, \code{seqv} returns a list.
#' @export
seqv <- function(from, to, ..., simplify = F) {
  sequences <- apply(cbind(from, to), 1, function(x) seq(x[1], x[2], ...))
  if (simplify) {
    return(as.vector(sequences))
  } else {
    return(sequences)
  }
}

#' rescale numeric objects
#' @param \code{v} vector/matrix to be rescaled
#' @return \code{normalize} returns vector/matrix rescaled to the \code{[0;1]} interval
#' @export
normalize <- function(v, ...) (v - min(v, ...))/diff(range(v, ...))

#' @rdname normalize
#' @param \code{low} minimum of the resulting interval
#' @param \code{high} maximum of the resulting interval
#' @param \code{...} parameters passed to \code{min} and \code{max} (or \code{range}) methods
#' @return \code{rescale} returns vector/matrix rescaled to the \code{[low;high]} interval
#' @export
rescale <- function(x, low = 0.0, high = 1.0, ...) {
  x <- as.numeric(x)
  
  minx = min(x, ...)
  maxx = max(x, ...)
  
  return(low+(high-low)*(x - minx)/(maxx - minx))
}


# APPLY FUNCTIONS ---------------------------------------------------------

#' convert a vector to a specified type
#'
#' @param \code{v} input vector
#' @param \code{classtype} "character", "numeric", "logical", "integer" or "double".
#'
#' @return vector of the specified type
#' @export
convertClass <- function(v, classtype){
  # switch(classtype,
  #        character = as.character(v),
  #        numeric = as.numeric(v),
  #        logical = as.logical(v),
  #        integer = as.integer(v),
  #        double = as.double(v))  
  return(as(v, classtype))
}

#' convert columns in a data.frame to a specified types
#'
#' @param \code{df} input data.frame that should be converted
#' @param \code{classtype} vector of target types : "character", "numeric", "logical", "integer" or "double".
#'
#' @return data.frame
#' @export
convertClasses <- function(df, classes) {
  if (!is.character(classes) || !is.data.frame(df) || dim(df)[2] != length(classes)) stop("Classes has to be a character vector!")
  
  names(classes) <- colnames(df)
  
  # dfnew <- as.data.frame(lapply(X = colnames(df), 
  #                               FUN = function(x) convertClass(v = df[[x]], 
  #                                                              classtype = classes[x])))
  dfnew <- as.data.frame(lapply(X = colnames(df), 
                                FUN = function(x) as(df[[x]], classes[x])))
  # keep the same column names
  colnames(dfnew) <- colnames(df)
  
  return(dfnew)
}

#' fill missing values by constant
#' @param \code{x} input vector
#' @param \code{by_val} substitute value 
#' @export
fillNAs <- function(x, by_val = 0) {
  x[is.na(x)] <- by_val
  return(x)
}

#' split vector into batches 
#' @param x vector
#' @param size (maximum) size of a single batch
split2Batches <- function(x, size) split(x, ceiling(seq_along(x)/size))

#' alternative version of diff() returning constant-length vector
#'
#' Wrapper to diff() function that fill in values at the beginning of the resulting vector
#'
#' @param x numeric/integer vector (see help for diff())
#' @param x lag (see help for diff())
#' @param x differences (see help for diff())
#' @param x fill value to fill in missing values
#'
#' @return vector of the same length and as \code{x}
#' @examples
#' str(diff(1:10))
#' str(diff_fill(1:10))
#' str(diff_fill(1:10, fill = NULL))
#' @export
diff_fill <- function(x, lag = 1, differences = 1, fill = NA) {
  c(rep(fill, lag*differences), diff(x, lag = lag, differences = differences))
}

#' differences of periodic cumsums
#'
#' Compute diffences of values that are cumulative sums restarting from 0 every period such as accouting values.
#'
#' @param x numeric/integer vector
#' @param t time index
#' @param tbase value of time index where cumulative sums starts from 0
#' @param fill (see help for diff_fill())
#'
#' @return return
#' @examples
#' ts <- c(cumsum(rnorm(12, 3)), cumsum(rnorm(12, 5)))
#' plot(ts, type = "l")
#' plot(diff(ts), type = "l")
#' 
#' tsd <- diff_basis(ts, rep(1:12, times = 2))
#' plot(tsd, type = "l")
#' 
#' @export
diff_basis <- function(x, t, tbase = 1, fill = NA) {
  result <- diff_fill(x, fill = fill)
  result[t == tbase] <- x[t == tbase]
  
  return(result)
}

#' Apply function to rolling/moving windows
#' @export
winply <- function(x, fun, win, ...) {
  if (length(x) <= win) return(as.numeric(NA))
  res <- rep(NA, length(x))
  for (i in win:length(x)) {
    res[i] <- do.call(fun, args = list(x[(i-win+1):i], ...))
  }
  
  res
}
winply <- cmpfun(winply)


#' Apply function to rolling/moving windows of changing width
#' @export
winply_v <- function(x, fun, win, fill = NA, ...) {
  if (length(win) == 1) {
    win <- rep(win, length(x))
  } else
    if (length(win) != length(x)) stop("Mismatch in lengths of x and window width!")
  
  res <- rep(NA, length(x))
  
  tmp <- seq_along(x) - win
  add <- -1*min(tmp[tmp < 0], 0)
  x2  <- c(rep(fill[1], add), x)
  
  for (i in 1:length(x)) {
    res[i] <- do.call(fun, args = list(x2[(i+add-win[i]+1):(i+add)], ...))
  }
  
  res
}
# require(compiler)
winply_v <- cmpfun(winply_v)


