#' cleans strings for use as a file name -> for saving charts with names based on chart titles
#' @details replaces spaces, special chars and and end-of-line charactgers by underscore characters
#' @export
clSpecialChars <- function(x) {
  # replace spaces and special chars
  tmpstr <- gsub(pattern = "[[:space:][:punct:]]+", 
                 replacement = "_", 
                 x)
  # drop underscore at the end of the string
  tmpstr <- gsub(pattern = "[_]$", 
                 replacement = "", 
                 tmpstr)
  
  # clean diacritics etc...
  tmpstr <- iconv(tmpstr, to = "ASCII//TRANSLIT")
  return(tmpstr)
}

# --------------- clDiacr -------------------
#' convert special letters to some normal equivalent characters
#' @export
clDiacr <- function(x) {
  #   convChars = list(    'Ĺ '='S', 'Ĺˇ'='s', 'Ĺ˝'='Z', 'Ĺľ'='z', 'Ă€'='A', 'Ă'='A', 'Ă‚'='A', 'Ă'='A', 'Ă„'='A', 'Ă…'='A', 'Ă†'='A', 'Ă‡'='C', 'Ă'='E', 'Ă‰'='E',
  #                             'ĂŠ'='E', 'Ă‹'='E', 'ĂŚ'='I', 'ĂŤ'='I', 'ĂŽ'='I', 'ĂŹ'='I', 'Ă‘'='N', 'Ă’'='O', 'Ă“'='O', 'Ă”'='O', 'Ă•'='O', 'Ă–'='O', 'Ă'='O', 'Ă™'='U',
  #                             'Ăš'='U', 'Ă›'='U', 'Ăś'='U', 'Ăť'='Y', 'Ăž'='B', 'Ăź'='Ss', 'Ă '='a', 'Ăˇ'='a', 'Ă˘'='a', 'ĂŁ'='a', 'Ă¤'='a', 'ĂĄ'='a', 'Ă¦'='a', 'Ă§'='c',
  #                             'Ă¨'='e', 'Ă©'='e', 'ĂŞ'='e', 'Ă«'='e', 'Ă¬'='i', 'Ă�'='i', 'Ă®'='i', 'ĂŻ'='i', 'Ă°'='o', 'Ă±'='n', 'Ă˛'='o', 'Ăł'='o', 'Ă´'='o', 'Ăµ'='o',
  #                             'Ă¶'='o', 'Ă¸'='o', 'Ăą'='u', 'Ăş'='u', 'ĹŻ'='u' 'Ă»'='u', 'Ă˝'='y', 'Ă˝'='y', 'Ăľ'='b', 'Ăż'='y' )
  #   tmpstr <- chartr(paste(names(unwanted_array), collapse=''),
  #          paste(unwanted_array, collapse=''),
  #          tmpstr)
  tmpstr <- iconv(x, to = "ASCII//TRANSLIT")
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
  result <- conversionTable[[to]][match(input, conversionTable[[from]])]
  if (replaceNA) {
    result[is.na(result)] <- input[is.na(result)]
  }
  return(result)
}

