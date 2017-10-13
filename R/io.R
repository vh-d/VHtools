excel_origin_date <- as.Date("1899-12-30")

#' read from clipboard for easy copy-paste from spreasdsheet (Excel)
#' \code{ctrlP} reads clipboard (copied from excel or other spreasheet) and returns a \code{data.frame}.
#' @export
ctrlP <- function(dec = ",", sep = "\t", rn = NULL, ...) {
  read.table(file = "clipboard", sep = sep, dec = dec, row.names = rn, ...)
}

#' writes last value to clipboard for easy copy-paste to excel
#' \code{ctrlC} writes last result to clipboard (to be paste to excel or other spreasheet).
#' @export
ctrlC <- function(data = .Last.value, dec = ",", sep = "\t", rn = F, ...) {
  write.table(x = data, file = "clipboard", sep = sep, dec = dec, row.names = rn, ...)
}

#' @export
#' @details For CtrlC decimal point character is set to "." while ctrlC uses ",".
#' @rdname ctrlC
CtrlC <- function(data = .Last.value, dec = ".", sep = "\t", rn = F, ...) {
  write.table(x = data, file = "clipboard", sep = sep, dec = dec, row.names = rn, ...)
}

#' read a series from (excel, Calc, ...) clipboard, draws its histogram and save results to clipbaord (for excel, Calc, ...)
#' @param \code{...} parameters tp be passed to \code{ctrlC} and \code{ctrlP} functions
#' @export
hist4Calc <- function(...) {
  tmp <- hist(ctrlP(...)[[1]])
  res <- data.frame(breaks = paste0("(", tmp$breaks[-length(tmp$breaks)], "–", tmp$breaks[-1], ">"), counts = tmp$counts)
  ctrlC(data = res, ...)
  return(res)
}

#' ask for user's input
#' @param \code{question} question to be prompted
#' @param \code{answers} vector of possible answers
#' @param \code{default} index of default answer 
#' @return valid answer, i.e. answer is in \code{answers}
#' @details 
#' In case \emph{default} is in range of possible answers, the default answer is uppercased and empty input (return key) is treated as the default answer.
#' @export
askUser <- function(question, answers = c("y", "n"), default = 1){
  
  answers <- tolower(answers)
  
  if (length(default) > 1) {
    simpleWarning("\'default\' parameter is a vector, only first is used")
    default <- default[1]
  }
  
  if (is.character(default)) {
    default <- which(match(tolower(default), answers))
  } else {
    default <- as.integer(default)
  } 
  
  answers_print <- answers
  
  if (default %in% 1:length(answers_print)) {
    answers_print[default] <- toupper(answers_print[default])
  }
  
  answer <- NULL
  while (is.null(answer) || !(tolower(answer) %in% answers)) {
    answer <- readline(prompt = paste0(question, " (", paste(answers_print, collapse = "/"), "): "))
    if (!is.null(default) && default %in% 1:length(answers) && answer == "") {
      answer <- answers[default]
    }
  }
  return(answer)
}

#' Save file after asking the user for confirmation
#' @param data data file to be writen
#' @param filename name of the file, defaults to name of the object
#' @param filepath path where to save the file, defaults to "../output/"
#' @param csv write the data into a csv2 as well?
#' @export
askSave <- function(data, 
                    filename = substitute(data), 
                    filepath = "../output/", 
                    csv = 0,
                    row.names = F,
                    ...) {
  
  if (askUser(paste0("Save and overwrite ", substitute(data), " file(s) on disk?")) == "y") {
    
    # Rdx file
    cat("Writing", substitute(data), "to", paste0(filename, ".RData"), " ...")
    save(list = as.character(substitute(data)),
         file = paste0(filepath, filename, ".RData"),
         ...)
    cat(" OK.\n")
    
    # csv file
    if (csv) {
      cat("Writing", substitute(data), "to", paste0(filename, ".csv"), " ...")
      switch(csv, 
             `1` = write.csv(data, 
                             file = paste0(filepath, filename, ".csv"),
                             row.names = row.names),
             `2` = write.csv2(data, 
                              file = paste0(filepath, filename, ".csv"),
                              row.names = row.names)
      )
      cat(" OK.\n")
    }
  }
}


#' convert a file from one encoding to another
#' @title convert file encoding
#' @description convert a file from one encoding to another
#' @param from_file source file path
#' @param to_file destination file path
#' @param from_encoding source endoding, Default: 'windows-1250'
#' @param to_encoding target encoding, Default: 'UTF-8'
#' @return TRUE if succesfull
#' @details 
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname iconvFile
#' @export 
iconvFile <- function(from_file, 
                      to_file, 
                      from_encoding = "windows-1250", 
                      to_encoding = "UTF-8"){

  writeLines(
    iconv(
      readLines("from_file", 
                encoding = from_encoding), 
      from = from_encoding, 
      to   = to_encoding), 
    file(to_file, open = "w"))
  
  return(invisible(TRUE))
}
