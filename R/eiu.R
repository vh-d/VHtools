# __ EIU ------------------------------------------------------------------

#' read xlsx file as downloaded from EIU and return panel data in the form of (country + year ~ variable)
#' @param \code{file} file name (path).
#' @param \code{dfRenameSeries} optional. 
#' @details depends on data.table and xlsx packages
#' @export
readEIU <- function(file, dfRenameSeries = NULL, ...) {
  
  # attach/detach xlsx
  if (!("package:xlsx" %in% search())) {
    if (!requireNamespace("xlsx")) stop("Cannot load xlsx package required for reading from .xlsx files \n")
    on.exit({detach("package:xlsx", unload=TRUE);
      detach("package:xlsxjars", unload=TRUE);
      detach("package:rJava", unload=TRUE)}
    )
  }
  
  if (!("package:data.table" %in% search())) {
    stopifnot(requireNamespace("data.table"))
    on.exit(detach("package:data.table", unload=TRUE))
  }
  
  ## extract data from a single .xlsx sheet
  readEIU_sheet <- function(sheet_num, codeCol, startCol) {
    
    # read metadata
    name <- attr(sheets, "names")[sheet_num]
    
    num_vars_cell <- readRows(sheets[[sheet_num]], startRow = 3, endRow=3, startColumn = 1, endColumn = 1)
    num_vars <- as.integer(regmatches(num_vars_cell, regexpr(pattern = "(\\d+)", num_vars_cell)))
    
    num_years_cell <- readRows(sheets[[sheet_num]], startRow = 3, endRow=3, startColumn = 2, endColumn = 2)
    num_years <- diff(as.integer(regmatches(num_years_cell, gregexpr(pattern = "(\\d+)", num_years_cell))[[1]])) + 1 
    
    endCol <- startCol + num_years - 1
    
    # read the data
    dates <- readRows(sheets[[sheet_num]], startRow=4, endRow=4, startColumn = startCol, endColumn = endCol)
    var <- readColumns(sheets[[sheet_num]], startRow=4, endRow=4+num_vars, startColumn = codeCol, endColumn = codeCol, header = T, stringsAsFactors=F)
    values <- readRows(sheets[[sheet_num]], startRow=5, endRow=4+num_vars, startColumn = startCol, endColumn = endCol)
    
    # convert string matrix to numeric
    values[values %in% c("n.a.", "", " ") ] <- NA 
    values <- apply(values, 2, FUN = as.numeric)
    
    dtReturn <- data.table(country = rep(name, num_years), year = dates[1,], t(values))
    setnames(dtReturn, c("country", "year", var[,1]))
    
    dtReturn[, year := as.integer(year)]
    
    return(dtReturn)
  }
  
  wb <- loadWorkbook(file) 
  sheets <- getSheets(wb)
  pocet <- length(sheets) ## hidden sheets?
  
  dtTEMP <- rbindlist(lapply(1:pocet, readEIU_sheet, ...))
  
  SeriesNamesActual <- dfRenameSeries[, 1] %in% colnames(dtTEMP)
  notFound <- !(colnames(dtTEMP) %in% dfRenameSeries[, 1])
  # notFound <- 
  if (sum(notFound) > 0) {
    warning(paste0("Could not find variable names for: ", paste0(colnames(dtTEMP)[notFound], collapse = ", "), collapse = " "))
  }
  
  # rename variables - data.frame with EUI series codes in the first column and new variable names in the second column
  if (!is.null(dfRenameSeries)) {
    if (!all(dim(as.data.frame(dfRenameSeries)) >= c(2, 1))) {
      warning("Could not rename the series names.")
    } else {
      setnames(dtTEMP, dfRenameSeries[SeriesNamesActual, 1], dfRenameSeries[SeriesNamesActual, 2])
    }
  }
  
  return(dtTEMP)
}

