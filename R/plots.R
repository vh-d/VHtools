# --------------- saveggplot -------------------
#' save last ggplot to a file 
#' 
#' \code{saveggplot} save last printed ggplot to a file
#' @param file   - name of the file. By default title of the plot is used.
#' @param chart  - name of the file. By default title of the plot is used.
#' @param dir    - destination folder - "..\/output\/" by default.
#' @param format - wmf\/pdf\/eps\/svg
#' @param width  - width of the plot
#' @param height - height of the plot
#' @export
saveggplot <- function(file   = NULL, 
                       chart  = last_plot(),
                       dir    = ifelse(is.null(getOption(".DIR_PLOT")), "../output/", getOption(".DIR_PLOT")), 
                       format = c("wmf", "pdf", "eps", "svg"), 
                       width  = 9, 
                       height = 9, 
                       ...) {
  
  if (is.null(file)) file <- clDiacr(last_plot()$labels$title)
  format <- match.arg(format)
  
  
  if (.Platform$OS.type != "windows" & format == "wmf") {
    warning("Windows metafile not supported. Overriding format to svg.")
    format <- "svg"
  } 
  
  switch(format,
         wmf = win.metafile(file = paste0(dir, file, ".wmf"), height = height, width = width, ...),
         pdf = pdf(file          = paste0(dir, file, ".pdf"), height = height, width = width, ...),
         eps = postscript(file   = paste0(dir, file, ".eps"), height = height, width = width, ...),
         svg = svg(file          = paste0(dir, file, ".svg"), height = height, width = width, ...),
         ... = stop("Format not supported."))
  print(chart)
  dev.off()
}

#' @export
col_palette <- 
  c("blue"       = "#3F84C4", 
    "red"        = "#EA5B3F",
    "green"      = "#21A535", 
    "violet"     = "#93338C",
    "blue2"      = "#59C6F2", 
    "orange"     = "#F29100",
    "green2"     = "#C4D31C",
    "yellow"     = "#FFDD00",
    "grey"       = "#B2B2B2",
    "pink"       = "#C59CC7")

#' @export
col_palette_val <- col_palette
attributes(col_palette_val) <- NULL
