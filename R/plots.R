# --------------- saveggplot -------------------
#' save last ggplot to a file 
#' 
#' \code{saveggplot} save last printed ggplot to a file
#' @param file - name of the file. By default title of the plot is used.
#' @param dir - destination folder - "..\/output\/" by default.
#' @param format - ".wmf" or ".pdf"
#' @param width - width of the plot
#' @param height - height of the plot
#' @export
saveggplot <- function(file = clDiacr(last_plot()$labels$title), dir = "../output/", format = ".wmf", width = 9, height = 9) {
  if (format == ".wmf") {
    win.metafile(paste0(dir, file, ".wmf"), height = height, width = width)
  } else {
    pdf(paste0(dir, file, ".pdf"), height = height, width = width)
  }
  print(last_plot())
  dev.off()
}


