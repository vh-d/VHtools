# --------------- install_github_proxy -------------------
#' set proxy seetings and call install_github() 
#' @export
install_github_proxy <- function(..., 
                                 proxy = getIEProxy()[1],
                                 port = as.integer(getIEProxy()[2])) {
  
  # check presence and load necessary packages
  # devtools package
  if (!("package:devtools" %in% search())) {
    tryCatch(require(devtools), error = function(x) {warning(x); cat("Cannot load devtools package \n")})
    on.exit(detach("package:devtools", unload=TRUE))
  }
  
  # httr package
  if (!("package:httr" %in% search())) {
    tryCatch(require(httr), error = function(x) {warning(x); cat("Cannot load httr package \n")})
    on.exit(detach("package:httr", unload=TRUE))
  }
  
  # set proxy via httr set_config() command
  httr::set_config(use_proxy(url = proxy, port = port))
  
  # run devtools install_github() command
  devtools::install_github(...)
}

# --------------- instGitHubProxy -------------------
#' set proxy seetings and call install_github() 
instgithubproxy <- install_github_proxy



# --------------- update_github -------------------
#' update all R packages installed from GitHub
#' @export
update_github_pkgs <- function(...) {
  # check/load necessary packages
  # devtools package
  if (!("package:devtools" %in% search())) {
    tryCatch(require(devtools), error = function(x) {warning(x); cat("Cannot load devtools package \n")})
    on.exit(detach("package:devtools", unload=TRUE))
  }
  
  pkgs <- installed.packages(fields = "RemoteType")
  github_pkgs <- pkgs[pkgs[, "RemoteType"] %in% "github", "Package"]
  
  # what packages are going to be updated
  print(github_pkgs)
  
  # main loop
  lapply(github_pkgs, function(pac) {
    message("Updating ", pac, " from GitHub...")
    
    repo = packageDescription(pac, fields = "GithubRepo")
    username = packageDescription(pac, fields = "GithubUsername")
    
    install_github_proxy(repo = paste0(username, "/", repo), ...)
  })
}

#' generate documentation for a folder of source files
#' @param \code{folder} path to the folder with files to be documented
#' @param \code{pattern} file name filter
#' @param \code{fields} attributes/metadata that should be searched for as a named vector 
#' such as \code{c(Title = "Title of the file")} where \emph{Title} is the pattern and \emph{Title of the file} is used as header
#' @return a character vector of a markdown table with header based on \code{fields} values
#' @examples 
#' # chunk of a Rmarkdown doc
#' fields <- c(Title = "Data source", DO = "What the code does", Author = "Author")
#' tab <- generateDocs(fields = fields)
#' paste(tab, collapse = "")
#' @export
generateDocs <- function(folder = ".", pattern = "*.R$", fields) {
  return(
    c(paste0("| File name", " | ", paste(fields, collapse = " | "), " |\n"),
      paste0("|", paste(rep("---", times = length(fields) + 1), collapse = "|"), "|\n"),
      sapply(dir(path = folder, 
                 full.names = T,
                 pattern = pattern),
             generateDocRow, 
             fields = names(fields))
    )
  )
}

