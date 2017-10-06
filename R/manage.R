# --------------- install_github_proxy -------------------
#' set proxy seetings and call install_github() 
#' @export
install_github_proxy <- function(..., 
                                 proxy = "auto",
                                 port  = NULL) {
  
  # check presence and load necessary packages
  # devtools package
  if (!("package:devtools" %in% search())) {
    tryCatch(require(devtools), error = function(x) {warning(x); cat("Cannot load devtools package \n")})
    on.exit(detach("package:devtools", unload=TRUE))
  }
  
  if (proxy == "auto") {
    if (.Platform$OS.type == "windows") {
      winprox <- getIEProxy()
      proxy   <- winprox[1]
      port    <- if (!is.null(port)) port else as.integer(winprox[2])
    } else {
      proxy <- NULL
      port  <- NULL
    }
  }
  
  if (!is.null(proxy)) {
    
    # httr package
    if (!("package:httr" %in% search())) {
      tryCatch(require(httr), error = function(x) {warning(x); cat("Cannot load httr package \n")})
      on.exit(detach("package:httr", unload=TRUE))
    }
    
    # set proxy via httr set_config() command
    httr::set_config(use_proxy(url = proxy, port = port))
  }
  
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


#' @export
generateDocRow <- function(filename, fields) {
  
  fieldString <- function(field){
    
    re <- regexpr(pattern = paste0("#'[ ]*", field, ":[ ]*"), 
                  text = cont,
                  perl = T)
    
    return(
      substr(cont[re > 0][1],
             start = re[re > 0][1] + attr(re, "match.length")[re > 0][1],
             stop = nchar(cont[re > 0][1])
      )
    )
  }  
  
  fls <- file(filename, "r")
  cont <- readLines(fls)
  close(fls)
  
  matchedFields <- sapply(fields,
                          fieldString)
  
  return(paste0("| ", basename(filename), " | ", paste(matchedFields, collapse = " | "), " |\n"))
}

#' Search for a file in different places and source the first hit.
#' @param \code{file} name of the file
#' @param \code{places} character vector of places (directories) where to search for the file
#' @param \code{...} other arguments will be passed to \code{source}
#' @export
source_search <- function(file, places, ...) {
  for (place in places) {
    if (file.exists(file.path(place, file))) {
      source(file.path(place, file), ...)
      return(invisible(TRUE))
    }
  }
  stop("File ", file, " not found.")  
}

