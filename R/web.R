#' @export
getIEProxy <- function() {
  syscall <-   system2(command = "REG", 
                       args = "QUERY \"HKCU\\Software\\Microsoft\\Windows\\CurrentVersion\\Internet Settings\" /v proxyServer", 
                       stdout = T, stderr = T)
  
  syscall <- paste0(syscall, collapse = "")
  
  # match proxy url:port
  proxy <- regexpr("\\H*$", syscall, perl = T )
  proxy <- regmatches(syscall, proxy)
  
  proxy <- strsplit(proxy, split = ":", fixed = T)[[1]] # split url and port
  proxy[2] <- as.integer(proxy[2])
  
  return(proxy)
}

#' setup httr proxy settings
#' @export
httr_proxy <- function(proxy = getIEProxy(), ...) {
  # attach httr
  if (!("package:httr" %in% search())) {
    if (!requireNamespace("httr")) stop("Cannot load httpr package \n")
  }
  
  if (!is.null(proxy) && proxy != "auto") {
    httr::set_config(httr::use_proxy(url = proxy[1], port = as.integer(proxy[2]), ...))
  }
}