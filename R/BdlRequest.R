

#' Create a GetData request
#' 
#' @return a BdlRequest object
#' @export
BdlRequestGetData <- function() {
  
  bdlRequest <- list()
  
  class(bdlRequest) <- append(class(bdlRequest), c("BdlRequest", "BdlRequestGetData"))
  
  return (bdlRequest)
}


#' Convert a BdlRequest instance to a character string
#' 
#' @export
print.BdlRequest <- function(bdlRequest, ...) {
  if (!inherits(bdlRequest,"BdlRequest")) stop("bdlRequest must be of class BdlRequest")
  NextMethod("print",bdlRequest)
}

#' Convert a BdlRequestGetData instance to a character string
#' 
#' @export
print.BdlRequestGetData <- function(bdlRequest, ...) {
  if (!inherits(bdlRequest,"BdlRequestGetData")) stop("bdlRequest must be of class BdlRequestGetData")
  return ("GetData BdlRequest")
  
}



UploadFTP <- function(content, ftpConnection, targetFileName) {
  url <- paste(ftpConnection, targetFileName, sep = '/')
  ftpUpload(I(content), url)
}
