

#' Create a GetData request
#' 
#' @return a BdlRequest object
#' @export
BdlRequestGetData <- function(
    firmName,
    fields,
    instruments,
    header = c()
  ) {
  
  bdlRequest <- list()
  bdlRequest$header <- c(FIRMNAME = firmName, header, PROGRAMNAME = 'getdata')
  bdlRequest$fields <- fields
  bdlRequest$instruments <- instruments
  class(bdlRequest) <- append(class(bdlRequest), c("BdlRequest", "BdlRequestGetData"))
  
  return (bdlRequest)
}


#' Convert a BdlRequest instance to a character string
#' 
#' @export
print.BdlRequest <- function(bdlRequest, ...) {
  if (!inherits(bdlRequest,"BdlRequest")) stop("bdlRequest must be of class BdlRequest")
  res <- 'START-OF-FILE'
  
  #header
  for (key in names(bdlRequest$header)) {
    res <- paste0(res, '\n\r', key, '=', bdlRequest$header[key])
  }
  
  res <- paste0(res, '\n\r')
  
  #fields
  res <- paste0(res, '\n\r', 'START-OF-FIELDS')
  for (field in bdlRequest$fields) {
    res <- paste0(res, '\n\r', field)
  }
  res <- paste0(res, '\n\r', 'END-OF-FIELDS')
  

  res <- paste0(res, '\n\r')
    
  #instruments / data
  res <- paste0(res, '\n\r', 'START-OF-DATA')
  
  for (instrument in bdlRequest$instruments) {
    res <- paste0(res, '\n\r', instrument)
  }  
  res <- paste0(res, '\n\r', 'END-OF-DATA')
  
  res <- paste0(res, '\n\r')
    
  res <- paste0(res, '\n\r', NextMethod("print",bdlRequest))
  res <- paste0(res, '\n\r', 'END-OF-FILE')
  return (res)
}

#' Convert a BdlRequestGetData instance to a character string
#' 
#' @export
print.BdlRequestGetData <- function(bdlRequest, ...) {
  if (!inherits(bdlRequest,"BdlRequestGetData")) stop("bdlRequest must be of class BdlRequestGetData")
  # add GetData specific stuff, if required
  return ('')
  
}



UploadFTP <- function(content, ftpConnection, targetFileName) {
  url <- paste(ftpConnection, targetFileName, sep = '/')
  ftpUpload(I(content), url)
}
