

#' Create a Datalicense request
#'
#' @param firmName The firm name assigned to you by Bloomberg
#' @param programName The name of the Datalicense program. Namely: 
#' getdata, gethistory, getquotes, getallquotes, getactions, getcompany, getsnap
#' @param fields A vector containing the fields to be downloaded, 
#' e.g. fields = c('PX_LAST', 'PX_CLOSE')
#' @param instruments A vector containing th instruments to be downloaded, 
#' e.g. instruments = c('IBM US Equity')
#' @param header A vector containing additional header fields, 
#' e.g. header = c(PROGRAMFLAG = 'oneshot', SECMASTER = 'yes')
#'   
#' @return a BdlRequest object
#' @export
BdlRequest <- function(
    firmName,
    programName,
    fields,
    instruments,
    header = c()
  ) {
  
  bdlRequest <- list()
  bdlRequest$header <- c(FIRMNAME = firmName, header, PROGRAMNAME = programName)
  bdlRequest$fields <- fields
  bdlRequest$instruments <- instruments
  class(bdlRequest) <- append(class(bdlRequest), c("BdlRequest"))
  
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
    
  res <- paste0(res, '\n\r', 'END-OF-FILE')
  return (res)
}


UploadFTP <- function(content, ftpConnection, targetFileName) {
  url <- paste(ftpConnection, targetFileName, sep = '/')
  ftpUpload(I(content), url)
}
