

#' Helper to create a Datalicense request
#' 
#' Please refer to the Bloomberg Per Security Enterprise Solution manual for details.
#'
#' @param header A vector containing additional header fields, 
#' e.g. header = c(FIRMNAME = 'dl1234', PROGRAMNAME = 'getdata', PROGRAMFLAG = 'oneshot', SECMASTER = 'yes')
#' @param fields A vector containing the fields to be downloaded, 
#' e.g. fields = c('PX_LAST', 'PX_CLOSE')
#' @param data A vector containing the instruments to be downloaded, 
#' e.g. data = c('IBM US Equity')
#'   
#' @return a BdlRequestBuilder object
#' @export
BdlRequestBuilder <- function(
    header,
    fields,
    data
  ) {
  
  if (!inherits(header,"character")) stop("header must be of class character")
  if (!inherits(fields,"character")) stop("fields must be of class character")
  if (!inherits(data,"character")) stop("data must be of class character")
  
  
  bdlRequest <- list()
  bdlRequest$header <- header
  bdlRequest$fields <- fields
  bdlRequest$data <- data
  class(bdlRequest) <- append(class(bdlRequest), c("BdlRequestBuilder"))
  
  return (bdlRequest)
}


#' Convert a BdlRequestBuilder instance to a character string
#' 
#' @param x A BdlRequestBuilder object
#' @param ... Any additional parameters
#' 
#' @export
as.character.BdlRequestBuilder <- function(x, ...) {
  if (!inherits(x,"BdlRequestBuilder")) stop("x must be of class BdlRequestBuilder")
  res <- 'START-OF-FILE'
  
  #header
  for (key in names(x$header)) {
    res <- paste0(res, '\n', key, '=', x$header[key])
  }
  
  res <- paste0(res, '\n')
  
  #fields
  res <- paste0(res, '\n', 'START-OF-FIELDS')
  for (field in x$fields) {
    res <- paste0(res, '\n', field)
  }
  res <- paste0(res, '\n', 'END-OF-FIELDS')
  

  res <- paste0(res, '\n')
    
  #instruments / data
  res <- paste0(res, '\n', 'START-OF-DATA')
  
  for (instrument in x$data) {
    res <- paste0(res, '\n', instrument)
  }  
  res <- paste0(res, '\n', 'END-OF-DATA')
  
  res <- paste0(res, '\n')
    
  res <- paste0(res, '\n', 'END-OF-FILE')
  return (res)
}

