#' Handle to let you retrieve a data file from Bloomberg
#' 
#' Typically, this is not called directly, but returned when you call the 
#' 
#' @param bdlRequest The request
#' @param bdlConnection The connection
#' @param targetFileName The name of the file at Bloomberg
#' 
#' @seealso UploadRequest
#' @seealso BdlRequest
#' 
BdlResponse <- function(bdlRequest, bdlConnection, targetFileName) {
  
  if (!inherits(bdlRequest,"BdlRequest")) stop("bdlRequest must be of class BdlRequest")
  if (!inherits(bdlConnection,"BdlConnection")) stop("bdlConnection must be of class BdlConnection")
  if (!inherits(targetFileName,"character")) stop("targetFileName must be of class character")
  
  bdlResponse <- list()
  bdlResponse$bdlRequest <- bdlRequest
  bdlResponse$bdlConnection <- bdlConnection
  bdlResponse$targetFileName <- targetFileName
  class(bdlRequest) <- append(class(bdlRequest), c("BdlResponse"))
  
  return (bdlRequest)
}



IsAvailable <- function(bdlResponse) {
  if (!inherits(bdlResponse,"BdlResponse")) stop("bdlResponse must be of class BdlResponse")
  
  
}