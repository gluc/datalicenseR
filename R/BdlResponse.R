#' Handle that lets you retrieve a data file from Bloomberg
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


#' Download the data, if available
#' 
#' @param bdlResponse The BdlResponse object returned by the UploadRequest function
#' 
#' @return either NULL (if the file is not yet available) or a data.frame containing the data
#' 
#' @seealso UploadRequest
#' @export
GetBdlData <- function(bdlResponse) {
  if (!inherits(bdlResponse,"BdlResponse")) stop("bdlResponse must be of class BdlResponse")
  ftpDownloadResult <- DownloadFTP(bdlResponse$bdlConnection$connectionString , bdlResponse$targetFileName, delete = FALSE)
  if(ftpDownloadResult$success) {
    res <- ParseBdlResponseFile(res)
    return (ftpDownloadResult$content)
  } else if(ftpDownloadResult$errorCode == "REMOTE_FILE_NOT_FOUND") {
    return (NULL)
  } else {
    stop(paste0(ftpDownloadResult$errorCode, ": ", ftpDownloadResult$errorMsg))
  }
}


#' Convert file content to data.frame
#' 
#' @param a string
#' @return a data.frame
ParseBdlResponseFile <- function(content) {
  #convert to data.frame
  return (data.frame())
}


  
  FTPDownloadResult <- function(content, success, msg) {
    
    
}