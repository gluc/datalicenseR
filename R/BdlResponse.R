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
  class(bdlResponse) <- append(class(bdlResponse), c("BdlResponse"))
  
  return (bdlResponse)
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
  
  if ("REPLYFILENAME" %in% names(bdlResponse$bdlRequest$header)) {
    replyFileName <- bdlResponse$bdlRequest$header$REPLYFILENAME
  } else {
    file <- substr(bdlResponse$targetFileName, 1, nchar(bdlResponse$targetFileName)-4)
    replyFileName <- paste0(file, '.out')
  }
  
  ftpDownloadResult <- DownloadFTP(bdlResponse$bdlConnection$connectionString , replyFileName, delete = FALSE)
  if(ftpDownloadResult$success) {
    res <- ParseBdlResponseFile(res)
    return (res)
  } else if(ftpDownloadResult$errorCode == "REMOTE_FILE_NOT_FOUND") {
    return (NULL)
  } else {
    stop(paste0(ftpDownloadResult$errorCode, ": ", ftpDownloadResult$errorMsg))
  }
}


#' Convert file content to data.frame
#' 
#' @param content The content string
#' @return a data.frame
ParseBdlResponseFile <- function(content) {
  #convert to data.frame
  return (data.frame())
}


  
