#' Create an FTP or SFTP connection to Bloomberg Datalicense
#' 
#' 
#' @param user The account number assigned by Bloomberg
#' @param pw The password of your Bloomberg account
#' @param host The Bloomberg ftp host
#' @param encrypt If \code{TRUE}, then SFTP is used
#' @param ftpType Any of \code{passive}, \code{active}
#' @param port The port used by FTP (20 for active, 21 for passive connections) or by SFTP (30206)
#' @param passivePorts In the case of ftpMode = 'passive', this is the min and the max port for the data port range.

#' @return an S3 BdlConnection object, used to upload requests and download prices.
#' @export
BdlConnection <- function(user, 
                          pw, 
                          host = 'bfmrr.bloomberg.com', 
                          encrypt = FALSE,
                          ftpType = 'passive',
                          port = 21, 
                          passivePorts = c(32768, 65535)) {
 
  
  
    # ftp://User:Password@FTPServer/Destination.html
    connectionString <- paste0('ftp://', user, ':', pw, '@', host)
    
    if (encrypt) connectionString <- paste0('s', connectionString)
    
    bdlConnection <- list();
    bdlConnection$connectionString <- connectionString
    

    
    bdlConnection$UploadRequest <- UploadRequest
    
    class(bdlConnection) <- append(class(bdlConnection),"BdlConnection")
    
    return (bdlConnection)
    
}


#' Uploads a BdlRequest to Bloomberg
#' 
#' @param bdlRequest A BdlRequest object
#' @param bdlConnection A BdlConnection object
#' @param targetFilePath The name the file should have at the Bloomberg FTP site 
#' @return BdlResponse
#' 
#' @seealso BdlConnection
#' @seealso BdlRequest
#' @seealso BdlResponse
#' 
#' @export
UploadRequest <- function(bdlRequest, bdlConnection, targetFilePath) {
  if (!inherits(bdlRequest,"BdlRequest")) stop("bdlRequest must be of class BdlRequest")
  if (!inherits(bdlConnection,"BdlConnection")) stop("bdlConnection must be of class BdlConnection")
  if (!inherits(targetFilePath,"character")) stop("targetFilePath must be of class character")
  
  request <- print(bdlRequest)
  UploadFTP(request, bdlConnection$connectionString, targetFilePath)
  response <- BdlResponse(bdlRequest, bdlConnection, targetFilePath)
  return (response)
}




UploadFTP <- function(content, ftpConnection, targetFileName) {
  url <- paste(ftpConnection, targetFileName, sep = '/')
  ftpUpload(I(content), url)
}


FTPErrorHandler <- function() {
  
  me <- environment()
  me$handleError <- function(e) {
    me$errorMsg <- e$message
    me$errorCode <- class(e)[1]
  }
  class(me) <- append(class(me), "FTPErrorHandler")
  return (me)
}


#' stores the result string and the download success
#' 
#' @param content The content of the Downloaded file
#' @param success TRUE if OK
#' @param errorMsg The error message, if !success
#' @param errorCode The error code, if applicable
#' 
#' @seealso DownloadFTP  
FTPDownloadResult <- function(content, success, errorMsg, errorCode) {
  me <- list()
  me$content <- content
  me$success <- success
  me$errorMsg <- errorMsg
  me$errorCode <- errorCode
  class(me) <- append(class(me), "FTPDownloadResult")
  return (me)
}


#' Downloads a file via FTP. 
#' 
#' @param baseURL The base URL
#' @param filePath The path to the file, relative from the baseURL
#' @param delete Whether or not to delete the file after successful downloading
#' 
#' @return An FTPDownloadResult
#' @seealso FTPDownloadResult
DownloadFTP <- function(baseURL, filePath, delete = FALSE) {

  #see here for libcurl options: http://stackoverflow.com/questions/17119449/rcurl-boolean-options
  #resp http://curl.haxx.se/libcurl/c/curl_easy_setopt.html
  url <- paste(baseURL, filePath, sep = '/')
  h <- basicTextGatherer()
  e <- FTPErrorHandler() 
  if (delete) postquote <- c(paste0("DELE ", '/', filePath))
  else postquote <- c()
  
  x <- tryCatch(curlPerform(url = url, 
                            upload = FALSE, 
                            curl = getCurlHandle(), 
                            postquote = postquote, 
                            writefunction = h$update),
               #COULDNT_RESOLVE_HOST = function(x) e$handleError('COULDNT_RESOLVE_HOST', x$message)
               REMOTE_FILE_NOT_FOUND = e$handleError
               # error = function(x) e$handleError(x)  )
              )
  
  res <- FTPDownloadResult(h$value(), is.null(e$errorCode), e$errorMsg, e$errorCode)
  return (res)
}