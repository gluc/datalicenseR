#' Create an FTP or SFTP connection to Bloomberg Datalicense
#' 
#' 
#' @param user The account number assigned by Bloomberg
#' @param pw The password of your Bloomberg account
#' @param key The DES decryption key of your Bloomberg account
#' 
#' @return an S3 BdlConnection object, used to upload requests and download prices.
#' @export
BdlConnection <- function(user, 
                          pw,
                          key) {
 
  
    sftp <- FALSE #libcurl doesn't support SFTP
    if (sftp) {
      host <- 'dlsftp.bloomberg.com'
      port <- 30206
      protocol <- 'sftp'
    } else { 
      host <- 'bfmrr.bloomberg.com'
      port <- 21
      protocol <- 'ftp'
    }
  
    
  
    # ftp://User:Password@FTPServer/Destination.html
    connectionString <- paste0(protocol, '://', user, ':', pw, '@', host, ':', port)
    
    bdlConnection <- list();
    bdlConnection$connectionString <- connectionString
    bdlConnection$key <- key
    
    class(bdlConnection) <- append(class(bdlConnection),"BdlConnection")
    
    return (bdlConnection)
    
}


#' Uploads a request file to Bloomberg
#' 
#' @param bdlConnection A BdlConnection object
#' @param bdlRequest A BdlRequestBuilder object, or a character string containing the request content
#' @param targetFileName The target file name of the request at the Bloomberg FTP site 
#' @param responseFileName The name of the response file. If omitted, the method deducts 
#' the response file name either from the BdlRequestBuilder, or from the targetFileName
#' @return A character string, representing the name of the response file
#' 
#' @seealso BdlConnection
#' @seealso BdlRequestBuilder
#' 
#' @export
UploadRequest <- function(bdlConnection, bdlRequest, targetFileName, responseFileName = NULL) {
  if (!inherits(bdlConnection,"BdlConnection")) stop("bdlConnection must be of class BdlConnection")
  if (!(inherits(bdlRequest,"BdlRequestBuilder") || inherits(bdlRequest, "character"))) stop("bdlRequest must be of class BdlRequestBuilder or character")
  if (!inherits(targetFileName,"character")) stop("targetFileName must be of class character")
  if (!(is.null(responseFileName) || inherits(targetFileName,"character"))) stop("targetFileName must be of class character")
  
  request <- as.character(bdlRequest)
  UploadFTP(request, bdlConnection$connectionString, targetFileName)
  responseFileName <- DeriveResponseFileName(bdlRequest, targetFileName, responseFileName)
  DeleteFTPFile(bdlConnection$connectionString, responseFileName)
  return (responseFileName)
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
DownloadFTP_1 <- function(baseURL, filePath, delete = FALSE) {
  url <- paste(baseURL, filePath, sep = '/')
  destFile <- tempfile()
  
  code <- download.file(url, destfile = destFile, method = 'curl', quiet = TRUE)
  res <- FTPDownloadResult(destFile, code == 0, '', code)
  return (res)
}



#' Downloads a file via FTP. 
#' 
#' @param baseURL The base URL
#' @param filePath The path to the file, relative from the baseURL
#' @param delete Whether or not to delete the file after successful downloading
#' 
#' @return An FTPDownloadResult
#' @seealso FTPDownloadResult
DownloadFTP_2 <- function(baseURL, filePath, delete = FALSE) {

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
  if (res$success) {
    destFile <- tempfile()
    cat(file = destFile)
    res$content <- destFile
  }
  return (res)
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
  destFile <- tempfile()
  file.create(destFile)
  f = CFILE(destFile, 'w')
  x <- tryCatch(curlPerform(url = url, 
                            upload = FALSE, 
                            curl = getCurlHandle(), 
                            postquote = postquote, 
                            writedata = f@ref),
                #COULDNT_RESOLVE_HOST = function(x) e$handleError('COULDNT_RESOLVE_HOST', x$message)
                REMOTE_FILE_NOT_FOUND = e$handleError
                # error = function(x) e$handleError(x)  )
  )
  close(f)
  res <- FTPDownloadResult(destFile, is.null(e$errorCode), e$errorMsg, e$errorCode)
  return (res)
}


DeleteFTPFile <- function(baseURL, filePath) {
  
  #see here for libcurl options: http://stackoverflow.com/questions/17119449/rcurl-boolean-options
  #resp http://curl.haxx.se/libcurl/c/curl_easy_setopt.html
  url <- paste0(baseURL, sep = '/')
  h <- basicTextGatherer()
  e <- FTPErrorHandler() 
  quote <- c(paste0("DELE ", '/', filePath))
  
  x <- tryCatch(curlPerform(url = url, 
                            upload = FALSE, 
                            curl = getCurlHandle(), 
                            quote = quote, 
                            writefunction = h$update),
                #COULDNT_RESOLVE_HOST = function(x) e$handleError('COULDNT_RESOLVE_HOST', x$message)
                REMOTE_FILE_NOT_FOUND = e$handleError,
                error = function(x) e$handleError(x)
  )
  if (is.null(e$errorCode)) return (TRUE)
  return (FALSE)

}