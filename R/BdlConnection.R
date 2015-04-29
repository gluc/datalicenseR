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
#' @param targetFileName The name the file should have at the Bloomberg FTP site 
#' @return BdlResponse
#' 
#' @seealso BdlConnection
#' @seealso BdlRequest
#' @seealso BdlResponse
#' 
#' @export
UploadRequest <- function(bdlRequest, bdlConnection, targetFileName) {
  if (!inherits(bdlRequest,"BdlRequest")) stop("bdlRequest must be of class BdlRequest")
  if (!inherits(bdlConnection,"BdlConnection")) stop("bdlConnection must be of class BdlConnection")
  if (!inherits(targetFileName,"character")) stop("targetFileName must be of class character")
  
  request <- BdlRequest$GetBdlRequest()
  UploadFTP(request, bdlConnection$connectionString, targetFileName)
  response <- BdlResponse(bdlRequest, bdlConnection, targetFileName)
  return (response)
}





DownloadFTP <- function(ftpConnection, file, delete = FALSE) {

  #see here for libcurl options: http://stackoverflow.com/questions/17119449/rcurl-boolean-options
  #resp http://curl.haxx.se/libcurl/c/curl_easy_setopt.html
  url <- paste(ftpConnection, file, sep = '/')
  h = basicTextGatherer()
  if (delete) postquote <- c(paste0("DELE ", '/', file))
  else postquote <- c()
  x <- curlPerform(url = url, upload = FALSE, curl = getCurlHandle(), 
                   postquote = postquote, writefunction = h$update)
  h$value()
}