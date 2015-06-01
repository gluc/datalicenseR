library(stringr)
library(libdes)

#' Derives the expected response file name
#'  
#' 
#' @param bdlRequest A BdlRequestBuilder object, or a character string 
#' or a character string containing the request content, optional
#' @param requestFileName The target file name of the request at the Bloomberg FTP site, optional
#' @param responseFileName The name of the response file. If omitted, the method deducts 
#' the response file name either from the BdlRequestBuilder, or from the requestFileName
#' @return a character string, representing the responseFileName derived from the arguments provided
#' 
#' @seealso BdlRequestBuilder
#' 
DeriveResponseFileName <- function(bdlRequest = NULL, requestFileName = NULL, responseFileName = NULL) {
  if (!(is.null(bdlRequest) || inherits(bdlRequest,"BdlRequestBuilder") || inherits(bdlRequest, "character"))) stop("bdlRequest must be of class BdlRequestBuilder or character")
  if (!(is.null(requestFileName) || inherits(requestFileName,"character"))) stop("requestFileName must be of class character")
  if (!(is.null(responseFileName) || inherits(responseFileName,"character"))) stop("responseFileName must be of class character")
  if (((is.null(bdlRequest) || inherits(bdlRequest, "character")) && is.null(requestFileName) && is.null(responseFileName))) stop("either bdlRequest as BdlRequestBuilder, requestFileName, or responseFileName must be provided")
  
  
  if(is.null(responseFileName)) {
    #if responseFileName is provided, it always takes precedence
    if (inherits(bdlRequest, "BdlRequestBuilder") && "REPLYFILENAME" %in% names(bdlRequest$header)) {
      responseFileName <- as.character(bdlRequest$header['REPLYFILENAME'])
    } else if (!is.null(requestFileName)) {
      file <- substr(requestFileName, 1, nchar(requestFileName)-4)
      responseFileName <- paste0(file, '.out')
    } else {
      stop("must provide either bdlRequest as a BdlRequestBuilder object, requestFileName, or responseFileName")
    }
  }
  
  #if gzip
  if ( inherits(bdlRequest, "BdlRequestBuilder") && 
         "COMPRESS" %in% names(bdlRequest$header) &&
         "PROGRAMNAME" %in% names(bdlRequest$header) &&
         (bdlRequest$header['COMPRESS'] == 'yes'||
          bdlRequest$header['PROGRAMNAME'] %in% c('getquotes', 'getallquotes', 'gethistory')
          )
      ) {
      responseFileName <- paste0(responseFileName, '.gz')
  }
  
  return (responseFileName)
}


#' Download the data, if available
#' 
#' @param bdlConnection The BdlConnection object used to establish the FTP download
#' @param responseFileName The file downloaded
#' @param parser The parser used to convert the file into an R object
#' 
#' @return either NULL (if the file is not yet available) or a data.frame containing the data
#' @import stringr
#' @seealso UploadRequest
#' @seealso BdlResponseHandle
#' @export
TryGetBdlData <- function(bdlConnection, responseFileName, parser) {
  if (!inherits(bdlConnection,"BdlConnection")) stop("bdlConnection must be of class BdlConnection")
  if (!inherits(responseFileName,"character")) stop("responseFileName must be of class character")
  iszip <- str_sub(responseFileName, start= -3) == '.gz'
  print(paste0("downloading ftp ", responseFileName, "..."))
  ftpDownloadResult <- DownloadFTP(bdlConnection$connectionString , responseFileName, delete = FALSE)
  if(ftpDownloadResult$success) {
    
    print("decrypting file...")
    
    decFile <- DecryptBdlResponse(ftpDownloadResult$content, bdlConnection$key, iszip)
    print("unzipping...")
    #decryptedResult <- readChar(decFile, file.info(decFile)$size)
    decryptedResult <- paste0(readLines(decFile), collapse = '\n')
    print("parsing...")
    res <- parser(decryptedResult)
    return (res)
  } else if(ftpDownloadResult$errorCode == "REMOTE_FILE_NOT_FOUND") {
    return (NULL)
  } else if(ftpDownloadResult$errorCode == 78) {
    return (NULL)
  } else {
    stop(paste0(ftpDownloadResult$errorCode, ": ", ftpDownloadResult$errorMsg))
  }
}


#' Download the data in sync mode, waiting until the result file is there
#' 
#' @param bdlConnection The BdlConnection object used to establish the FTP download
#' @param responseFileName The file downloaded
#' @param parser The parser used to convert the file into an R object
#' @param pollFrequencySec The polling frequency to check if file is available at Bloomberg
#' @param timeoutMin The timeout in minutes
#' 
#' @return the response content
#' @seealso UploadRequest
#' @seealso BdlResponseHandle
#' @export
DownloadResponse <- function(bdlConnection, responseFileName, parser, pollFrequencySec = 40, timeoutMin = 10) {
  if(pollFrequencySec < 0) stop("pollFrequencySec must be > 0")
  if(timeoutMin < 0) stop("timeoutMin must be > 0")
  
  if(pollFrequencySec <= 20) warning("pollFrequencySec is recommended to be > 20")
  if(timeoutMin <= 3) warning("timeoutMin is recommended to be > 3")
  
  
  res <- NULL
  timeoutSec <- 60 * timeoutMin
  myTime <- 0
  while (is.null(res)) {
    res <- TryGetBdlData(bdlConnection, responseFileName, parser)
    if(is.null(res)) {
      print('File not yet available, waiting...')
      print(Sys.time())
      for (x in 1:as.integer(pollFrequencySec / 2)) {
        cat('.')
        Sys.sleep(2)
      }
      myTime <- myTime + pollFrequencySec
      cat('\r\n')
      if (myTime > timeoutSec) {
        print(paste0('Timeout! Could not download file ', responseFileName, ' from bloomberg in ', timeoutMin, ' min. Giving up!'))
        return (NULL)
      }
      
      print("Checking if file is available...")
    } else {
      print('File available! Downloading...')
    }
  }
  return (res)
}


#' @import libdes
DecryptBdlResponse <- function(fileName, key, iszip) {
  decFile <- paste0(fileName, '.dec')
  if (iszip) {
    decFile <- paste0(decFile, '.gz')
  }
  if(file.exists(decFile)) file.remove(decFile) #should not happen, is temp file
  DecryptFile(fileName, decFile, key, UUENC = TRUE)
  return (decFile)
}




  
