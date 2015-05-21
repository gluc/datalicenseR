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
  
  return (responseFileName)
}


#' Download the data, if available
#' 
#' @param bdlConnection The BdlConnection object used to establish the FTP download
#' @param responseFileName The file downloaded
#' 
#' @return either NULL (if the file is not yet available) or a data.frame containing the data
#' @import stringr
#' @seealso UploadRequest
#' @seealso BdlResponseHandle
#' @export
TryGetBdlData <- function(bdlConnection, responseFileName) {
  if (!inherits(bdlConnection,"BdlConnection")) stop("bdlConnection must be of class BdlConnection")
  if (!inherits(responseFileName,"character")) stop("responseFileName must be of class character")
  print(Sys.time())
  ftpDownloadResult <- DownloadFTP(bdlConnection$connectionString , responseFileName, delete = FALSE)
  if(ftpDownloadResult$success) {
    decryptedResult <- DecryptBdlResponse(ftpDownloadResult$content, bdlConnection$key, responseFileName)
    res <- ParseBdlResponse(decryptedResult)
    return (res)
  } else if(ftpDownloadResult$errorCode == "REMOTE_FILE_NOT_FOUND") {
    return (NULL)
  } else {
    stop(paste0(ftpDownloadResult$errorCode, ": ", ftpDownloadResult$errorMsg))
  }
}


#' Download the data in sync mode, waiting until the result file is there
#' 
#' @param bdlConnection The BdlConnection object used to establish the FTP download
#' @param responseFileName The file downloaded
#' 
#' @return the response content
#' @seealso UploadRequest
#' @seealso BdlResponseHandle
#' @export
DownloadResponse <- function(bdlConnection, responseFileName) {
  res <- NULL
  while (is.null(res)) {
    print('File not yet available, waiting...')
    Sys.sleep(time = 60)
    res <- TryGetBdlData(con, respFileName)
  }
  return (res)
}


#' @import libdes
DecryptBdlResponse <- function(bdlContent, key, responseFileName) {
  fileName <- tempfile()
  decFile <- paste0(fileName, '.dec')
  cat(bdlContent, file = fileName)
  DecryptFile(fileName, decFile, key, UUENC = TRUE)
  decryptedContent <- readChar(decFile, file.info(decFile)$size)
  return (decryptedContent)
}


#' Convert Bloomberg out file content to data.frame
#' 
#' @param bdlOutContent The content string
#' @return a data.frame
#'  
#' @export
ParseBdlResponse <- function(bdlOutContent) {
  #convert to data.frame
  
  #col names
  start <- str_locate(bdlOutContent, 'START-OF-FIELDS')[2] + 1
  end <- str_locate(bdlOutContent, 'END-OF-FIELDS')[1] - 1
  fieldsStr <- str_trim(str_sub(bdlOutContent, start, end))
  colNames <- str_split(fieldsStr, '\n')[[1]]
  colNames <- c('Status', colNames)
  colClasses <- c('numeric', replicate(length(colNames) - 1, 'numeric'))
  
  
  
  #data
  start <- str_locate(bdlOutContent, 'START-OF-DATA')[2] + 1
  end <- str_locate(bdlOutContent, 'END-OF-DATA')[1] - 1
  rowsStr <- str_trim(str_sub(bdlOutContent, start, end))
  
  rows <- str_split(rowsStr, '\n')[[1]]
  
  df <- read.table(text = "", colClasses = colClasses, col.names = colNames)
  
  for (rowNum in 1:length(rows)) {
    dfCols <- str_trim(str_split(rows[rowNum], coll('|'))[[1]])
    
    df[rowNum, 'Status']  <- as.integer(dfCols[2])
    
    for (colNum in 4:(length(dfCols) - 1)) {
      if( is.numeric(dfCols[colNum]) ) {
        val <- as.numeric(dfCols[colNum])
      } else {
        val <- as.character(dfCols[colNum])
      }
      df[rowNum, colNum - 2] <- val
    }
    
    rownames(df)[rowNum] <- dfCols[1]
  }
  
  return (df)
  
}


  
