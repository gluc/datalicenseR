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
    res <- ParseBdlResponseFile(ftpDownloadResult$content)
    return (res)
  } else if(ftpDownloadResult$errorCode == "REMOTE_FILE_NOT_FOUND") {
    return (NULL)
  } else {
    stop(paste0(ftpDownloadResult$errorCode, ": ", ftpDownloadResult$errorMsg))
  }
}


#' Convert Bloomberg out file content to data.frame
#' 
#' @param bdlOutContent The content string
#' @return a data.frame
#'  
#' @export
ParseBdlResponseFile <- function(bdlOutContent) {
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
      df[rowNum, colNum - 2] <- as.numeric(dfCols[colNum])
    }
    
    rownames(df)[rowNum] <- dfCols[1]
  }
  
  return (df)
  
}


  
