#' Convert Bloomberg out file content to data.frame
#' 
#' @param bdlOutContent The content string
#' @param columnHeader If TRUE, then column headers are auto detected. If FALSE, then it
#' is assumed no column headers exist.
#' @return a data.frame
#' 
#' @examples
#' #to look at an example:
#' data(getDataReply)
#' getDataReply
#'  
#' @export
GetDataParser <- function(bdlOutContent, columnHeader = TRUE) {
  bdlOutContent <- str_replace_all(bdlOutContent, "[\r]", "")
  
  #convert to data.frame
  
  #col names
  start <- str_locate(bdlOutContent, 'START-OF-FIELDS')[2] + 1
  end <- str_locate(bdlOutContent, 'END-OF-FIELDS')[1] - 1
  fieldsStr <- str_trim(str_sub(bdlOutContent, start, end))
  
  #col headers?
  if (columnHeader) {
    columnHeaders <- !is.na(str_locate(bdlOutContent, 'COLUMNHEADER=yes')[1])
  } else {
    columnHeaders <- FALSE
  }
  
  #data
  start <- str_locate(bdlOutContent, 'START-OF-DATA')[2] + 1
  end <- str_locate(bdlOutContent, 'END-OF-DATA')[1] - 1
  rowsStr <- str_trim(str_sub(bdlOutContent, start, end))
  
  rows <- str_split(rowsStr, '\n')[[1]]
  
  
  
  if (columnHeaders) {
    colNames <- head(str_trim(str_split(rows[1], coll('|'))[[1]])[-1],-1)[-2]
    rows <- rows[-1]
  } else {
    colNames <- str_split(fieldsStr, '\n')[[1]]
    colNames <- c('ERROR CODE', colNames)
  }
  colNames <- str_replace_all(colNames, " ", "_")
  colClasses <- c('numeric', replicate(length(colNames) - 1, 'numeric'))
  df <- read.table(text = "", colClasses = colClasses, col.names = colNames)
  for (rowNum in 1:length(rows)) {
    dfCols <- str_trim(str_split(rows[rowNum], coll('|'))[[1]])
    
    df[rowNum, 'ERROR_CODE']  <- as.integer(dfCols[2])
    
    for (colNum in 4:(length(dfCols) - 1)) {
      val <- ParseBdlField(dfCols[colNum], colNames[colNum - 2])
      df[rowNum, colNum - 2] <- val
    }
    
    rownames(df)[rowNum] <- dfCols[1]
  }
  
  return (df)
  
}


ParseBdlField <- function(value, fieldName) {
  val <- type.convert(value, na.strings = c("NA", "N.A."), as.is = TRUE)
  if(FALSE) {
    #does not work yet
    type <- subset(bdlFields, rownames(bdlFields) == fieldName, 'Field.Type', drop = TRUE)
    
    if(length(type) == 0) {
      type <- subset(bdlFields, toupper(str_replace_all(bdlFields$Description, " ", "_")) == fieldName, 'Field.Type', drop = TRUE)
    }
    if (length(type) > 0 && type %in% c('Date', 'Date or Time', 'Time')) {
      val <- as.Date(val)
    }
  }
  return (val)
}


#' Convert Bloomberg gethistory out file content to a list of xts objects
#' 
#' @param bdlOutContent The content string
#' @return A list containing the xts objects
#' 
#' 
#' @examples
#' #to look at an example:
#' data(getHistoryListReply)
#' getHistoryListReply
#'  
#' @seealso GetHistoryParser
#' 
#' @export
GetHistoryListParser <- function(bdlOutContent) {
  #decFile <- 'inst/extdata/gethist.out'
  #bdlOutContent <- readChar(decFile, file.info(decFile)$size)
  
  cols <- ParseGetHistoryCols(bdlOutContent)
  
  idx <- ParseGetHistoryIndex(bdlOutContent)
  resultList <- list()
  for (col in cols[-1]) {
    res <- xts(order.by = idx )
    parseRes <- ParseGetHistoryCol(col, idx, FALSE)
    res <- merge(res, parseRes)
    attr(res, "errors") <- c(attr(res, "errors"), attr(parseRes, "errors"))
    ticker <- attr(parseRes, 'ticker')
    if (ticker %in% names(resultList)) {
      resultList[[ticker]] <- merge(resultList[[ticker]], res)
      attr(resultList[[ticker]], "errors") <- c(attr(resultList[[ticker]], "errors"), attr(parseRes, "errors"))
    } else {
      resultList[[ticker]] <- res
    }
    
  }
  
  return (resultList)
  
}


#' Convert Bloomberg out file content to a single xts
#' 
#' @param bdlOutContent The content string
#' @return an xts object containing the price series
#' 
#' @examples
#' #to look at an example:
#' data(getHistoryReply)
#' getHistoryReply
#'  
#' @seealso GetHistoryListParser
#' @import xts
#' @export
GetHistoryParser <- function(bdlOutContent) {
  #decFile <- 'inst/extdata/gethist.out'
  #bdlOutContent <- readChar(decFile, file.info(decFile)$size)
  
  cols <- ParseGetHistoryCols(bdlOutContent)
  
  idx <- ParseGetHistoryIndex(bdlOutContent)
  
  res <- xts(order.by = idx )
  attr(res, "errors") <- list()
  for (col in cols[-1]) {
    parseRes <- ParseGetHistoryCol(col, idx)
    res <- merge(res, parseRes)
    if (!is.null(attr(parseRes, "errors"))) attr(res, "errors")[[tail(names(res), 1)]] <- as.character(attr(parseRes, "errors"))
  }
  
  return (res)
  
}


ParseGetHistoryCols <- function(bdlOutContent) {
  bdlOutContent <- str_replace_all(bdlOutContent, "[\r]", "")
  
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
  
  #security
  cols <- str_split(rowsStr, pattern = 'START SECURITY')[[1]]
}


ParseGetHistoryIndex <- function(bdlOutContent) {
  start <- str_locate(bdlOutContent, 'DATERANGE')[2] + 2
  dteRng <- str_sub(bdlOutContent, start, start + 16)
  dteFormat <- '%Y%m%d'
  startDte <- as.Date( str_sub(dteRng, 1, 8), dteFormat)
  endDte <- as.Date( str_sub( dteRng, 10, 17), dteFormat)
  
  idx <- seq(from = startDte, to = endDte, by = 1)
}

ParseGetHistoryCol <- function(col, idx, tickerInColName = TRUE) {
  col <- str_replace(col, "[\n]", "")
  rows <- str_split(col, '[|]')[[1]]
  
  ticker <- rows[2]
  field <- rows[3]
  errorCode <- rows[length(rows)-1]
  
  if(tickerInColName) {
    colName <- str_replace( paste0(ticker, '.', field), " ", "_")
  } else {
    colName <- field
  }
  #print(paste0("ticker ", ticker, " ", length(rows)))
  
  if (length(rows) - 4 < 5) {
    res <- xts(rep(NA, length(idx)), order.by = idx)
  } else {
    dts <- rows[seq(5, length(rows) - 4, by = 3)]
    prices <- rows[seq(6, length(rows) - 4, by = 3)]
  
    res <- xts(prices, order.by = as.Date(dts, '%m/%d/%Y'))
  }
  colnames(res) <- c(colName)  
  attr(res, 'ticker') <- ticker
  
  if (errorCode != 0) {
    if (is.null(attr(res, 'errors'))) {
      attr(res, 'errors') <- list()
    }
    attr(res, 'errors')[[field]] <- errorCode
  }
  
  return (res)
}


#' Convert Bloomberg resp file content to data.frame
#' 
#' The Response file can be used to check if there are any exceptions.
#' It does not contain the actual price data.
#' 
#' @examples
#' #to look at an example:
#' data(getSnapshotResponse)
#' getSnapshotResponse
#' 
#' @param bdlOutContent The content string
#' @return a data frame object containing the price series
#' @export
GetSnapshotResponseParser <- function(bdlOutContent) {
  response <- GetDataParser(bdlOutContent, FALSE)
  return (response)
}


#' Convert Bloomberg out file content to data.frame
#' 
#' @examples
#' #to look at an example:
#' data(getSnapshotReply)
#' getSnapshotReply
#' 
#' 
#' @param bdlOutContent The content string
#' @return an data frame object containing the price series
#' @export
GetSnapshotReplyParser <- function(bdlOutContent) {
  return (GetDataParser(bdlOutContent))
}