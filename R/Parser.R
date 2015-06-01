#' Convert Bloomberg out file content to data.frame
#' 
#' @param bdlOutContent The content string
#' @return a data.frame
#'  
#' @export
GetDataParser <- function(bdlOutContent) {
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
  
  rows <- str_split(rowsStr, '\n')[[1]]
  
  df <- read.table(text = "", colClasses = colClasses, col.names = colNames)
  
  for (rowNum in 1:length(rows)) {
    dfCols <- str_trim(str_split(rows[rowNum], coll('|'))[[1]])
    
    df[rowNum, 'Status']  <- as.integer(dfCols[2])
    
    for (colNum in 4:(length(dfCols) - 1)) {
      val <- type.convert(dfCols[colNum], na.strings = c("NA", "N.A."))
      if (is.factor(val)) val <- dfCols[colNum]
      df[rowNum, colNum - 2] <- val
    }
    
    rownames(df)[rowNum] <- dfCols[1]
  }
  
  return (df)
  
}



#' Convert Bloomberg out file content to a list of xts objects
#' 
#' @param bdlOutContent The content string
#' @return A list containing the xts objects
#' @export
GetHistoryParserList <- function(bdlOutContent) {
  #decFile <- 'inst/extdata/gethist.out'
  #bdlOutContent <- readChar(decFile, file.info(decFile)$size)
  
  cols <- ParseGetHistoryCols(bdlOutContent)
  
  idx <- ParseGetHistoryIndex(bdlOutContent)
  resultList <- list()
  for (col in cols[-1]) {
    res <- xts(order.by = idx )
    res <- merge(res, ParseGetHistoryCol(col, idx))
    list[attr(res, 'ticker')] <- res
  }
  
  return (resultList)
  
}


#' Convert Bloomberg out file content to xts
#' 
#' @param bdlOutContent The content string
#' @return an xts object containing the price series
#' @import xts
#' @export
GetHistoryParser <- function(bdlOutContent) {
  #decFile <- 'inst/extdata/gethist.out'
  #bdlOutContent <- readChar(decFile, file.info(decFile)$size)
  
  cols <- ParseGetHistoryCols(bdlOutContent)
  
  idx <- ParseGetHistoryIndex(bdlOutContent)
  
  res <- xts(order.by = idx )
  
  for (col in cols[-1]) {
    res <- merge(res, ParseGetHistoryCol(col, idx))
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

ParseGetHistoryCol <- function(col, idx) {
  col <- str_replace(col, "[\n]", "")
  rows <- str_split(col, '[|]')[[1]]
  
  ticker <- rows[2]
  field <- rows[3]
  colName <- str_replace( paste0(ticker, '.', field), " ", "_")
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
  return (res)
}


#' Convert Bloomberg out file content to data.frame
#' 
#' The Response file can be used to check if there are any exceptions.
#' It does not contain the actual price data.
#' 
#' @param bdlOutContent The content string
#' @return a data frame object containing the price series
#' @export
GetSnapshotResponseParser <- function(bdlOutContent) {
  return (GetDataParser(bdlOutContent))
}


#' Convert Bloomberg out file content to data.frame
#' 
#' @param bdlOutContent The content string
#' @return an data frame object containing the price series
#' @export
GetSnapshotReplyParser <- function(bdlOutContent) {
  return (GetDataParser(bdlOutContent))
}