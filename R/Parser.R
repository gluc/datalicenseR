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




#' Convert Bloomberg out file content to data.frame
#' 
#' @param bdlOutContent The content string
#' @return an xts object containing the price series
#' @import xts
#' @export
GetHistoryParser <- function(bdlOutContent) {
  #decFile <- 'inst/extdata/gethist.out'
  #bdlOutContent <- readChar(decFile, file.info(decFile)$size)
  
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
  
  #result
  start <- str_locate(bdlOutContent, 'DATERANGE')[2] + 2
  dteRng <- str_sub(bdlOutContent, start, start + 16)
  dteFormat <- '%Y%m%d'
  startDte <- as.Date( str_sub(dteRng, 1, 8), dteFormat)
  endDte <- as.Date( str_sub( dteRng, 10, 17), dteFormat)
  
  idx <- seq(from = startDte, to = endDte, by = 1)
  res <- xts(order.by = idx )
  
  #security
  cols <- str_split(rowsStr, pattern = 'START SECURITY')[[1]]
  
  for (col in cols[-1]) {
    res <- merge(res, ParseGetHistoryCol(col))
  }
  
  return (res)
  
}

ParseGetHistoryCol <- function(col) {
  col <- str_replace(col, "[\n]", "")
  rows <- str_split(col, '[|]')[[1]]
  
  ticker <- rows[2]
  field <- rows[3]
  colName <- str_replace( paste0(ticker, '.', field), " ", "_")
  
  dts <- rows[seq(5, length(rows) - 4, by = 3)]
  prices <- rows[seq(6, length(rows) - 4, by = 3)]
  
  res <- xts(prices, order.by = as.Date(dts, '%m/%d/%Y'))
  colnames(res) <- c(colName)
  return (res)
}