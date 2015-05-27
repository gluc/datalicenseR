#' Convert Bloomberg out file content to data.frame
#' 
#' @param bdlOutContent The content string
#' @return a data.frame
#'  
#' @export
GetDataParser <- function(bdlOutContent) {
  bdlOutContent <- str_replace(bdlOutContent, "/r", "")
  
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
#' @return a data.frame
#'  
#' @export
GetHistoryParser <- function(bdlOutContent) {
  
  return (paste0("NOT YET IMPLEMENTED!", '\n', bdlOutContent))
  
}

