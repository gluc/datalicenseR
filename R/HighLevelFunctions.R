#' Download data from Bloomberg using the getdata program.
#' 
#' This methods blocks call, until the file is available (which can be several minutes)
#' 
#' @param user The account number assigned by Bloomberg
#' @param pw The password of your Bloomberg account
#' @param key The DES decryption key of your Bloomberg account
#' @param fields A vector of fields, e.g. c('PX_LAST', 'NAME')
#' @param tickers A vector of Bloomberg tickers to be downloaded, e.g. c('SPX Index', 'IBM US Equity')
#' @param parser The parser used to convert the file into an R object
#' 
#' @return An R object containing the downloaded data. If you use the default parser, then a data.frame 
#' is returned.
#' 
#' @import stringr
#' @export
GetData <- function(user, pw, key, fields, tickers, parser = GetDataParser) {
  con <- BdlConnection(user, pw, key)
  
  header <- c(FIRMNAME = user, 
              PROGRAMNAME = 'getdata', 
              PROGRAMFLAG = 'oneshot', 
              PRICING = 'yes', 
              COMPRESS = 'no')
  
  req <- BdlRequestBuilder(header = header,
                           fields = fields, 
                           data = tickers)
  
  fileName <- str_sub(tempfile(pattern = "getdata_", tmpdir = "", fileext = ".req"), start = 2)
  respFileName <- UploadRequest(con, req, fileName)
    
  res <- DownloadResponse(con, respFileName, parser)
  return (res)
}


#' Download data from Bloomberg using the gethistory program.
#' 
#' This methods blocks call, until the file is available (which can be several minutes)
#' 
#' @param user The account number assigned by Bloomberg
#' @param pw The password of your Bloomberg account
#' @param key The DES decryption key of your Bloomberg account
#' @param fields A vector of fields, e.g. c('PX_LAST', 'NAME')
#' @param tickers A vector of Bloomberg tickers to be downloaded, e.g. c('SPX Index', 'IBM US Equity')
#' @param fromDate The start date in your request
#' @param toDate The end date in your request
#' @param parser The parser used to convert the file into an R object
#' 
#' @return An R object containing the downloaded data. If you use the default parser, then an xts object
#' is returned.
#' 
#' @import stringr
#' @export
GetHistory <- function(user, pw, key, 
                       fields, tickers, 
                       fromDate, toDate = Sys.Date(),  
                       parser = GetHistoryParser) {
  
  con <- BdlConnection(user, pw, key)
  fmt <- '%Y%m%d'
  dtrng <- paste0(format(as.Date(fromDate), fmt), '|', format(as.Date(toDate), fmt))
  header <- c(FIRMNAME = user, 
              PROGRAMNAME = 'gethistory', 
              DATERANGE = dtrng,              
              COMPRESS = 'yes')
  
  req <- BdlRequestBuilder(header = header,
                           fields = fields, 
                           data = tickers)
  
  fileName <- str_sub(tempfile(pattern = "gethist_", tmpdir = "", fileext = ".req"), start = 2)
  respFileName <- UploadRequest(con, req, fileName)
  
  res <- DownloadResponse(con, respFileName, parser)
  return (res)
}