#' Download data from Bloomberg using the getdata program.
#' 
#' This methods blocks call, until the file is available (which can be several minutes)
#' 
#' @param user The account number assigned by Bloomberg
#' @param pw The password of your Bloomberg account
#' @param key The DES decryption key of your Bloomberg account
#' @param fields A vector of fields, e.g. c('PX_LAST', 'NAME')
#' @param tickers A vector of Bloomberg tickers to be downloaded, e.g. c('SPX Index', 'IBM US Equity')
#' 
#' @import stringr
#' @export
GetData <- function(user, pw, key, fields, tickers) {
  con <- BdlConnection(user, pw, key)
  
  header <- c(FIRMNAME = user, PROGRAMNAME = 'getdata', PROGRAMFLAG = 'oneshot', COMPRESS = 'no')
  
  req <- BdlRequestBuilder(header = header,
                           fields = fields, 
                           data = tickers)
  
  fileName <- str_sub(tempfile(pattern = "getdata_", tmpdir = "", fileext = ".req"), start = 2)
  respFileName <- UploadRequest(con, req, fileName)
    
  res <- DownloadResponse(con, respFileName)
  return (res)
}