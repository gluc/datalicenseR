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
              FUNDAMENTALS = 'yes',
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



#' Download data from Bloomberg using the getsnap program.
#' 
#' The program getsnap returns two files: one response file containing some errorcodes and masterdate.
#' It can be used to check if the request was OK.
#' Some time after the snaptime, the reply file is made available. It contains the actual prices.
#' 
#' @param user The account number assigned by Bloomberg
#' @param pw The password of your Bloomberg account
#' @param key The DES decryption key of your Bloomberg account
#' @param tickers A vector of Bloomberg tickers to be downloaded, e.g. c('SPX Index', 'IBM US Equity')
#' @param snaptime A character string, containing the snapshot time, in the format HHMM, and only
#' half-hours are allowed. Also, the timezone of the snapshot is your Bloomberg region (either
#' NY, LONDON, or TOKYO), e.g. 0930
#' @param delayLimit An integer, representing the maximum time Bloomberg should wait on the next price after
#' the snaptime.
#' @param responseParser The parser used to convert the response file into an R object
#' @param replyParser The parser used to convert the reply file into an R object
#' 
#' @return A list containing the response file, as well as a callback to fetch the reply.
#' 
#' @import stringr
#' @export
GetSnapshot <- function(user, pw, key, 
                        tickers, 
                        snaptime,
                        delayLimit = 3,
                        sync = FALSE,
                        responseParser = GetSnapshotResponseParser,
                        replyParser = GetSnapshotReplyParser) {
  
  con <- BdlConnection(user, pw, key)
  header <- c(FIRMNAME = user, 
              PROGRAMNAME = 'getsnap', 
              SNAPTIME = snaptime,
              DELAY_LIMIT = delayLimit,
              COLUMNHEADER = 'yes')
  
  req <- BdlRequestBuilder(header = header,
                           fields = vector(mode = "character"), 
                           data = tickers)
  
  fileName <- str_sub(tempfile(pattern = "getsnap_", tmpdir = "", fileext = ".req"), start = 2)
  replyFileName <- UploadRequest(con, req, fileName)
  
  if(FALSE) {
    # DEBUG
    fileName <- 'getsnap_2f854cb734ca.req'
    replyFileName <- 'getsnap_2f854cb734ca.req.out'
    responseParser <- GetSnapshotResponseParser
    replyParser <- GetSnapshotReplyParser
  }
  responseFileName <- paste0(fileName, '.', 'resp')
  response <- DownloadResponse(con, responseFileName, responseParser)
  
  callback <- function() {
    TryGetBdlData(con, replyFileName, replyParser)
  }
  res <- list()
  if (sync) {
    for (x in 1:as.integer(90)) {
      cat('.')
      Sys.sleep(10)
    }
    reply <- DownloadResponse(con, replyFileName, replyParser, pollFrequencySec = 300, timeoutMin = 120)
    res$reply <- reply  
  }
  
  
  res$response <- response
  res$GetReply <- callback
  res$connection <- con
  res$replyFileName <- replyFileName
  
  return (res)
  
}