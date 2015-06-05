#' Download data from Bloomberg using the getdata program.
#' 
#' This methods blocks call, until the file is available (which can be several minutes)
#' 
#' @param user The account number assigned by Bloomberg
#' @param pw The password of your Bloomberg account
#' @param key The DES decryption key of your Bloomberg account
#' @param fields A vector of fields, e.g. c('PX_LAST', 'NAME')
#' @param tickers A vector of Bloomberg tickers to be downloaded, e.g. c('SPX Index', 'IBM US Equity')
#' @param sync If TRUE, the call does not return until the response file is available, or if a timeout occurs
#' @param parser The parser used to convert the file into an R object
#' @param verbose Prints output if TRUE
#' 
#' @return A list containing the downloaded data. If you use the default parser, then a data.frame 
#' is returned as the response object.
#' 
#' @import stringr
#' @export
GetData <- function(user, pw, key, 
                    fields, 
                    tickers,
                    sync = TRUE,
                    parser = GetDataParser, 
                    verbose = FALSE) {
  con <- BdlConnection(user, pw, key)
  res <- list()
  res$connection <- con
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
  respFileName <- UploadRequest(con, req, fileName, verbose = verbose)
  
  res$requestFileName <- fileName
  res$replyFileName <- respFileName
  
  callback <- function(parser = parser, verbose = verbose) {
    TryGetBdlData(con, respFileName, parser, verbose = verbose)
  }
  
  res$GetReply <- callback
  
  if (sync) {
    response <- DownloadResponse(con, respFileName, parser, verbose = verbose)
    res$success <- !is.null(response)  
  }
  res$reply <- response
  
  
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
#' @param sync If TRUE, then the call will not return until the file is available, or until it times out
#' @param parser The parser used to convert the file into an R object
#' @param verbose Prints output if TRUE
#' 
#' @return A list containing the downloaded data. If you use the default parser, then an xts object
#' is returned.
#' 
#' @import stringr
#' @export
GetHistory <- function(user, pw, key, 
                       fields, tickers, 
                       fromDate, toDate = Sys.Date(),  
                       sync = TRUE,
                       parser = GetHistoryParser,
                       verbose = FALSE) {
  res <- list()
  con <- BdlConnection(user, pw, key)
  res$connection <- con
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
  respFileName <- UploadRequest(con, req, fileName, verbose = verbose)
  
  res$requestFileName <- fileName
  res$replyFileName <- respFileName

  callback <- function(parser = parser, verbose = verbose) {
    TryGetBdlData(con, respFileName, parser, verbose = verbose)
  }
  
  res$GetReply <- callback
  
  if (sync) {
    response <- DownloadResponse(con, respFileName, parser, verbose = verbose)
    res$success <- !is.null(response)  
  }
  
  res$reply <- response

  return (res)
}



#' Download data from Bloomberg using the getsnap program.
#' 
#' The program getsnap returns two files: one response file containing some errorcodes and masterdate.
#' It can be used to check if the request was OK.
#' Some time after the snaptime, the reply file is made available. It contains the actual prices.
#' See \code{sync} for 
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
#' @param sync If TRUE, the call does not return until the response file is available, or if a timeout occurs
#' @param responseParser The parser used to convert the response file into an R object
#' @param replyParser The parser used to convert the reply file into an R object
#' @param verbose Prints output if TRUE
#' 
#' @return A list containing the response file, as well as a callback to fetch the reply.
#' 
#' @import stringr
#' @export
GetSnapshot <- function(user, pw, key, 
                        tickers, 
                        snaptime,
                        delayLimit = 3,
                        sync = TRUE,
                        responseParser = GetSnapshotResponseParser,
                        replyParser = GetSnapshotReplyParser,
                        verbose = FALSE) {
  res <- list()
  
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
  replyFileName <- UploadRequest(con, req, fileName, verbose = verbose)
  
  responseFileName <- paste0(fileName, '.', 'resp')
  
  callback <- function(responseParser = responseParser, verbose = verbose) {
    TryGetBdlData(con, responseFileName, responseParser, verbose = verbose)
  }
  res$GetRespone <- callback
  if (sync) {
    response <- DownloadResponse(con, responseFileName, responseParser, verbose = verbose)
  }
  
  res$response <- response
  res$responseFileName <- respFileName
  res$connection <- con
  res$requestFileName <- fileName
  res$replyFileName <- replyFileName
  res$success <- TRUE
  if (is.null(response)) {
    res$success <- FALSE
    warning("Could not download response file!")
    return (res)
  } else {
    if (any(response$ERROR_CODE != 0)) {
      warning("Bloomberg says: Your snapshot request containes some errors!", immediate. = TRUE)
    }
  }
  
  callback <- function(replyParser = replyParser) {
    TryGetBdlData(con, replyFileName, replyParser, verbose = verbose)
  }
  res$GetReply <- callback
  
  if (sync) {
    
    for (x in 1:as.integer(90)) {
      if(verbose) cat('.')
      Sys.sleep(10)
    }
    if (verbose) cat('\r\n')
    reply <- DownloadResponse(con, replyFileName, replyParser, pollFrequencySec = 300, timeoutMin = 120, verbose = verbose)
    res$reply <- reply
    res$success <- !is.null(reply)
  }
  
  
  
  return (res)
  
}