#' Download data from Bloomberg using the getdata program.
#' 
#' By default, this methods blocks call, until the file is available (which can be several minutes) (see \code{sync} parameter
#' for details)
#' 
#' @param con The \code{\link{BdlConnection}} object containing the Bloomberg credentials
#' @param fields A vector of fields, e.g. c('PX_LAST', 'NAME')
#' @param tickers A vector of Bloomberg tickers to be downloaded, e.g. c('SPX Index', 'IBM US Equity')
#' @param sync If TRUE, the call does not return until the response file is available, or if a timeout occurs
#' @param parser The parser used to convert the file into an R object. See \code{\link{GetDataParser}} for details on
#' the default parser.
#' @param verbose Prints output if TRUE
#' 
#' @return A list containing the downloaded data. If you use the default parser, then a dataframe
#' is returned. More precisely, the list contains the following items:
#' \describe{
#'   \item{\code{connection}}{The \code{\link{BdlConnection}} object used to connect ot Bloomberg (R). This contains the credentials necessary.}
#'   \item{\code{requestTime}}{The time at which the request was sent to Bloomberg (R)}
#'   \item{\code{requestFileName}}{The name of the file that was uploaded to Bloomberg (R)}
#'   \item{\code{replyTime}}{The time at which the reply was downloaded from Bloomberg (R). Applies only if \code{sync == TRUE}}
#'   \item{\code{replyFileName}}{The name of the file that was uploaded to Bloomberg (R)}
#'   \item{\code{GetReply(parser., verbose.)}}{A callback function to download the reply file. The paramaeters are optional and allow
#'   you to overwrite their counterparts given in the \code{GetSnapshot} call.}
#'   \item{\code{success}}{\code{TRUE} if the method call was successful. In case of \code{sync == TRUE}, this means that a reply file 
#'   was successfully downloaded and parsed. Otherwise, it means that the request file was successfully uploaded. }
#' }
#' 
#' @examples
#' \dontrun{
#' 
#' con <- BdlConnection(user = 'dl111111', 
#'                      pw = 'XvH,gE2A', 
#'                      key = '3xzZl0yA')
#' 
#' 
#' #######################
#' #Sync call
#' 
#' data <- GetData(con,
#'                 fields = c('PX_LAST', 'NAME', 'CRNCY'), 
#'                 tickers = c('SPX Index', 'SMI Index', 'IBM US Equity', '120421Q FP Equity'))
#'                       
#' #the call blocks, and after 5 minutes or so, you get the reply
#' if(data$success) {
#'   data$reply
#'   #or extract a single value
#'   data$reply['SPX Index', 'PX_LAST']
#' }
#' 
#' #If your data does not make sense, or you have other problems, you can re-download again using
#' #any other parser, and overwriting the verbose parameter:
#' cat(data$GetReply(verbose. = TRUE, parser. = function(x) x))
#' 
#' #######################
#' #Async call
#' 
#' data <- GetData(con,
#'                 fields = c('PX_LAST', 'NAME', 'CRNCY'), 
#'                 tickers = c('SPX Index', 'SMI Index', 'IBM US Equity', '120421Q FP Equity'),
#'                 sync = FALSE)
#' 
#' #the method returns after uploading. 5 minutes later, you can request to download
#' #the reply using the callback:
#' reply <- data$GetReply()
#' if(!is.null(reply)) {
#'   #do something with the data
#' }
#' 
#' 
#' }
#' 
#' 
#' @seealso GetHistory, GetSnapshot
#' @import stringr
#' @export
GetData <- function(con,
                    fields, 
                    tickers,
                    sync = TRUE,
                    parser = GetDataParser, 
                    verbose = FALSE) {
  res <- list()
  res$connection <- con
  header <- c(FIRMNAME = con$user, 
              PROGRAMNAME = 'getdata', 
              PROGRAMFLAG = 'oneshot', 
              PRICING = 'yes', 
              FUNDAMENTALS = 'yes',
              COMPRESS = 'no')
  
  req <- BdlRequestBuilder(header = header,
                           fields = fields, 
                           data = tickers)
    
  fileName <- str_sub(tempfile(pattern = "getdata_", tmpdir = "", fileext = ".req"), start = 2)
  res$requestTime <- Sys.time()
  respFileName <- UploadRequest(con, req, fileName, verbose = verbose)
  
  res$requestFileName <- fileName
  res$replyFileName <- respFileName
  
  callback <- function(parser. = parser, verbose. = verbose) {
    TryGetBdlData(con, respFileName, parser., verbose = verbose.)
  }
  
  res$GetReply <- callback
  
  if (sync) {
    response <- DownloadResponse(con, respFileName, parser, verbose = verbose)
    res$reply <- response
    res$replyTime <- Sys.time()
    res$success <- !is.null(response)  
  }
  
  
  
  return (res)
}


#' Download data from Bloomberg using the gethistory program.
#' 
#' By default, this methods blocks call, until the file is available (which can be several minutes) (see \code{sync} parameter
#' for details)
#' 
#' @param con The \code{\link{BdlConnection}} object containing the Bloomberg credentials
#' @param fields A vector of fields, e.g. c('PX_LAST', 'NAME')
#' @param tickers A vector of Bloomberg tickers to be downloaded, e.g. c('SPX Index', 'IBM US Equity')
#' @param fromDate The start date in your request
#' @param toDate The end date in your request. Per default, this is today.
#' @param sync If TRUE, then the call will not return until the file is available, or until it times out
#' @param parser The parser used to convert the file into an R object. The default parser is the \code{\link{GetHistoryParser}}.
#' Another parser you might prefer is the \code{\link{GetHistoryListParser}}.
#' @param verbose Prints output if TRUE
#' 
#' @return A list containing the downloaded data. If you use the default parser, then an xts object
#' is returned. More precisely, the list contains the following items:
#' \describe{
#'   \item{\code{connection}}{The \code{\link{BdlConnection}} object used to connect ot Bloomberg (R). This contains the credentials necessary.}
#'   \item{\code{requestTime}}{The time at which the request was sent to Bloomberg (R)}
#'   \item{\code{requestFileName}}{The name of the file that was uploaded to Bloomberg (R)}
#'   \item{\code{replyTime}}{The time at which the reply was downloaded from Bloomberg (R). Applies only if \code{sync == TRUE}}
#'   \item{\code{replyFileName}}{The name of the file that was uploaded to Bloomberg (R)}
#'   \item{\code{GetReply(parser., verbose.)}}{A callback function to download the reply file. The paramaeters are optional and allow
#'   you to overwrite their counterparts given in the \code{GetSnapshot} call.}
#'   \item{\code{success}}{\code{TRUE} if the method call was successful. In case of \code{sync == TRUE}, this means that a reply file 
#'   was successfully downloaded and parsed. Otherwise, it means that the request file was successfully uploaded. }
#' }
#' 
#' @examples
#' \dontrun{
#' 
#' con <- BdlConnection(user = 'dl111111', 
#'                      pw = 'XvH,gE2A', 
#'                      key = '3xzZl0yA')
#' 
#' 
#' #######################
#' #Sync call
#' 
#' history <- GetHistory(con, 
#'                       fields = c('PX_OPEN', 'PX_HIGH', 'PX_LOW'), 
#'                       tickers = c('SPX Index', 'SMI Index', 'IBM US Equity', '120421Q FP Equity'),
#'                       fromDate = "2015-05-01", toDate = "2015-05-08",
#'                       verbose = TRUE)
#'                       
#' #the call blocks, and after 5 minutes or so, you get the reply
#' if(history$success) {
#'   history$reply
#'   #or extract single column:
#'   history$reply$SPX_Index.PX_HIGH
#' }
#' 
#' #To see the downloaded file content:
#' cat(history$response)
#' 
#' #Or, if you prefer a list with one xts object per security:
#' history <- GetHistory(con, 
#'                       fields = c('PX_OPEN', 'PX_HIGH', 'PX_LOW'), 
#'                       tickers = c('SPX Index', 'SMI Index', 'IBM US Equity', '120421Q FP Equity'),
#'                       fromDate = "2015-05-01", toDate = "2015-05-08",
#'                       parser = GetHistoryListParser,
#'                       verbose = TRUE)
#'                       
#' 
#' #######################
#' #Async call
#' 
#' history <- GetHistory(con, 
#'                       fields = c('PX_OPEN', 'PX_HIGH', 'PX_LOW'), 
#'                       tickers = c('SPX Index', 'SMI Index', 'IBM US Equity', '120421Q FP Equity'),
#'                       fromDate = "2015-05-01", toDate = "2015-05-08",
#'                       sync = FALSE,
#'                       verbose = TRUE)
#' 
#' #the method returns after uploading. 5 minutes later, you can request to download
#' #the reply using the callback:
#' reply <- history$GetReply()
#' if(!is.null(reply)) {
#'   #do something with the data
#' }
#' 
#' 
#' 
#' 
#' ######################
#' #Re-download an existing file
#' 
#' TryGetBdlData(con, history$replyFileName)
#' 
#' 
#' @seealso GetData, GetSnapshot
#' @import stringr
#' @export
GetHistory <- function(con, 
                       fields, tickers, 
                       fromDate, toDate = Sys.Date(),  
                       sync = TRUE,
                       parser = GetHistoryParser,
                       verbose = FALSE) {
  res <- list()
  res$connection <- con
  res$success <- TRUE
  fmt <- '%Y%m%d'
  dtrng <- paste0(format(as.Date(fromDate), fmt), '|', format(as.Date(toDate), fmt))
  header <- c(FIRMNAME = con$user, 
              PROGRAMNAME = 'gethistory', 
              DATERANGE = dtrng,              
              COMPRESS = 'yes')
  
  req <- BdlRequestBuilder(header = header,
                           fields = fields, 
                           data = tickers)
  
  fileName <- str_sub(tempfile(pattern = "gethist_", tmpdir = "", fileext = ".req"), start = 2)
  res$requestTime <- Sys.time()
  respFileName <- UploadRequest(con, req, fileName, verbose = verbose)
  
  res$requestFileName <- fileName
  res$replyFileName <- respFileName

  callback <- function(parser. = parser, verbose. = verbose) {
    TryGetBdlData(con, respFileName, parser., verbose = verbose.)
  }
  
  res$GetReply <- callback
  
  if (sync) {
    res$response <- DownloadResponse(con, respFileName, identity, verbose = verbose)
    res$success <- !is.null(res$response) 
    res$replyTime <- Sys.time()
    res$reply <- parser(res$response)
  }

  return (res)
}



#' Download data from Bloomberg using the getsnap program.
#' 
#' The program getsnap returns two files: one response file containing some errorcodes and masterdate.
#' It can be used to check if the request was OK.
#' Some time after the snaptime, the reply file is made available. It contains the actual prices.
#' By default, this methods blocks call, until the file is available (which can be several hours, depending on your \code{snaptime}) (see \code{sync} parameter
#' for details)
#' 
#' @param con The \code{\link{BdlConnection}} object containing the Bloomberg credentials
#' @param tickers A vector of Bloomberg tickers to be downloaded, e.g. c('SPX Index', 'IBM US Equity')
#' @param snaptime A character string, containing the snapshot time, in the format HHMM, and only
#' half-hours are allowed (e.g. 0900 or 0930, but not 0915). Also, the timezone of the snapshot is your Bloomberg region (either
#' NY, LONDON, or TOKYO), e.g. 0930
#' @param delayLimit An integer, representing the maximum time Bloomberg should wait on the next price after
#' the snaptime. This is useful if you have delayed prices on some of the markets requested.
#' @param sync If TRUE, the call does not return until the response file is available, or if a timeout occurs
#' @param responseParser The parser used to convert the response file into an R object
#' @param replyParser The parser used to convert the reply file into an R object
#' @param timeoutMin The timeout of the replyFile. This only applies if \code{sync == TRUE}.
#' @param verbose Prints output if TRUE
#' 
#' @return A list containing the response file, as well as a callback to fetch the reply. More precisely, the list contains
#' the following items:
#' \describe{
#'   \item{\code{connection}}{The \code{\link{BdlConnection}} object used to connect ot Bloomberg (R). This contains the credentials necessary.}
#'   \item{\code{requestTime}}{The time at which the request was sent to Bloomberg (R)}
#'   \item{\code{requestFileName}}{The name of the file that was uploaded to Bloomberg (R)}
#'   \item{\code{responseTime}}{The time at which the response file was downloaded from Bloomberg (R). Applies only if \code{sync == TRUE}}
#'   \item{\code{responseFileName}}{The name of the response file containing the error codes.}
#'   \item{\code{GetResponse(responseParser., verbose.)}}{A callback function to download the response file. The paramaeters are optional and allow
#'   you to overwrite their counterparts given in the \code{GetSnapshot} call.}
#'   \item{\code{replyTime}}{The time at which the reply was downloaded from Bloomberg (R). Applies only if \code{sync == TRUE}}
#'   \item{\code{replyFileName}}{The name of the file that was uploaded to Bloomberg (R)}
#'   \item{\code{GetReply(replyParser., verbose.)}}{A callback function to download the reply file. The paramaeters are optional and allow
#'   you to overwrite their counterparts given in the \code{GetSnapshot} call.}
#'   \item{\code{success}}{\code{TRUE} if the method call was successful. In case of \code{sync == TRUE}, this means that a reply file 
#'   was successfully downloaded and parsed. Otherwise, it means that the request file was successfully uploaded. }
#' 
#' }
#' 
#' @examples
#' \dontrun{
#' 
#' con <- BdlConnection(user = 'dl111111', 
#'                      pw = 'XvH,gE2A', 
#'                      key = '3xzZl0yA')
#' 
#' #######################
#' # Sync call
#'    
#' #this blocks, and waits until the files are available, or until a timeout occurs.
#' #Remember that the snaptime is in the timezone of your Bloomberg region (London, NY, Tokyo),
#' #and that only full and have hours are allowed (e.g. 0900 or 0930)
#' snapshot <- GetSnapshot(con,
#'                         tickers = c('SPX Index', 'SMI Index'),
#'                         snaptime = "0930",
#'                         delayLimit = 15,
#'                         sync = TRUE)
#'                        
#' #after long time waiting                        
#' if(snapshot$success) {
#'   snapshot$reply
#'   reply['SPX Index', 'OPEN_PRICE']
#' }
#' 
#' #You can re-download both the reply and the response file:
#' snapshot$GetResponse()
#' 
#' if you have problems with the download, you can overwrite the verbose parameter:
#' snapshot$GetResponse(verbose. = TRUE)
#' 
#' #or you might use a different parser, to see the file in raw text:
#' cat(snapshot$GetResponse(responseParser. = function(x) x))
#' 
#' 
#' #######################
#' #Async call
#' 
#' #this returns immediately after uploading the request. You 
#' #can then use the callback handles for downloading the results.
#' snapshot <- GetSnapshot(con, 
#'                         tickers = c('SPX Index', 'SMI Index'),
#'                         snaptime = "0930",
#'                         delayLimit = 15)
#'                         
#' #after 3 minutes or so, you can check if your request was ok:
#' response <- snapshot$GetResponse()
#' 
#' #check if there were any errors in the response:
#' any(response$ERROR_CODE != 0)
#' 
#' #get the error message to identify the problem:
#' GetSnapshotErrorMessage(response$ERROR_CODE)
#' 
#' #when you expect the reply to be available:
#' reply <- snapshot$GetReply()
#' if(!is.null(reply)) reply['SPX Index', 'OPEN_PRICE']
#' 
#' }
#' 
#' @seealso GetData, GetHistory
#' 
#' @import stringr
#' @export
GetSnapshot <- function(con, 
                        tickers, 
                        snaptime,
                        delayLimit = 3,
                        sync = TRUE,
                        responseParser = GetSnapshotResponseParser,
                        replyParser = GetSnapshotReplyParser,
                        timeoutMin = 120,
                        verbose = FALSE) {
  res <- list()
  res$success <- TRUE
  res$connection <- con
  
  header <- c(FIRMNAME = con$user, 
              PROGRAMNAME = 'getsnap', 
              SNAPTIME = snaptime,
              DELAY_LIMIT = delayLimit,
              COLUMNHEADER = 'yes')
  
  req <- BdlRequestBuilder(header = header,
                           fields = vector(mode = "character"), 
                           data = tickers)
  
  fileName <- str_sub(tempfile(pattern = "getsnap_", tmpdir = "", fileext = ".req"), start = 2)
  res$requestTime <- Sys.time()
  replyFileName <- UploadRequest(con, req, fileName, verbose = verbose)
  
  responseFileName <- paste0(fileName, '.', 'resp')
  
  callback <- function(responseParser. = responseParser, verbose. = verbose) {
    TryGetBdlData(con, responseFileName, responseParser., verbose = verbose.)
  }
  res$GetResponse <- callback

  res$responseFileName <- responseFileName
  res$requestFileName <- fileName
  res$replyFileName <- replyFileName
  
  
  if (sync) {
    response <- DownloadResponse(con, responseFileName, responseParser, timeoutMin = 5, verbose = verbose)
    res$responseTime <- Sys.time()
    res$response <- response
    if (is.null(response)) {
      res$success <- FALSE
      warning("Could not download response file!")
      return (res)
    } else {
      if (any(response$ERROR_CODE != 0)) {
        warning("Bloomberg says: Your snapshot request containes some errors!", immediate. = TRUE)
      }
    }
  }
  
  callback <- function(replyParser. = replyParser, verbose. = verbose) {
    TryGetBdlData(con, replyFileName, replyParser., verbose = verbose.)
  }
  res$GetReply <- callback
  
  if (sync) {
    
    for (x in 1:as.integer(90)) {
      if(verbose) cat('.')
      Sys.sleep(10)
    }
    if (verbose) cat('\r\n')
    reply <- DownloadResponse(con, replyFileName, replyParser, pollFrequencySec = 300, timeoutMin = timeoutMin, verbose = verbose)
    res$reply <- reply
    res$replyTime <- Sys.time()
    res$success <- !is.null(reply)
  }
  
  
  
  return (res)
  
}