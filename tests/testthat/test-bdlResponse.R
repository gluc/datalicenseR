context("bdl response")

test_that("BdlResponseHandle DeriveResponseFileName from request", {

  bdlRequest <- BdlRequestBuilder(header = c(PROGRAMFLAG = 'oneshot', REPLYFILENAME = 'rfile.txt'),
                    fields = c('PX_LAST', 'PX_CLOSE'), 
                    data = c('IBM US Equity'))
  
  responseFileName <- DeriveResponseFileName(bdlRequest = bdlRequest,
                                             requestFileName = NULL,
                                             responseFileName = NULL)
  
  expect_equal(responseFileName, 'rfile.txt')
  
  
})


test_that("BdlResponseHandle DeriveResponseFileName from bdlRequest 2", {
  
  bdlRequest <- BdlRequestBuilder(header = c(PROGRAMFLAG = 'oneshot', REPLYFILENAME = 'rfile.txt'),
                           fields = c('PX_LAST', 'PX_CLOSE'), 
                           data = c('IBM US Equity'))
  
  responseFileName <- DeriveResponseFileName(bdlRequest = bdlRequest,
                                             requestFileName = 'rfile1.req')
  
  expect_equal(responseFileName, 'rfile.txt')
  
  
})


test_that("BdlResponseHandle DeriveResponseFileName from requestFileName", {
  
  bdlRequest <- BdlRequestBuilder(header = c(PROGRAMFLAG = 'oneshot'),
                           fields = c('PX_LAST', 'PX_CLOSE'), 
                           data = c('IBM US Equity'))
  
  responseFileName <- DeriveResponseFileName(bdlRequest = bdlRequest,
                                             requestFileName = 'rfile1.req')
  
  expect_equal(responseFileName, 'rfile1.out')
  
  
})


test_that("BdlResponseHandle DeriveResponseFileName from response", {
  
  bdlRequest <- BdlRequestBuilder(header = c(PROGRAMFLAG = 'oneshot', REPLYFILENAME = 'rfile.txt'),
                           fields = c('PX_LAST', 'PX_CLOSE'), 
                           data = c('IBM US Equity'))
  
  responseFileName <- DeriveResponseFileName(bdlRequest = bdlRequest,
                                             responseFileName = "rfile2.txt")
  
  expect_equal(responseFileName, 'rfile2.txt')
  
  
})

test_that("BdlResponseHandle DeriveResponseFileName defaults", {
  
  bdlRequest <- BdlRequestBuilder(header = c(PROGRAMFLAG = 'oneshot', REPLYFILENAME = 'rfile.txt'),
                           fields = c('PX_LAST', 'PX_CLOSE'), 
                           data = c('IBM US Equity'))
  
  responseFileName <- DeriveResponseFileName(bdlRequest = bdlRequest)
  
  expect_equal(responseFileName, 'rfile.txt')
  
  
})


