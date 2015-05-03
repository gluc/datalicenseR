context("bdl request")

test_that("BdlRequest Sections", {

  req <- BdlRequest(firmName = 'dl1234',
                           programName = 'getdata',
                           header = c(PROGRAMFLAG = 'oneshot'),
                           fields = c('PX_LAST', 'PX_CLOSE'), 
                           instruments = c('IBM US Equity'))
  
  reqs <- print(req)
  #cat(reqs)
  expect_equal(class(reqs), 'character')
  expect_true(str_detect(reqs, 'START-OF-FILE'))
  expect_true(str_detect(reqs, 'END-OF-FILE'))

  expect_true(str_detect(reqs, 'START-OF-FIELDS'))
  expect_true(str_detect(reqs, 'END-OF-FIELDS'))
  
  expect_true(str_detect(reqs, 'START-OF-DATA'))
  expect_true(str_detect(reqs, 'END-OF-DATA'))
  
  
})


test_that("BdlRequest Custom Fields", {
  
  header <- c(PROGRAMFLAG = 'oneshot',
              SECMASTER = 'yes')
  
  req <- BdlRequest(firmName = 'dl1234', 
                           programName = 'getdata',
                           fields = c('PX_LAST', 'PX_CLOSE'), 
                           instruments = c('IBM US Equity'),
                           header = header)
  
  reqs <- print(req)
  #cat(reqs)
  expect_equal(class(reqs), 'character')
  expect_true(str_detect(reqs, 'PROGRAMNAME=getdata'))
  expect_true(str_detect(reqs, 'PROGRAMFLAG=oneshot'))
  expect_true(str_detect(reqs, 'SECMASTER=yes'))
})