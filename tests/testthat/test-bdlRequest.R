context("bdl request")

test_that("BdlRequest Sections", {

  req <- BdlRequestBuilder(header = c(FIRMNAME = 'dl1234', PROGRAMNAME = 'getdata', PROGRAMFLAG = 'oneshot'),
                    fields = c('PX_LAST', 'PX_CLOSE'), 
                    data = c('IBM US Equity'))
  
  reqs <- as.character(req)
  #cat(reqs)
  expect_equal(class(reqs), 'character')
  expect_true(str_detect(reqs, 'START-OF-FILE'))
  expect_true(str_detect(reqs, 'END-OF-FILE'))

  expect_true(str_detect(reqs, 'START-OF-FIELDS'))
  expect_true(str_detect(reqs, 'END-OF-FIELDS'))
  
  expect_true(str_detect(reqs, 'START-OF-DATA'))
  expect_true(str_detect(reqs, 'END-OF-DATA'))
  
  
})

