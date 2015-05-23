context("bdl parse file")

test_that("BdlParseFile Simple sample1", {
  
  #browser()
  fileName <- system.file("extdata", "sample1.out", package="datalicenseR")
  bdlResponse <- readChar(fileName, file.info(fileName)$size)
  
  bdlResponseDf <- ParseBdlResponse(bdlResponse)
  expect_equal(class(bdlResponseDf), 'data.frame')
  expect_equal(nrow(bdlResponseDf), 2)
  expect_equal(rownames(bdlResponseDf), c('IBM US Equity', 'INDU Index'))
  
  expect_equal(ncol(bdlResponseDf), 3)
  expect_equal(colnames(bdlResponseDf), c('Status', 'PX_LAST', 'PX_CLOSE'))
  
  expect_equal(bdlResponseDf[ , 1], c(0,0))
  expect_equal(bdlResponseDf[ , 2], c(105.23, 17840.52))
  expect_equal(bdlResponseDf[ , 3], c(105.29, 17839.62))
  
  
  
})