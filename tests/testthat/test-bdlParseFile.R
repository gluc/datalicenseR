context("bdl parse file")

test_that("BdlParseFile Simple sample1", {
  
  #browser()
  fileName <- system.file("extdata", "sample1.out", package="datalicenseR")
  bdlResponse <- readChar(fileName, file.info(fileName)$size)
  
  bdlResponseDf <- GetDataParser(bdlResponse)
  expect_equal(class(bdlResponseDf), 'data.frame')
  expect_equal(nrow(bdlResponseDf), 2)
  expect_equal(rownames(bdlResponseDf), c('IBM US Equity', 'INDU Index'))
  
  expect_equal(ncol(bdlResponseDf), 3)
  expect_equal(colnames(bdlResponseDf), c('ERROR_CODE', 'PX_LAST', 'PX_CLOSE'))
  
  expect_equal(bdlResponseDf[ , 1], c(0,0))
  expect_equal(bdlResponseDf[ , 2], c(105.23, 17840.52))
  expect_equal(bdlResponseDf[ , 3], c(105.29, 17839.62))
  
  
  
})



test_that("GetHistoryParser Multi", {
  
  
  fileName <- system.file("extdata", "historyReply1.out", package="datalicenseR")
  bdlResponse <- readChar(fileName, file.info(fileName)$size)
  
  bdlResponseXts <- GetHistoryParser(bdlResponse)
  expect_equal(class(bdlResponseXts), c('xts', 'zoo'))
  expect_equal(nrow(bdlResponseXts), 21)
  expect_equal(ncol(bdlResponseXts), 4)
  expect_equal(colnames(bdlResponseXts), c("GX1_A.00_0_R.Index.PX_OPEN",
                                           "GX1_A.00_0_R.Index.PX_LAST",
                                           "VG1_A.00_0_R.Index.PX_OPEN",
                                           "VG1_A.00_0_R.Index.PX_LAST"))
  
  expect_true(is.na(bdlResponseXts[1 , 1]))
  expect_equal(as.numeric(bdlResponseXts[21, 3]), 3276)

  
})




test_that("GetHistoryParserList Multi", {
  
  
  fileName <- system.file("extdata", "historyReply1.out", package="datalicenseR")
  bdlResponse <- readChar(fileName, file.info(fileName)$size)
  
  bdlResponse <- GetHistoryParserList(bdlResponse)
  expect_equal(class(bdlResponse), 'list')
  expect_equal(length(bdlResponse), 2)

  expect_equal(names(bdlResponse), c('GX1 A:00_0_R Index', 'VG1 A:00_0_R Index'))
  
  for (i in 1:2) {
    ci <- bdlResponse[[i]]
    expect_equal(nrow(ci), 21)
    expect_equal(ncol(ci), 2)
    expect_equal(colnames(ci), c('PX_OPEN', 'PX_LAST'))
  }
  
  gx1 <- bdlResponse[[1]]
  vg1 <- bdlResponse[[2]]
  
  
  expect_true(is.na(gx1[1 , 2]))
  expect_equal(as.numeric(gx1[21, 2]), 10152.0)
  
  expect_true(is.na(vg1[1 , 2]))
  expect_equal(as.numeric(vg1[21, 2]), 3256)
  
  
})