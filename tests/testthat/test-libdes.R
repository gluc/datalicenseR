

context("des")




test_that("roundtrip wrapper", {
  
  TestDES()

})

test_that("roundtrip long UUENC", {
  
  TestDES(UUENC = TRUE, numChars = 49134, lines = 1)
  
})


test_that("roundtrip longer UUENC", {
  
  TestDES(UUENC = TRUE, numChars = 49135, lines = 1)  
  
})


test_that("roundtrip long", {
  
  TestDES(UUENC = FALSE, numChars = 49135, lines = 1)
  
})




test_that("roundtrip wrapper ECB", {
  
  TestDES(ECB = TRUE)
  
})

test_that("roundtrip wrapper UUENC", {
  
  TestDES(UUENC = TRUE, uuencFile = "test.txt")
  #we had the problem that 2 subsequent calls crashed R
  TestDES(UUENC = TRUE, uuencFile = "test.txt")
  
})


test_that("roundtrip wrapper 3DES", {
  
  #create a temporary file
  fileIn <- tempfile()
  #Write something into it
  fileContent <- "Hello baby"
  cat(fileContent, file = fileIn)
  
  
  #name of the target encrypted file
  fileEnc <- paste0(fileIn, ".enc")
  
  key <- "821768A87C13467BDF68F375353CC80B55C1237F7AE98C879EE9ACFE"
  
  #call the new method "callRDES" in the libdes.dll
  
  result <- EncryptFileTripleDES(fileIn, fileEnc, key)
  
  # now decrypt
  
  fileDec <- paste0(fileIn, ".dec")
  
  
  result <- DecryptFileTripleDES(fileEnc, fileDec, key)
  
  #read in decrypted file
  helloBaby <- readChar(fileDec, file.info(fileDec)$size)
  
  #check that content is the same
  expect_equal(fileContent, helloBaby)
  
})






