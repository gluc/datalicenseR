context("des direct")


test_that("Call Wrappers", {
  
  myText <- "Heute gibt es Tigerentensalat!"
  key = "y2KjsdE39FJ"
  
  enc <- Encrypt(myText, key)
  dec <- Decrypt(enc, key)  
  
  expect_equal(dec, myText)
})


test_that("Call directly", {
  
  myText <- "Hello baby!"
  myTextRaw <- charToRaw(myText)
  
  key = "y2KjsdE39FJ"
  
  encrypted <- .Call("rdesEncrypt", key, myTextRaw)
  
  rawToChar(encrypted)
  
  decrypted <- .Call("rdesDecrypt", key, encrypted)
  
  result <- rawToChar(decrypted)
  
  expect_equal(result, myText)
  
})