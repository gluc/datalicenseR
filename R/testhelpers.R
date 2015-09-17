TestDES <- function(SUNOS = FALSE, HEX_KEY = FALSE, ECB = FALSE, UUENC = FALSE, uuencFile = "", numChars = 100, lines = 10) {
  browser()
  #create a temporary file
  fileIn <- tempfile()
  #Write something into it
  fileContent <- RandomString(n = lines, length = numChars)
  writeLines(fileContent, fileIn)
  
  
  #name of the target encrypted file
  fileEnc <- paste0(fileIn, ".enc")
  
  key <- "Ab4qY9qm"
  
  #call the new method "callRDES" in the libdes.dll
  
  result <- EncryptFile(sourceFile = fileIn,
                        encryptedFile =  fileEnc,
                        key, 
                        SUNOS = SUNOS, 
                        HEX_KEY = HEX_KEY, 
                        ECB = ECB, 
                        UUENC = UUENC,
                        uuencFile = fileIn)
  
  # now decrypt
  
  fileDec <- paste0(fileIn, ".dec")
  
  
  #result <- DecryptFile(fileEnc, fileDec, key, SUNOS, HEX_KEY, ECB, UUENC)
  #read in decrypted file
  #decryptedString <- readLines(fileDec, file.info(fileDec)$size)
  
  cnt <- readChar(fileEnc, file.info(fileEnc)$size)
  
  decryptedString <- Decrypt(cnt, key = key)
  
  
  
  #check that content is the same
  expect_equal(fileContent, decryptedString)
  
  
}



RandomString <- function(n = 1, length = 12) {
  randomString <- c(1:n)                  
  for (i in 1:n)
  {
    randomString[i] <- paste(sample(c(0:9, letters, LETTERS, " ", "|", ":"),
                                    length, replace=TRUE),
                             collapse="")
  }
  return(randomString)
}
