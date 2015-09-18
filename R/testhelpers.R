
DESRounttrip <- function(fileContent, SUNOS = FALSE, HEX_KEY = FALSE, ECB = FALSE, UUENC = FALSE, uuencFile = "") {
  fileIn <- tempfile()
  #Write something into it
  
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
                        uuencFileName = fileIn)
  
  # now decrypt
  
  fileDec <- paste0(fileIn, ".dec")
  
  
  result <- DecryptFile(fileEnc, fileDec, key, SUNOS, HEX_KEY, ECB, UUENC)
  #read in decrypted file
  #decryptedString <- readChar(fileDec, file.info(fileDec)$size)
  
  decryptedString <- readLines(fileDec)
  #cnt <- readChar(fileEnc, file.info(fileEnc)$size)
  
  #decryptedString <- Decrypt(cnt, key = key)
  
  return( decryptedString )
  
}


TestDES <- function(SUNOS = FALSE, HEX_KEY = FALSE, ECB = FALSE, UUENC = FALSE, uuencFile = "", numChars = 100, lines = 10) {
  
  fileContent <- RandomString(n = lines, length = numChars)
  #create a temporary file
  
  res <- DESRounttrip(fileContent)
  expect_equal(fileContent, res)
  
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
