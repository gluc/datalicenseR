
#' Encrypt a file with the DES algorithm
#' 
#' @param sourceFile The path to the file that should be encrypted
#' @param encryptedFile The path to the target file
#' @param key The key (either a character string of arbitrary length, or a 56bit HEX key)
#' @param SUNOS Use SUNOS?
#' @param HEX_KEY Should the key be interpreted as HEX?
#' @param ECB Use ECB?
#' @param UUENC Whether to use UU Encoding
#' @param uuencFileName The original file name to add as a header to the Uuencoded file
#'
#' @export
EncryptFile <- function(sourceFile, encryptedFile, key, SUNOS = FALSE, HEX_KEY = FALSE, ECB = FALSE, UUENC = FALSE, uuencFileName = "") {
  
  opts <- GetOptions(ENCRYPT = TRUE, 
                     SUNOS = SUNOS, 
                     HEX_KEY = HEX_KEY,
                     THREEDES = FALSE, 
                     ECB = ECB, 
                     UUENC = UUENC)
    
  .C( "callRDES", opts, key, sourceFile, encryptedFile, "", uuencFileName )
}


#' Decrypt a file with the DES algorithm
#' 
#' @param sourceFile The path to the file that should be decrypted
#' @param decryptedFile The path to the target file
#' @param key The key (either a character string of arbitrary length, or a 56bit HEX key)
#' @param SUNOS Use SUNOS?
#' @param HEX_KEY Should the key be interpreted as HEX?
#' @param ECB Use ECB?
#' @param UUENC Whether to use UU Encoding
#'
#' @export
DecryptFile <- function(sourceFile, decryptedFile, key, SUNOS = FALSE, HEX_KEY = FALSE, ECB = FALSE, UUENC = FALSE) {
  
  opts <- GetOptions(ENCRYPT = FALSE, 
                     SUNOS = SUNOS, 
                     HEX_KEY = HEX_KEY,
                     THREEDES = FALSE, 
                     ECB = ECB, 
                     UUENC = UUENC)
  
  .C( "callRDES", opts, key, sourceFile, decryptedFile, "", "")
}


#' Encrypt a file with the Triple DES algorithm
#'
#' @param sourceFile The path the the file that should be encrypted
#' @param encryptedFile The path to the target file
#' @param key The 56bit HEX key
#' @param ECB Use ECB?
#' @param UUENC Whether the files should be UU encoded
#' @param uuencFileName The original file name to add as a header to the Uuencoded file
#'
#' @export
EncryptFileTripleDES <- function(sourceFile, encryptedFile, key, ECB = FALSE, UUENC = FALSE, uuencFileName = "") {
  
  opts <- GetOptions(ENCRYPT = TRUE, 
                     THREEDES = TRUE, 
                     ECB = ECB, 
                     UUENC = UUENC)
  
  .C( "callRDES", opts, key, sourceFile, encryptedFile, "", uuencFileName )
}


#' Decrypt a file with the Triple DES algorithm
#'
#' @param sourceFile The path the the file that should be decrypted
#' @param decryptedFile The path to the target file
#' @param key The 56bit HEX key
#' @param ECB Use ECB?
#' @param UUENC Whether the files should be UU encoded
#'
#' @export
DecryptFileTripleDES <- function(sourceFile, decryptedFile, key, ECB = FALSE, UUENC = FALSE) {
  
  opts <- GetOptions(ENCRYPT = FALSE, 
                     THREEDES = TRUE, 
                     ECB = ECB, 
                     UUENC = UUENC)
  
  .C( "callRDES", opts, key, sourceFile, decryptedFile, "", "" )
}


#' Encrypt a character string
#' 
#' @param text A character string
#' @param key The key as a character string
#' 
#' @export
Encrypt <- function(text, key) {
  r <- charToRaw(text)
  res <- .Call("rdesEncrypt", key, r)
  resChar <- rawToChar(res)
  return (resChar)
}



#' Decrypt a character string
#' 
#' @param text An encrypted character string
#' @param key The key as a character string
#' 
#' @export
Decrypt <- function(text, key) {
  r <- charToRaw(text)
  res <- .Call("rdesDecrypt", key, r)
  resChar <- rawToChar(res)
  return (resChar)
}




GetOptions <- function(ENCRYPT = FALSE, SUNOS = FALSE, HEX_KEY = FALSE, THREEDES = FALSE, ECB = FALSE, UUENC = FALSE) {
  #define RLIBDES_ENCRYPT       0x00000001     // encrypt if set, else decrypt 
  #define RLIBDES_SUNOS_COMPAT 0x00000002 // enable/disable SUNOS compatibility 
  #define RLIBDES_CBC_CHECKSUM  0x00000004 // calculate cbc-checksum or not 
  #define RLIBDES_KEY_FMT_HEX    0x00000008 // if key string is in hex instead of b64 
  #define RLIBDES_3DES             0x00000010 // (16) set to use 3DES 
  #define RLIBDES_MODE_ECB    0x00000020 // (32) if set, then encryption mode is ECB, if not - CBC 
  #define RLIBDES_UUENC_ENCRYPTED 0x00000040 // (64) set if encrypted data is\should be uuencoded (uuencHeaderFile must be set)
  opts <- 0
  if (ENCRYPT) opts <- opts + 1
  if (SUNOS) opts <- opts + 2
  if (HEX_KEY) opts <- opts + 8
  if (THREEDES) opts <- opts + 16
  if (ECB) opts <- opts + 32
  if (UUENC) opts <- opts + 64
  return (as.integer(opts))
}