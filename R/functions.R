#' Create an FTP or SFTP connection to Bloomberg Datalicense
#' 
#' 
#' @param account The account number assigned by Bloomberg
#' @param pw The password of your Bloomberg account
#' @param host The Bloomberg ftp host
#' @param ftpType Any of \code{passive}, \code{active}
#' @param encrypt If \code{TRUE}, then SFTP is used
#' @param port The port used by FTP (20 for active, 21 for passive connections) or by SFTP (30206)
#' @param passivePorts In the case of ftpMode = 'passive', this is the min and the max port for the data port range.

#' @return an S3 BdlConnection object, used to upload requests and download prices.
#' @export
BdlConnection <- function(account, 
                          pw, 
                          host = 'bfmrr.bloomberg.com', 
                          ftpType = 'passive',
                          encrypt = FALSE,
                          port = 21, 
                          passivePorts = c(32768, 65535)) {
  
}