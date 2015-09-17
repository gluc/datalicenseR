#' Download market data from Bloomberg Datalicense
#' 
#' \code{datalicenseR} lets you download price data directly from Bloomberg. Prerequisite is a valid Datalicense with Bloomberg.
#' The main high level functions you can use to quickly download and parse data from Bloomberg are:
#' \describe{
#'   \item{\code{\link{GetData}}}{To use the bloomberg getdata program.}
#'   \item{\code{\link{GetHistory}}}{To use the bloomberg gethistory program.}
#'   \item{\code{\link{GetSnapshot}}}{To use the bloomberg getsnap program.}
#' }
#' 
#' @import RCurl
#' @useDynLib libdes
#' @seealso For more details and lower level functions, see the \code{datalicenseR} vignette by running: \code{vignette("datalicenseR")}
#' @docType package
#' @name datalicenseR
NULL