
#' Convert an Error Code from a Snapshot to an Error Message
#' 
#' @param errorCode The Bloomberg Error Code
#' @return a human readable error message
#' @export
GetSnapshotErrorMessage <- function(errorCode) {
  f <- function(errorCode) {
    if ( errorCode == 0 ) return ('Good return. No errors occurred.')
    if ( errorCode == 10 ) return ('Bloomberg cannot find the security as specified.')
    if ( errorCode == 11 ) return ('Restricted Security. Must link to BLOOMBERG PROFESSIONAL® Service with access.')
    if ( errorCode == 100 ) return ('Maximum number of securities exceeded (20,000)')
    if ( errorCode == 150 ) return ('Security blocked due to embargo not being met')
    if ( errorCode == 988 ) return ('System Error on security level')
    if ( errorCode == 989 ) return ('Unrecognized pricing source')
    if ( errorCode == 990 ) return ('System Error Contact Product Support and Technical Assistance.')
    if ( errorCode == 991 ) return ('Invalid override value (e.g., bad date or number)')
    if ( errorCode == 992 ) return ('Unknown override field')
    if ( errorCode == 993 ) return ('Maximum number of overrides (20) exceeded')
    if ( errorCode == 994 ) return ('Permission denied.')
    if ( errorCode == 995 ) return ('Maximum number of fields exceeded.')
    if ( errorCode == 996 ) return ('Maximum number of data points exceeded (some data for this security is missing)')
    if ( errorCode == 997 ) return ('General override error (e.g., formatting error)')
    if ( errorCode == 998 ) return ('Security identifier type (e.g., CUSIP) is not recognized.')
    if ( errorCode == 999 ) return ('Unloadable security')
    return ('Unknown error')
  }
  sapply(errorCode, FUN = f)

}




#' Convert an Error Code from a GetData request to an Error Message
#' 
#' @param errorCode The Bloomberg Error Code
#' @return a human readable error message
#' @export
GetDataErrorMessage <- function(errorCode) {
  f <- function(errorCode) {
    if ( errorCode == 0 ) return ('Good return. No errors occurred.')
    if ( errorCode == 9 ) return ('Asset class not supported for BVAL Tier1 pricing.')
    if ( errorCode == 10 ) return ('Bloomberg cannot find the security as specified.')
    if ( errorCode == 11 ) return ('Restricted Security. Must link to a BLOOMBERG PROFESSIONAL terminal with access.')
    if ( errorCode == 123 ) return ('User not authorized for private loan (PRPL).')
    if ( errorCode == 605 ) return ('Permission for pricing source denied.')
    if ( errorCode == 988 ) return ('System Error on security level')
    if ( errorCode == 989 ) return ('Unrecognized pricing source')
    if ( errorCode == 990 ) return ('System Error. Contact Product Support and Technical Assistance')
    if ( errorCode == 991 ) return ('Invalid override value (e.g., bad date or number) or Maximum number of overrides (20) exceeded')
    if ( errorCode == 992 ) return ('Unknown override field')
    if ( errorCode == 994 ) return ('Maximum number of fields exceeded.')
    if ( errorCode == 995 ) return ('Permission denied.')
    if ( errorCode == 996 ) return ('Maximum number of data points exceeded (some data for this security is missing)')
    if ( errorCode == 997 ) return ('General override error (e.g., formatting error)')
    if ( errorCode == 998 ) return ('Security identifier type (e.g., CUSIP) is not recognized.')
    if ( errorCode == 999 ) return ('Unloadable security')
    
    return ('Unknown error')
  }
  sapply(errorCode, FUN = f)
}



#' Convert an Error Code from a GetHistory request to an Error Message
#' 
#' @param errorCode The Bloomberg Error Code
#' @return a human readable error message
#' @export
GetHistoryErrorMessage <- function(errorCode) {
  f <- function(errorCode) {
    if ( errorCode == 0 ) return ('Good return. No errors occurred.')
    if ( errorCode == -14 ) return ('Field is not recognized or supported by the gethistory program.')
    if ( errorCode == -13 ) return ('Field is not applicable and is only available for certain types of securities (for example, PX_EVAL_LEGACY will only return for securities redenominated in Euro.)')
    if ( errorCode == -12 ) return ('Field is not available.')
    if ( errorCode == -10 ) return ('Start date > End date.')
    if ( errorCode == 10 ) return ('Bloomberg cannot find the security as specified.')
    if ( errorCode == 11 ) return ('Restricted Security. Must link to a terminal with access.')
    if ( errorCode == 123 ) return ('User not authorized for private loan (PRPL).')
    if ( errorCode == 605 ) return ('Permission for pricing source denied.')
    if ( errorCode == 990 ) return ('System Error. Contact Product Support and Technical Assistance.')
    if ( errorCode == 992 ) return ('Unknown override field.')
    if ( errorCode == 994 ) return ('User does not have permission (contractual) to download history for this security.')
    if ( errorCode == 995 ) return ('Maximum number of fields exceeded.')
    if ( errorCode == 996 ) return ('Maximum number of data points exceeded (some data for this security is missing)')
    if ( errorCode == 998 ) return ('Security identifier type (e.g. CUSIP) is not recognized.')
    if ( errorCode == 999 ) return ('Unloadable security')
    
    
    return ('Unknown error')
  }
  sapply(errorCode, FUN = f)
}