##' Validate user input to be correct type
##' @title Validate user input
##' @param prompt Message to display to user
##' @param expect Type expected as response
##' @param allow_blank Allow empty responses
validate_input <- function(prompt, expect='character', allow_blank=FALSE) {
  while(TRUE) {
    response <- readline(prompt=prompt)
    if (expect %in% c('integer', 'numeric'))
      response <- as(response, expect)
    if (!is.null(response) && !is.na(response) &&
        (allow_blank || nzchar(response))) break
    cat(sprintf("Invalid input: expecting input of type %s\n", expect))
  }
  return( response )
}

##' Check for AFS, prompt for credentials and sign in if not
##' @title Check for AFS tokens
##' @export
check_afs <- function() {
  response <- system2("tokens", stdout=TRUE)
  has_token <- is.character(response) && any(grepl("tokens for afs@", response))
  if (has_token) return( invisible(TRUE) )
  cat("\n\nNo tokens found for AFS.")
  res <- FALSE
  if (interactive()) {
    res <- sign_in()
  }
  return( res )
}

##' Sign in to AFS
##' @title Attempt to sign in to AFS using klog
##' @export
sign_in <- function() {
  cat("\n\nAttempting to sign in to AFS with klog [Ctrl-C Ctrl-C to EXIT].\n")
  while(TRUE) {
    if (exists("res", inherits=FALSE))
      cat("\nTrying again (Ctrl-C Ctrl-C to EXIT).\n")
    username <- validate_input("Enter username:")
    passwd <- validate_input("Enter password:")
    other <- validate_input("Other commands to pass to klog (hit return to skip):",
                            allow_blank = TRUE)
    res <- system2("klog", args=c("-principal", username, "-password", passwd, other))
    if (res == 0L) {
      cat('\nSigned in successfully.')
      return ( TRUE )
    }
  }
  cat("\nFailed signing into AFS.  Please sign in manually.")
  return ( FALSE )
}
