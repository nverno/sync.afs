##' Sign in to AFS, if options aren't supplied, try interactive
##' ... ignored for now, but can be other options to klog
signin <- function(user, pwd, cell, ...) {
  cell <- if (missing(cell)) {
    if (!is.na(self$cell)) {
      self$cell
    } else getOption('afs.cell')
  }
  if (missing(user) || missing(pwd)) {
    if (interactive()) {
      cat("\n\n*******************************************************\n")
      cat("Attempting to sign in to AFS [Ctrl-C Ctrl-C to EXIT].\n")
      while(TRUE) {
        user <- readline("Enter username:")
        pwd <- readline("Enter password:")
        cat(sprintf('\n Logging in to cell %s', getOption('afs.cell')))
        ## dots <- readline("Other commands to pass to klog (hit return to skip):")
        res <- private$submit(user, pwd, cell, ...)
        if (!res) {
          cat(sprintf('\n%s', as.vector(private$error)))
          cat("\nTrying again (Ctrl-C Ctrl-C to EXIT).\n")
        } else {
          cat('\nSigned in successfully.\n')
          cat('********************************************************\n')
          return( TRUE )
        }
      }
      FALSE
    } else FALSE
  } else {
    private$submit(user, pwd, cell, ...)
  }
}

