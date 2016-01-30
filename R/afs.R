##' AFS class to interact with data on AFS
##'
##' @docType class
##' @importFrom R6 R6Class
##' @format An object of \code{\link{R6Class}} with methods to interact with AFS.
##' @examples
##' AFS$new(cell='northstar.dartmouth.edu')
##' @field connected Checks if connected to AFS.
##' @field signin Sign in to AFS with credentials
##' @field logout Logout of AFS (remove all tokens)
##' @field get_tokens Get the current tokens held in the cache.
##' @field get_error Get the last error.
##' @field print_tokens Print the tokens table in a few formats.
##' @include sign_in.R
##' @export
AFS <- R6::R6Class(
  'AFS',
  public = list(
    ## AFS paths
    root = '//afs',  # root AFS path
    cell = NA,       # cell
    base = NA,       # user base directory 
    path = NA,       # full path to base directory
    
    ## When initializing, just check for sysutils and current tokens
    ## if the utils are available
    initialize = function(cell, base) {
      private$has_utils <- private$utils()
      if (!private$has_utils)
        stop("Can't connect to AFS without system utilities.")
      tokens <- private$get_tkns()
      if (length(tokens)) {
        private$tokens <- private$parse(tokens)
      }
      if (!missing(cell)) {
        self$cell <- cell
      } else if (!is.null((opt <- getOption('afs.cell')))) {
        self$cell <- opt
      }
      if (!missing(base)) {
        self$base <- base
      } else if (!is.null((opt <- getOption('afs.path')))) {
        self$base <- opt
      }
      if (!is.na(self$root) && !is.na(self$cell) && !is.na(self$base))
        self$path <- file.path(self$root, self$cell, self$base)
    },

    ## Check to see if there are valid tokens in the cache.
    connected = function() {
      if (identical(NA, private$tokens)) {
        FALSE
      } else {
        any(private$tokens[, expires > as.POSIXct(Sys.time())])
      }
    },
    
    ## sign in (interactive or non-interactive)
    signin = signin,

    ## Logout, removing all tokens from the cached (unlog)
    logout = function() {
      res <- system2('unlog')
      if (invisible(res == 0L)) {
        private$tokens <- NA
        private$error <- NA
        TRUE
      } else {
        warning('Failed to logout (using "unlog")')
        FALSE
      }
    },

    ## Get the last error (if not then NA) 
    get_error = function() { private$error },

    ## Get the current tokens (if none then NA)
    get_tokens = function() { private$tokens },
    
    ## Should probably just make a generic print function here
    ## Print the tokens table in different formats
    print_tokens = function(format=c('none', 'formattable', 'knitr')) {
      if (!self$connected()) return()
      type <- match.arg(format, format)
      if (type == 'formattable') {
        if (requireNamespace('formattable')) {
          res <- formattable::formattable(private$tokens, list(
            expires = formattable::formatter(
              'span',
              style=x ~ formattable::style(
                color = formattable::csscolor(
                  formattable::gradient(rank(x), 'white', 'red')))
            )
          ))
          return(res)
        } else warning('formattable not available')
      }
      if (type == 'knitr') {
        if (requireNamespace('knitr', quietly = TRUE)) {
          res <- as.character(knitr::kable(private$tokens, format='html', 
                                           caption='Current AFS Tokens'))
          return(res)
        } else warning('knitr not available')
      } else {
        private$tokens
      }
    }
  ),
  
  ## Private members/methods
  private = list(
    tokens = NA,
    has_utils = NA,
    error = NA,
    
    ## Check for required system utilities
    utils = function() {
      progs <- c('klog', 'tokens')
      command <- switch(Sys.info()['sysname'], 'Windows'='where', 'type')
      all(unlist(lapply(progs, system2, command=command, stdout=FALSE)) == 0L)
    },
    
    ## Submit AFS credentials
    submit = function(user, pwd, cell, ...) {
      res <- suppressWarnings(
        system2("klog", args=c("-principal", user, "-password", 
                               pwd, "-cell", cell), 
                stderr=TRUE)
      )
      if (!length(res)) {
        private$tokens <- private$parse(private$get_tkns())
        private$error <- NA
        TRUE
      } else {
        private$error <- res
        FALSE
      }
    },

    ## Parse tokens to a data.table
    parse = function(tokens) {
      tokens <- strsplit(tokens, '\\s+|@')
      out <- lapply(tokens, function(x) {
        list(user=sub('([[:alnum:]]+).*', '\\1', x[[2]]),
             cell=x[[6]],
             expires=as.POSIXct(
               paste(c(x[8:9], sub(']', '', x[[10]])), collapse=' '),
               format="%b %d %H:%M"))
      })
      data.table::rbindlist(out)
    },

    ## Query the system for any user tokens
    get_tkns = function() {
      response <- system2("tokens", stdout=TRUE, stderr=TRUE)
      has_token <- is.character(response) && 
        any((inds <- grepl("tokens for afs@", response)))
      if (has_token) response[inds] else character(0)
    }
  )
)
