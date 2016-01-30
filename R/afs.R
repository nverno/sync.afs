##' AFS class to interact with data on AFS
##'
##' @docType class
##' @importFrom R6 R6Class
##' @format An object of \code{\link{R6Class}} with methods to interact with AFS.
##' @examples
##' AFS$new()
##' @field connected Checks if connected to AFS.
##' @field signin Sign in to AFS with credentials
##' @field logout Logout of AFS (remove all tokens)
##' @field get_tokens Get the current tokens held in the cache.
##' @field get_error Get the last error.
##' @field print_tokens Print the tokens table in a few formats.
##' @export
AFS <- R6Class(
  'AFS',
  public = list(
    ## When initializing, just check for sysutils and current tokens
    ## if the utils are available
    initialize = function() {
      private$has_utils <- private$utils()
      if (!private$has_utils) 
        stop("Can't connect to AFS without system utilities.")
      tokens <- private$get_tkns()
      if (length(tokens)) {
        private$tokens <- private$parse(tokens)
      }
    },
    connected = function() {
      if (identical(NA, private$tokens)) {
        FALSE
      } else {
        any(private$tokens[, expires > as.POSIXct(Sys.time())])
      }
    },
    signin = function(user, pwd, cell='northstar.dartmouth.edu', ...) {
      if (missing(user) || missing(pwd)) return(FALSE)
      res <- suppressWarnings(
        system2("klog", 
                args=c("-principal", user, 
                       "-password", pwd,
                       "-cell", cell), 
                stderr=TRUE))
      if (!length(res)) {
        private$tokens <- private$parse(private$get_tkns())
        private$error <- NA
        return(TRUE)
      } else {
        private$error <- res
        return(FALSE)
      }
    },
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
    get_error = function() private$error,
    get_tokens = function() private$tokens,
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
  
  ## Private Methods
  private = list(
    tokens = NA,
    has_utils = NA,
    error = NA,
      
    ## Check for system utils
    utils = function() {
      progs <- c('klog', 'tokens')
      command <- switch(Sys.info()['sysname'], 'Windows'='where', 'type')
      all(unlist(lapply(progs, system2, command=command, stdout=FALSE)) == 0L)
    },
    ## Parse tokens to data.table
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
    ## Get any user tokens
    get_tkns = function() {
      response <- system2("tokens", stdout=TRUE, stderr=TRUE)
      has_token <- is.character(response) && 
        any((inds <- grepl("tokens for afs@", response)))
      if (has_token) response[inds] else character(0)
    }
  )
)
