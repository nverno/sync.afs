##' @details
##' Process the file_tracker.txt document.  If files were renamd, replace with new names.
##' Also, if filenames are duplicated, they are removed.
##' @title process_tracker
##' @param tracker Path to file tracking file
##' @return list of two elements: filenames to track, and renamed files
##' @export
process_tracker <- function(
  tracker=file.path(if(Sys.info()[['sysname']]=='Linux') afs.linux else afs,
    "file_tracker.txt")) {
  doc <- readLines(tracker)
  fileinds <- !grepl("^#|^\\s+$|^$", doc)
  lines <- doc[fileinds]
  renamed <- strsplit(lines, "\\s*->\\s*")
  key <- if (any((ind <- lengths(lapply(renamed, unlist)) > 1))) renamed[ind] else list()
  files <- basename(trimws(lines[!ind], "both"))
  files <- unique(files[!(files %in% sapply(key, `[[`, 1))])  # in case duplicates for some reason

    ## If filenames were changed, rewrite the file
    if (length(key)) {
        lines[ind] <- sapply(key, `[[`, 2)
        lines <- unique(lines)
        doc[fileinds] <- lines
        writeLines(doc, con=tracker)
        ## remove extra quotes from key
        key <- rapply(key, function(x) gsub('\"|\'', '', x), how = 'replace')
    }

    ## If duplicated filenames, rewrite the file with those removed
    if (any((inds <- duplicated(lines)))) {
        doc <- doc[sort(c(which(!fileinds), which(fileinds)[-inds]))]
        writeLines(doc, con=tracker)
    }
    
    list(files=gsub('\"|\'', '', files), renamed=key)
}
