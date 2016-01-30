context("data_key")
library(data.table)

## Dummy check for tests
afs <- AFS$new()
simple_afs_check <- function() afs$connected() && file.exists(afs$path)
test_afs <- TRUE
## if (Sys.info()['sysname'] == 'Windows') test_afs <- FALSE  # path length problems
path <- if (test_afs && simple_afs_check()) afs$path else 'test_data/'

## Some file names for testing
testdir <- 'tracking_test'
path <- file.path(path, testdir)
test_tracker <- 'original.txt'
test_rename_fail <- 'test_rename_fail.txt'
test_rename_pass <- 'test_rename_pass.txt'
test_add <- 'test_add.txt'
backup <- 'backup.txt'

test_that("Creating key works/throws warnings", {
    tracker <- file.path(path, test_tracker)
    
    ## save this for next tests
    expect_warning(dummy <<- create_data_key_template(path=path, tracker=tracker))
    expect_equal(nrow(dummy), 12)
})

test_that("Renaming sas files works", {
    rename_fail <- file.path(path, test_rename_fail)
    rename_pass <- file.path(path, test_rename_pass)
    backup_file <- file.path(path, backup)

    ## Error when rdata has no master file
    file.copy(from=rename_fail, to=backup_file, overwrite = TRUE)  # backup test
    expect_error(update_key(path=path, tracker=rename_fail,
                            data=dummy, save_key=FALSE))           # master file for 'b' not found
    file.copy(from=backup_file, to=rename_fail, overwrite = TRUE)  # reset test

    ## Passing case: no missing master files
    file.copy(rename_pass, backup_file, overwrite = TRUE)  # backup test
    res <- update_key(path=path, tracker=rename_pass, data=dummy, save_key=FALSE)
    file.copy(backup_file, rename_pass, overwrite = TRUE)  # reset test

    ## Passing tests
    expect_equal(res$rname, dummy$rname)                           # rdata names same
    expect_false(isTRUE(all.equal(res$filename, dummy$filename)))  # filenames change
})

test_that('Adding new data to key works', {
    add_pass <- file.path(path, test_add)
    backup_file <- file.path(path, backup)
    
    ## Add data
    file.copy(add_pass, backup_file, overwrite = TRUE)  # backup test
    res <- update_key(path=path, tracker=add_pass, data=dummy, save_key=FALSE)
    file.copy(backup_file, add_pass, overwrite = TRUE)  # backup test

    ## Test that extra row is added
    expect_equal(nrow(res), nrow(dummy)+1)
})

