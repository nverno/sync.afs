context("data_key")
library(data.table)

## Dummy check for tests
simple_afs_check <- function() file.exists(get_afs())
test_afs <- FALSE
if (Sys.info()['sysname'] == 'Windows') test_afs <- FALSE  # path length problems
path <- if (test_afs && simple_afs_check()) get_afs() else 'test_data/'

## Some file names for testing
testdir <- 'tracking_test'
test_tracker <- 'original.txt'
test_rename_fail <- 'test_rename_fail.txt'
test_rename_pass <- 'test_rename_pass.txt'
test_add <- 'test_add.txt'
backup <- 'backup.txt'

test_that("Creating key works/throws warnings", {
    tracker <- file.path(path, testdir, test_tracker)
    expect_warning(dummy <<- create_data_key_template(path=path,
                                                      tracker=tracker))  # want this later
    expect_equal(nrow(dummy), 12)
})

test_that("Renaming sas files works", {
    rename_fail <- file.path(path, testdir, test_rename_fail)
    rename_pass <- file.path(path, testdir, test_rename_pass)
    backup_file <- file.path(path, testdir, backup)

    ## Error when rdata has no master file
    file.copy(from=rename_fail, to=backup_file, overwrite = TRUE)  # backup test
    expect_error(update_key(path=file.path(path, testdir),
                            tracker=test_rename_fail,
                            data=dummy))               # master file for 'b' not found
    file.copy(from=backup_file, to=rename_fail, overwrite = TRUE)  # reset test

    ## Passing case: no missing master files
    file.copy(rename_pass, backup_file, overwrite = TRUE)  # backup test
    res <- update_key(path=file.path(path, testdir),
                      tracker=test_rename_pass,
                      data=dummy)
    file.copy(backup_file, rename_pass, overwrite = TRUE)  # reset test

    ## Passing tests
    expect_equal(res$rname, dummy$rname)                           # rdata names same
    expect_false(isTRUE(all.equal(res$filename, dummy$filename)))  # filenames change
})

test_that('Adding new data to key works', {
    add_pass <- file.path(path, testdir, test_add)
    backup_file <- file.path(path, testdir, backup)
    
    ## Add data
    file.copy(add_pass, backup_file, overwrite = TRUE)  # backup test
    res <- update_key(path=file.path(path, testdir),
                      tracker=test_add,
                      data=dummy)
    file.copy(backup_file, add_pass, overwrite = TRUE)  # backup test

    ## Test that extra row is added
    expect_equal(nrow(res), nrow(dummy)+1)
})

