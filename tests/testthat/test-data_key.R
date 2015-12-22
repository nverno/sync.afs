context("data_key")
library(data.table)

check_afs <- function() {
  if (!file.exists(get_afs()))
    skip('AFS directory not reachable.')
}

## Some file names for testing
testdir <- 'tracking_test'
test_tracker <- 'original.txt'
test_rename_fail <- 'test_rename_fail.txt'
test_rename_pass <- 'test_rename_pass.txt'
test_add <- 'test_add.txt'
backup <- 'backup.txt'

test_that("Creating key works/throws warings", {
  check_afs()
  tracker <- file.path(get_afs(), testdir, test_tracker)
  expect_warning(dummy <<- create_data_key_template(tracker=tracker))  # want this later
  expect_equal(nrow(dummy), 12)
})

test_that("Renaming sas files works", {
  check_afs()
  rename_fail <- file.path(get_afs(), testdir, test_rename_fail)
  rename_pass <- file.path(get_afs(), testdir, test_rename_pass)
  backup_file <- file.path(get_afs(), testdir, backup)

  ## Error when rdata has no master file
  file.copy(rename_fail, backup_file, overwrite = TRUE)  # backup test
  expect_error(update_key(path=file.path(get_afs(), testdir),
                          tracker=test_rename_fail,
                          data_key=copy(dummy)))         # master file for 'b' not found
  file.copy(backup_file, rename_fail, overwrite = TRUE)  # reset test

  ## Passing case: no missing master files
  file.copy(rename_pass, backup_file, overwrite = TRUE)  # backup test
  res <- update_key(path=file.path(get_afs(), testdir),
                    tracker=test_rename_pass,
                    data_key=copy(dummy))
  file.copy(backup_file, rename_pass, overwrite = TRUE)  # reset test

  ## Passing tests
  expect_equal(res$rname, dummy$rname)                           # rdata names same
  expect_false(isTRUE(all.equal(res$filename, dummy$filename)))  # filenames change
})

test_that('Adding new data to key works', {
  check_afs()
  add_pass <- file.path(get_afs(), testdir, test_add)
  backup_file <- file.path(get_afs(), testdir, backup)
  
  ## Add data
  file.copy(add_pass, backup_file, overwrite = TRUE)  # backup test
  res <- update_key(path=file.path(get_afs(), testdir),
                    tracker=test_add,
                    data_key=copy(dummy))
  file.copy(backup_file, add_pass, overwrite = TRUE)  # backup test

  ## Test that extra row is added
  expect_equal(nrow(res), nrow(dummy)+1)
})
