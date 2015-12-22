context("data_key")

check_afs <- function() {
  if (!file.exists(get_afs()))
    skip('AFS directory not reachable.')
}

## Some file names for testing
testdir <- 'tracking_test'
test_tracker <- 'test.txt'
test_rename <- 'test_rename.txt'
test_reset <- 'reset.txt'

test_that("Creating key works/throws warings", {
  check_afs()
  tracker <- file.path(get_afs(), testdir, test_tracker)
  expect_warning(dummy <<- create_data_key_template(tracker=tracker))  # want this later
  expect_equal(nrow(dummy), 2)
})

test_that("Renaming sas files works", {
  check_afs()
  rename_file <- file.path(get_afs(), testdir, test_rename)
  reset_file <- file.path(get_afs(), testdir, test_reset)
  
  ## reset the rename file
  file.copy(reset_file, rename_file, overwrite = TRUE)
})
