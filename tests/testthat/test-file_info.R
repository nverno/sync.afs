context('test file info')
library(data.table)

files <- c('abba15ex.sas7bdat', 'pp_demslope.csv')
rnames <- c('abba', 'dem')

test_that('file_info names R files properly', {
  res <- file_info(files = files, rnames=rnames)
  expect_true(all(res[order(filename), rname] == rnames[order(files)]))

  res <- file_info(files = rev(files), rnames= rev(rnames))
  expect_true(all(res[order(filename), rname] == rnames[order(files)]))
})



