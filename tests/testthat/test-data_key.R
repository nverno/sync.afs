context("data_key")

check_afs <- function() {
  if (!file.exists(get_afs()))
    skip('AFS directory not reachable.')
}

testdir <- 'tracking_test'
test_tracker <- 'test.txt'
test_rename <- 'test_rename.txt'

test_that("Creating key works/throws warings", {
  check_afs()
  tracker <- file.path(get_afs(), testdir, test_tracker)
  expect_warning(dummy <<- create_data_key_template(tracker=tracker))  # want this later
  expect_equal(nrow(dummy), 2)
})

test_that("Renaming sas files works", {
  check_afs()
  rename_file <- file.path(get_afs(), testdir, test_rename)
  
})

## library(dplyr)
## library(geometry)
## library(rgl)

## # define point grid
## r <- 50   # resolution
## grid <- expand.grid(
##   x = seq(-6000, 6000, by = r),
##   y = seq(-4000, 4000, by = r),
##   z = seq(-6000, 6000, by = r))  # data.table::CJ(x,y,z) if speed is a factor

## # get points satisfying every inequality
## toPlot <- df %>% 
##   select(x, y, z) %>% 
##   data.matrix %>% 
##   `%*%`(t(grid)) %>%
##   `<`(df$value) %>% 
##   apply(2, all)

## ## Alternative way to get points (saves time avoiding apply)
## toPlot2 <-
##   colSums(data.matrix(df[, c('x', 'y', 'z')]) %*% t(grid) < df$value) == nrow(df)

## # get convex hull, print volume
## gridPoints <- grid[toPlot2, ]
## hull <- convhulln(gridPoints, "FA")
## hull$vol
## #> 285767854167

## ## plot (option 2: extract triangles first - much faster avoiding apply)
## triangles <- gridPoints[c(t(hull$hull)), ]
## rgl.triangles(triangles, alpha=0.8, color=gray.colors(3))

## apply(hull$hull, 1, function(i) gridPoints[i, ]) %>% 
##   lapply(rgl.triangles, alpha = .8, color = gray.colors(5))
