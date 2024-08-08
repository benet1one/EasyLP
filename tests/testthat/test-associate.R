
lp <- easylp$new()
lp$var("x",  1:3, lower=0)
lp$var("y",  1:3, lower=-4, upper=12)
lp$var("b",  1:3, binary=TRUE)
lp$var("nb", 1:3, integer=TRUE, lower=0, upper=1)

test_that("unbounded numeric association", {
    expect_error(lp$associate(x, b))
    expect_no_error(lp$associate(x, b, max1 = 10))
})

test_that("non-binary association", {
    expect_warning(lp$associate(x, nb, max1 = 10))
})

lp$variables$x$bound[] <- 1:2
lp$associate(x + y/2, b)
