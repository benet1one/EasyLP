
lp <- easylp$new()
lp$var("x", lower = 0)
lp$var("y")

test_that("decreasing transformations", {
    expect_no_warning(lp$max(x, trans = log))
    expect_warning(lp$max(y, trans = log))
    expect_warning(lp$max(x, trans = \(z) 1/z))
})
