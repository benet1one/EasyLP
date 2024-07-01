
lp <- easylp$new()
lp$var("x")
lp$max(x)
lp$solve()

test_that("infinity", {
    expect_equal(unname(lp$solution), +Inf)
    expect_equal(unname(lp$objective_value), +Inf)
})
