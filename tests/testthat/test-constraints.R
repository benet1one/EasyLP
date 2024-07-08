
A <- 1:2
B <- 1:3
C <- 1:2

lp <- easylp$new()
lp$var("x", A, B, C)
lp$var("y", B)
lp$var("z", A, B, C)

lp$con(
    r1 = for(b in B) sum(x[, b, ]) <= y[b],
    r2 = for(a in A) for(b in B) x[a, b, 1] >= y[b]/2 + 1,
    r3 = for(b in B) x[, b, 2] >= 1,
    r4 = x <= z,
    r5 = cumsum(2*y + 1) >= 0,
)

lp$remove_constraint("r3")

test_that("invalid variable operations", {
    expect_error(lp$con(2/x[1, 1, 1] >= 0))
    expect_error(lp$con(x[1L] * y[1L] >= 0))
    expect_error(lp$con(abs(x) >= 2))
    expect_error(lp$con(y[9L] >= 0))
    expect_error(lp$con(y[1L, 1L] >= 0))
})
