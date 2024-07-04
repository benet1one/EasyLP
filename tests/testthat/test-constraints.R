
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
    r4 = x <= z
)

test_that("can't divide by variable", {
    expect_error(lp$con(2/x[1, 1, 1] <= 0))
})
