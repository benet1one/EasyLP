
lp <- easylp$new()
lp$var("x", 1:3, 1:3, lower=1, upper=10)
lp$var("y", 1:2, 1:2, 1:2, lower=1, upper=10)
lp$min(sum(x * runif(9, -1,+1)) + sum(y * runif(8, -1,+1)))

lp$con(
    rowSums(x) == colSums(x),
    diag(x)[2:3] == 1:2,
    apply(y, 1:2, mean) == 2:5
)

lp$solve()
x <- lp$solution$x
y <- lp$solution$y

test_that("simple modified", {
    expect_equal(rowSums(x), colSums(x), ignore_attr=TRUE)
    expect_equal(apply(y, 1:2, mean), 2:5, ignore_attr=TRUE)
    expect_equal(diag(x)[2:3], 1:2, ignore_attr=TRUE)
})


lp2 <- easylp$new()
lp2$var("x", d1 = letters[1:4], d2 = LETTERS[1:3], d3 = 1:2,
        lower = -10, upper = 10)
lp2$min(sum(x))
lp2$con(
    rowSums(x)[1] == 3,
    rowSums(x)["b"] == 4,
    apply(x, 1:2, mean)[1:2, "B"] == 2
)

lp2$solve()
x <- lp2$solution$x

test_that("indexed modified", {
    expect_true(rowSums(x)[1] == 3)
    expect_true(rowSums(x)["b"] == 4)
    expect_true(all(apply(x, 1:2, mean)[1:2, "B"] == 2))
})
