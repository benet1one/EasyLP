
lp <- easylp$new()
lp$var("x", 1:3, 1:3, lower=1, upper=10)
lp$min(sum(x * runif(9, -1,+1)))

lp$con(
    rowSums(x) == colSums(x),
    diag(x) == 1:3
)

lp$solve()
x <- lp$solution$x |> print()
rowSums(x) |> unname() |> print()
colSums(x) |> unname() |> print()

test_that("rowSums == colSums", {
    expect_equal(rowSums(x), colSums(x), ignore_attr=TRUE)
})
