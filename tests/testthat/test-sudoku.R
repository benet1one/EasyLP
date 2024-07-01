
sudoku <- matrix(ncol = 4, c(
    0, 1, 0, 2,
    0, 3, 0, 0,
    3, 0, 4, 0,
    0, 0, 0, 3
))

bin_sudoku <- array(FALSE, dim = rep(4, 3L))
for (i in 1:4) for (j in 1:4) {
    n <- sudoku[i, j]
    if (n == 0) next
    bin_sudoku[i, j, n] <- TRUE
}

lp <- easylp$new()
sets <- rep(list(1:4), 3L)
names(sets) <- c("row", "col", "num")
lp$var("x", !!!sets, binary = TRUE)
lp$min(sum(x))
lp$con(
    fill  = for(i in 1:4)  for(j in 1:4)  sum(x[i, j, ]) == 1L,
    rows  = for(i in 1:4)  for(n in 1:4)  sum(x[i, , n]) == 1L,
    cols  = for(j in 1:4)  for(n in 1:4)  sum(x[, j, n]) == 1L,
    quad1 = for(n in 1:4)  sum(x[1:2, 1:2, n]) == 1L,
    quad2 = for(n in 1:4)  sum(x[1:2, 3:4, n]) == 1L,
    quad3 = for(n in 1:4)  sum(x[3:4, 1:2, n]) == 1L,
    quad4 = for(n in 1:4)  sum(x[3:4, 3:4, n]) == 1L
)

for(i in 1:4) for(j in 1:4) for(n in 1:4) {
    if (!bin_sudoku[i, j, n])
        next
    lp$con(x[i, j, n] == 1L)
}

lp$solve()

sol <- lp$pretty_solution() $ x
for (n in 2:4) sol[, , n] <- sol[, , n] * n
sol <- apply(sol, 1:2, sum)

correct_sol <- matrix(ncol = 4, c(
    4, 1, 3, 2,
    2, 3, 1, 4,
    3, 2, 4, 1,
    1, 4, 2, 3
))

test_that("sudoku solved", {
    expect_equal(sol, correct_sol, ignore_attr = TRUE)
})
