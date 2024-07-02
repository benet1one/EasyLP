
source("R/utils.R")
source("R/methods.R")
source("R/class.R")

library(purrr)

size <- 9L
quadrants <- expand.grid(c(1, 4, 7), c(1, 4, 7)) %>% as.matrix()


sudoku <- matrix(ncol = size, c(
    0,4,0,8,1,0,3,2,0,
    0,2,6,0,0,9,0,8,0,
    0,9,8,2,7,0,0,6,0,
    0,0,2,0,6,4,5,0,0,
    6,0,3,0,0,0,2,0,8,
    4,0,1,7,0,2,9,0,0,
    5,6,0,0,0,0,0,0,2,
    2,0,0,4,0,8,0,0,3,
    8,3,0,0,0,0,0,1,4
))

lp <- easylp$new()
lp$var("x", row = 1:size, col = 1:size, num = 1:size, binary = TRUE)
lp$min(sum(x))
lp$con(
    fill = for(i in 1:size)  for(j in 1:size)  sum(x[i, j, ]) == 1L,
    rows = for(i in 1:size)  for(n in 1:size)  sum(x[i, , n]) == 1L,
    cols = for(j in 1:size)  for(n in 1:size)  sum(x[, j, n]) == 1L,
)

for(k in 1:nrow(quadrants)) {
    start <- quadrants[k, ]
    end <- start + 2L
    i <- start[1L] : end[1L]
    j <- start[2L] : end[2L]
    lp$con(quad = for(n in 1:size) sum(x[i, j, n]) == 1L)
}

for(i in 1:size) for(j in 1:size) {
    n <- sudoku[i, j]
    if (n != 0) lp$con(x[i, j, n] == 1L)
}

lp$solve(break.at.first = TRUE, timeout = 120)
sol <- lp$pretty_solution() $ x
for(n in 2:4) sol[, , n] <- sol[, , n] * n
sol <- apply(sol, 1:2, sum)

