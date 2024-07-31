
lp <- easylp$new()
lp$var("x", 1:4, 1:4)
lp$con(
    hi = for(i in 1:4) for(j in i:4) x[i, j] == 1L
)
lp$test(x[1, 1])
