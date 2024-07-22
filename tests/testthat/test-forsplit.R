
lp <- easylp$new()
lp$var("x", 1:4, 1:4)
lp$con(
    for(i in 1:4) for(j in i:4) x[i, j] == 1L
)
