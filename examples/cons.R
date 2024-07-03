
lp <- easylp$new()
lp$var("x", 1:2)
lp$var("y", 1:2)
lp$con(
    a =  x >= y,
    b =  sum(x) + y[1] == 2
)
print(lp$constraint)
