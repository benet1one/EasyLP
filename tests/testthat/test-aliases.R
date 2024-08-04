
factory <- c("A", "B")
market <- c(1, 2)

capacity <- c(120, 180) |> parameter(factory)
demand <- c(140, 150) |> parameter(market)

lp <- easylp$new()
lp$var("t", factory, market, lower=0)

lp$alias(
    Fac = factory,
    Mar = market,
    made = rowSums(t),
    sold = colSums(t),
)

lp$con(
    cap =  for(i in Fac)  made[i] <= capacity[i],
    dem =  for(j in Mar)  sold[j] >= demand[j]
)
lp$constraint

test_that("aliases", {
    expect_error(lp$alias(err = t[1, 2, 3]))
})
