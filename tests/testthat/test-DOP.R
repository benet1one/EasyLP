
DOP <- c("Empordà", "Garrigues", "Siurana", "Terra Alta")
Super <- c("Girona", "Lleida", "Tarragona")
Molí <- c("A", "B")

capacitat_recollecció <- parameter(c(6000, 7000, 8000, 7000), DOP)
coeficient_extracció <- parameter(c(.25, .3, .25, .2), DOP)

cost_tdm <- parameter(c(
    54, 56,
    60, 49,
    41, 53,
    54, 52
), DOP, Molí, byrow = TRUE)

capacitat_extracció <- parameter(c(12000, 20000), Molí)
cost_extracció <- parameter(c(78, 82), Molí)

cost_tms <- parameter(c(
    47, 56, 58,
    51, 52, 59
), Super, Molí, byrow = TRUE)

demanda <- parameter(c(1500, 3000, 2500), Super)


lp <- easylp$new()
lp$var("tdm", DOP, Molí, lower = 0)
lp$var("tms", Molí, Super, lower = 0)

lp$min(sum(cost_tdm * tdm)
     + sum_for(m=Molí, tdm[, m] * cost_extracció[m])
     + sum(cost_tms * tms)
     - 45000
)

lp$alias(
    rec = rowSums(tdm),
    ext = rowSums(tms)
)

lp$con(
    tdm_ext =  for(m in Molí)
        sum_for(d=DOP, tdm[d, m] * coeficient_extracció[d]) == ext[m],
    recolleccció =  for(d in DOP)    rec[d] <= capacitat_recollecció[d],
    extracció =     for(m in Molí)   sum(tdm[, m]) <= capacitat_extracció[m],
    satisfacció =   for(s in Super)  sum(tms[, s]) >= demanda[s]
)

lp$solve()

test_that("correct objective", {
    expect_equal(lp$objective_value, 3985000 - 45000)
})

