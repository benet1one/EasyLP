
Avio <- c("Jumbo", "Petit", "Mitjà", "Gran")
preu <- c(79, 67, 50, 35)
benefici <- c(5.8, 4.2, 3, 2.3)
pressupost <- 2000

lp <- easylp$new()
lp$var("quin", Avio, binary = TRUE)
lp$var("x", Avio, integer = TRUE, lower = 0)

lp$max(sum(x * benefici))
lp$con(
    no_triat = x <= 100*quin,
    si_triat = x >= quin,
    tipus = sum(quin) == 3,

    r_pressupost = sum(x * preu) <= pressupost,
    min_avions = sum(x) >= 35,

    no_mes_petits_que_mitjans = x["Petit"] <= x["Mitjà"],
    no_jumbo_i_grans = quin["Jumbo"] + quin["Gran"] <= 1,
    quinze_percent = x["Jumbo"] <= 0.15 * sum(x)
)

lp$solve()
sol <- lp$pretty_solution()
despesa_total <- sum(sol$x * preu)

print(lp)
cat("Benefici =", lp$objective_value,
  "\nDespesa  =", despesa_total, "\n")

test_that("correct solution", {
    expect_equal(unname(lp$solution), c(0, 1, 1, 1, 0, 2, 3, 49))
})

test_that("become unfeasable", {
    expect_message(lp$con(no_factible = x["Petit"] + x["Mitjà"] >= 6))
    expect_setequal(lp$solution, 0)
})
