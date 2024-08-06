
Avio <- c("Jumbo", "Petit", "Mitjà", "Gran")
preu <- c(79, 67, 50, 35)
benefici <- c(5.8, 4.2, 3, 2.3)
pressupost <- 2000

lp <- easylp$new()
lp$var("quin", Avio, binary = TRUE)
lp$var("x", Avio, integer = TRUE, lower=0, upper=100)

lp$max(sum(x * benefici))
lp$associate(x, quin, min1 = 1)
lp$con(
    tipus = sum(quin) == 3,
    r_pressupost = sum(x * preu) <= pressupost,
    min_avions = sum(x) >= 35,

    no_mes_petits_que_mitjans = x["Petit"] <= x["Mitjà"],
    no_jumbo_i_grans = quin["Jumbo"] + quin["Gran"] <= 1,
    quinze_percent = x["Jumbo"] <= 0.15 * sum(x)
)

lp$solve()
lp$import_solution(silent = TRUE)

test_that("correct solution", {
    expect_equal(x, c(0, 2, 3, 49), ignore_attr = TRUE)
    expect_equal(quin, c(0, 1, 1, 1), ignore_attr = TRUE)
})

test_that("become unfeasable", {
    expect_message(lp$con(no_factible = x["Petit"] + x["Mitjà"] >= 6))
})

