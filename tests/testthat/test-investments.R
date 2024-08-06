
Project = 1:6
Year = 1:5

npv <- c(141, 187, 121, 83, 265, 127) |> parameter(Project)
budget <- c(250, 75, 50, 50, 50) |> parameter(Year)

investment <- c(
    75, 25, 20, 15, 10,
    90, 35,  0,  0, 30,
    60, 15, 15, 15, 15,
    30, 20, 10,  5,  5,
    100, 25, 20, 20, 20,
    50, 20, 10, 30, 40
) |> parameter(Project, Year, byrow = TRUE)

. <- NA
incompatible <- c(
    ., 1, 0, 1, 0, 0,
    ., ., 1, 0, 0, 0,
    ., ., ., 0, 0, 0,
    ., ., ., ., 0, 0,
    ., ., ., ., ., 1,
    ., ., ., ., ., .
) |> parameter(Project, Project, byrow = TRUE)

lp <- easylp$new()
lp$var("x", Project, binary=TRUE)
lp$max(sum(x*npv))
lp$con(
    budget =
        for(a in Year)
        sum_for(p=Project, x[p] * investment[p, a]) <= budget[a],
    compatibility =
        for(p in Project[-length(Project)])
        for(q in (p+1):length(Project))
        x[p] + x[q] + incompatible[p, q] <= 2L
)

lp$solve()
lp

test_that("correct solution", {
    expect_equal(lp$objective_value, 469)
    expect_equal(lp$solution$x, c(0, 0, 1, 1, 1, 0), ignore_attr = TRUE)
})

test_that("wrong indexes", {
    expect_error(lp$variables$x[500])
    # TODO
    # expect_error(lp$variables$x[0])
})
