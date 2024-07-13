
# set.seed(123)

like <- c(
    Math = 90,
    Physics = 80,
    Chemistry = 50,
    Spanish = 75,
    French = 10,
    Economics = 30,
    Sociology = 60,
    Music = 100,
    Drawing = 95,
    History = 40
)

subject <- names(like)
class <- c("A", "B")
day <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
hour <- paste0(8:12, ":00")

# Random Schedule ---------------------------

rs <- easylp$new()
rs$var("x", subject, hour, day, class, binary=TRUE)
rs$con(
    one_teacher =  x[, , , "A"] + x[, , , "B"] <= 1L,
    once_a_day  =  for (s in subject) for(d in day) for(c in class)
        sum(x[s, , d, c]) <= 1L,
    always_class = for(h in hour) for(d in day) for(c in class)
        sum(x[, h, d, c]) == 1L
)

lessons <- runif(length(subject), 1, 2) |> setNames(subject)
lessons <- (lessons / sum(lessons)) * length(day) * length(hour)
lessons <- layer::round_sum(lessons)

rs$con(
    min_lessons = for(s in subject) for(c in class) sum(x[s, , , c]) == lessons[s],
    # same_lessons = for(s in subject) sum(x[s, , , "A"]) == sum(x[s, , , "B"])
)

rand_obj <- runif(length(rs$variables$x))
rs$min(rand_obj*x)
rs$solve()
print(rs$status)

x <- rs$solution$x
schedule <- lapply(class, \(c) {
    mat <- parameter("", hour, day)
    for(h in hour) for(d in day) {
        s <- subject[as.logical(x[, h, d, c])]
        # browser()
        mat[h, d] <- s
    }
    mat
})
names(schedule) <- class
print(schedule)

