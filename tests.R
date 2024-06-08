
source("R/easylp.R")
source("R/utils.R")
source("R/methods.R")

provider <- 1:5
town <- c("Vallirana", "Cervelló", "Barcelona")

demand <- c(35, 21, 50)

x <- lp_var("x", provider = provider, town = town, bound = .x <= 5)
y <- lp_var("y", town = town, bound = .x >= 0, integer = TRUE)
z <- lp_var("z", binary = TRUE)

varlist <- join_lp_vars(list(x, y, z))

calc <- varlist$x[2, ] / 0.2
my_con <- lp_con(for (t in town) { x[p, t] >= demand[t] },
                 p = provider)

double_con <- lp_con(for (p in provider) for (t in town) {x[p, t] >= 0})
