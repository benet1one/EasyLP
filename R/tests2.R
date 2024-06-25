
source("R/class.R")
source("R/methods.R")
source("R/utils.R")

# Sets
provider <- 1:5
town <- c("Vallirana", "Cervelló", "Barcelona")

# Parameters
demand <- c(35, 21, 50)
names(demand) <- town

# Problem
mylp <- easylp$new()
mylp$var("x", provider = provider, town = town)
mylp$var("y", town = town, integer = TRUE)
mylp$con(x_bound = x >= 0,
         y_bound = y <= 5L)
mylp$var("z", binary = TRUE)

mylp$con(for (t in town) sum(x[, t]) >= demand[t])
# for (t in town) {
#     mylp$con(sum(x[, t]) >= demand[t])
# }
