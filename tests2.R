
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
mylp$var(x <= 19.5, provider = provider, town = town)
mylp$var(integer(y) >= 1.8, town = town)
mylp$con(y <= sum(x))
mylp$var(binary(z))

mylp$min(sum(x) - sum(y) - z - 2)
mylp$con(for (t in town) sum(x[, t]) >= demand[t])

mylp$solve()
mylp$feasable()

