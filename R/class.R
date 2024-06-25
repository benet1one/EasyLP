
require(R6)
require(lpSolve)
require(rlang)

easylp <- R6Class("easylp", public = list(

    variables = list(),
    constraints = list(
        mat = array(dim = c(0, 0)),
        dir = character(),
        rhs = numeric()
    ),
    n_vars = 0L,

    objective_fun = numeric(),
    objective_add = 0,
    direction = "min",

    status = "unsolved",
    objective_value = NA_real_,
    solution = numeric(),
    lpsolve = list(),

    var = function(name, ..., integer = FALSE, binary = FALSE) {

        stopifnot(is_scalar_character(name))
        stopifnot(is_scalar_logical(integer))
        stopifnot(is_scalar_logical(binary))

        # if (integer && binary)
        #     message("If binary = TRUE, you don't need to specify integer = TRUE")
        if (binary)
            integer <- TRUE

        if (...length() == 0L) {
            sets <- list(scalar = "")
        } else {
            sets <- list(...)
            if (any(names(sets) == ""))
                stop("All sets in ... must be named")
        }

        # grid <- do.call(expand.grid, sets)
        # x_name <- pmap(grid, paste, sep = "-")
        ind <- array(dim = lengths(sets), dimnames = sets)
        ind[] <- 1:length(ind) + self$n_vars
        len <- length(ind)

        selected <- logical(ind[len])
        selected[ind] <- TRUE

        add <- numeric(len)
        coef <- cbind(matrix(0, nrow = len, ncol = self$n_vars),
                      diag(len))

        # Update other variables
        for (k in seq_along(self$variables)) {
            ref <- self$variables [[k]]
            self$variables [[k]] $ selected <- c(
                ref$selected,
                logical(len)
            )
            self$variables [[k]] $ coef <- cbind(
                ref$coef,
                matrix(0, nrow = nrow(ref$coef), ncol = len)
            )
        }

        # Update constraint matrix
        self$constraints$mat <- cbind(
            self$constraints$mat,
            matrix(0, nrow = nrow(self$constraints$mat), ncol = len)
        )

        # Update objective function and solution
        self$objective_fun <- c(self$objective_fun, numeric(len))
        self$solution <- c(self$solution, numeric(len))

        # Create new variable
        x <- list(
            name = name,
            sets = sets,
            ind = ind,
            integer = integer,
            binary = binary,
            selected = selected,
            coef = coef,
            add = add
        ) |> structure(class = "lp_var")

        self$variables <- append(self$variables, list(x) |> setNames(name))
        self$n_vars <- self$n_vars + len
        self
    },
    con = function(..., expr_list = list()) {
        envir <- as_environment(self$variables, parent = caller_env())
        dots <- c(enexprs(...), expr_list)
        for (k in seq_along(dots)) {
            expr <- inside(dots[[k]])
            if (expr[[1L]] == quote(`for`)) {
                return(self$con(expr_list = for_split(expr)))
            }
            constraint <- eval(expr, envir)
            stopifnot(is_lp_con(constraint))
            if (nrow(constraint$mat) == 0L) {
                warning("Constraint ", k, " is empty.")
                next
            }
            constraint <- name_constraint(constraint, names(dots)[k])
            self$constraints$mat <- rbind(self$constraints$mat, constraint$mat)
            self$constraints$dir <- c(self$constraints$dir, constraint$dir)
            self$constraints$rhs <- c(self$constraints$rhs, constraint$rhs)
        }
        invisible(self)
    },
    min = function(objective) {
        self$direction <- "min"
        self$.obj(enexpr(objective))
    },
    max = function(objective) {
        self$direction <- "max"
        self$.obj(enexpr(objective))
    },
    solve = function(...) {

        if (self$n_vars == 0L)
            stop("Problem contains no variables.")
        if (all(self$objective_fun == 0))
            stop("Must specify objective function.")

        integer <- logical(self$n_vars)
        binary <- logical(self$n_vars)

        for (v in self$variables) {
            if (v$integer)
                integer[v$selected] <- TRUE
            if (v$binary)
                binary[v$selected] <- TRUE
        }

        self$lpsolve <- lpSolve::lp(
            direction = self$direction,
            objective.in = self$objective_fun,
            const.mat = self$constraints$mat,
            const.dir = self$constraints$dir,
            const.rhs = self$constraints$rhs,
            int.vec = which(integer),
            binary.vec = which(binary),
            ...
        )

        self$objective_value <- self$lpsolve$objval + self$objective_add
        self$solution <- self$lpsolve$solution
        self$status <- if (self$lpsolve$status == 0)
            "optimal"
        else
            "unfeasable/unlimited"
        self
    },
    .obj = function(expr) {
        envir <- as_environment(self$variables, parent = caller_env(2))
        joint_var <- sum(eval(expr, envir))
        self$objective_fun <- joint_var$coef
        self$objective_add <- joint_var$add
        self
    },
    .test = function(expr) {
        expr <- enexpr(expr)
        envir <- as_environment(self$variables, parent = caller_env())
        eval(expr, envir)
    },

    pretty_solution = function() {
        lapply(self$variables, \(x) {
            sol <- x$ind
            sol[] <- self$solution[x$ind]
            sol
        })
    },
    print = function() {
        cat("Easy Linear Problem \nStatus:", self$status)
        if (self$status != "optimal")
            return()

        val <- self$objective_value
        add <- self$objective_add
        cat("\nObjective Value =", val - add)
        if (add != 0)
            cat("", ifelse(add > 0, "+", "-"), abs(add), "=", val)

        cat("\nSolution:\n")
        print(self$pretty_solution())
    }
))


