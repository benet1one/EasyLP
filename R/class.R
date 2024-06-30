
require(R6)
require(lpSolveAPI)
require(rlang)

#' Easy Linear Problem
#' @description
#' Object containing all the information about a linear problem, as well
#' as functions to define, modify, and solve it.
#'
#' @field variables List of variables included.
#' @field nvar Actual number of variables, \code{sum(lengths(variables))}.
#' @field constraint List including the constraint matrix 'mat',
#' a vector of directions 'dir', and a vector of right-hand-side values 'rhs'.
#' @field objective_fun Vector of coefficients for the objective function.
#' @field objective_add Optional value to add to the objective value.
#' @field direction Character indicating whether to minimize 'min' or maximize
#' 'max' the objective function.
#' @field status Character indicating the status of the problem. Initialized as
#' 'unsolved'. Changes when solving the problem using \link{easylp$solve()}.
#' @field objective_value Value of the objective function with the optimal solution.
#' @field solution Vector containing the optimal values for the variables.
#' See \link{easylp$pretty_solution()} for a better representation of the solution.
#' @field pointer Pointer to an lpSolveAPI object.
#' See more at \url{https://lpsolve.sourceforge.net/5.5/}.
easylp <- R6Class("easylp", public = list(

    variables = list(),
    constraint = list(
        mat = array(dim = c(0, 0)),
        dir = character(),
        rhs = numeric()
    ),
    nvar = 0L,

    objective_fun = numeric(),
    objective_add = 0,
    direction = "min",

    status = "unsolved",
    objective_value = NA_real_,
    solution = structure(
        numeric(),
        info = "Use easylp$pretty_solution() to display the solution!"
    ),
    sensitivity = list(
        objective = array(
            dim = c(0, 2),
            dimnames = list(Var = character(), Bound = c("Upper", "Lower"))
        ),
        rhs = array(
            dim = c(0, 2),
            dimnames = list(Var = character(), Bound = c("Upper", "Lower"))
        )
    ),
    pointer = NULL,

    var = function(name, ..., integer = FALSE, binary = FALSE,
                   lower_bound = -Inf, upper_bound = +Inf) {

        stopifnot(is_scalar_character(name))
        stopifnot(is_scalar_logical(integer))
        stopifnot(is_scalar_logical(binary))

        if (binary) {
            integer <- FALSE
            if (lower_bound != -Inf || upper_bound != +Inf)
                warning("Ignoring bounds for binary variable ", name)
            lower_bound <- 0
            upper_bound <- 1
        }

        if (...length() == 0L) {
            sets <- list(scalar = "")
        } else {
            sets <- list(...)
            if (any(names(sets) == ""))
                stop("All sets in ... must be named")
        }

        ind <- array(dim = lengths(sets), dimnames = sets)
        ind[] <- 1:length(ind) + self$nvar
        len <- length(ind)

        selected <- logical(ind[len])
        selected[ind] <- TRUE

        add <- numeric(len)
        coef <- cbind(matrix(0, nrow = len, ncol = self$nvar),
                      diag(len))

        type <- if (integer)
            "integer"
        else if (binary)
            "binary"
        else
            "real"

        nams <- name_variable(name, sets)

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
        self$constraint$mat <- cbind(
            self$constraint$mat,
            matrix(0, nrow = nrow(self$constraint$mat), ncol = len,
                   dimnames = list(NULL, nams))
        )

        # Update objective function and solution
        self$objective_fun <- c(self$objective_fun, numeric(len) |> setNames(nams))
        self$solution <- c(self$solution, numeric(len) |> setNames(nams))

        # Create new variable
        x <- list(
            name = name,
            sets = sets,
            ind = ind,
            type = type,
            integer = integer,
            binary = binary,
            bound = c(Lower = lower_bound, Upper = upper_bound),
            selected = selected,
            coef = coef,
            add = add
        ) |> structure(class = "lp_var")

        self$variables <- append(self$variables, list(x) |> setNames(name))
        self$nvar <- self$nvar + len
        invisible(self)
    },
    con = function(..., expr_list = list()) {
        envir <- as_environment(self$variables, parent = caller_env())
        dots <- c(enexprs(...), expr_list)
        for (k in seq_along(dots)) {
            expr <- inside(dots[[k]])
            if (expr[[1L]] == quote(`for`)) {
                split <- for_split(expr)
                split <- name_for_split(split, name = names(dots)[k])
                self$con(expr_list = split)
                next
            }
            constraint <- eval(expr, envir)
            stopifnot(is_lp_con(constraint))
            if (nrow(constraint$mat) == 0L) {
                warning("Constraint ", k, " is empty.")
                next
            }
            constraint <- name_constraint(constraint, names(dots)[k])
            self$constraint$mat <- rbind(self$constraint$mat, constraint$mat)
            self$constraint$dir <- c(self$constraint$dir, unname(constraint$dir))
            self$constraint$rhs <- c(self$constraint$rhs, unname(constraint$rhs))
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
    solve = function() {

        if (self$nvar == 0L)
            stop("Problem contains no variables.")
        if (all(self$objective_fun == 0))
            stop("Must specify objective function.")

        prob <- make.lp(nrow = 0, ncol = self$nvar)
        set.objfn(prob, c(self$objective_fun))

        for (x in self$variables) {
            set.type(prob, columns = x$ind, type = x$type)
            set.bounds(prob, columns = x$ind,
                       lower = rep(x$bound[1L], length(x$ind)),
                       upper = rep(x$bound[2L], length(x$ind)))
        }

        with(self$constraint, for (i in 1:nrow(mat)) {
            d <- if (dir[i] == "==") "="  else dir[i]
            add.constraint(prob, mat[i, ], d, rhs[i])
        })

        status <- solve(prob)
        self$objective_value <- get.objective(prob) + self$objective_add
        self$solution[] <- get.variables(prob)
        self$status <- switch(
            as.character(status),
            "0" = "optimal",
            "1" = "sub-optimal",
            "2" = "unfeasable",
            "3" = "unbounded",

            "7" = "timeout",
            "5" =,
            "6" =,
            "10" = "failure",

            "unknown"
        )

        self$sensitivity$objective <- array(
            dim = c(length(self$objective_fun), 2L),
            dimnames = list(Variable = names(self$objective_fun),
                            Bound = c("Upper", "Lower"))
        )
        rhs <- array(
            dim = c(nrow(self$constraint$mat), 2L),
            dimnames = list(Constraint = rownames(self$constraint$mat),
                            Bound = c("Upper", "Lower"))
        )
        sens <- get.sensitivity.obj(prob)
        self$sensitivity$objective[, "Lower"] <- large_to_infinity(sens$objfrom)
        self$sensitivity$objective[, "Upper"] <- large_to_infinity(sens$objtill)
        sens <- get.sensitivity.rhs(prob)
        self$sensitivity$rhs[, "Lower"] <- large_to_infinity(sens$dualsfrom)
        self$sensitivity$rhs[, "Upper"] <- large_to_infinity(sens$dualstill)

        self$pointer <- prob
        self
    },

    feasable = function(solution = self$solution, tol = 2e-8) {
        stopifnot(nrow(self$constraint$mat) > 0L)
        for (k in 1:nrow(self$constraint$mat)) {
            lhs <- sum(solution * self$constraint$mat[k, ])
            dir <- self$constraint$dir[k]
            rhs <- self$constraint$rhs[k]

            feasable_k <- compare_tol(lhs, rhs, dir, tol)
            if (!feasable_k)
                return(FALSE)
        }
        TRUE
    },
    check_feasable = function() {
        if (self$feasable())
            return(self)
        message("Current solution has become unfeasable. Use $solve to find a new one.")
        self$status <- "unsolved"
        self$solution[] <- 0
        self$objective_value <- NA_real_
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

        cat("\n\nSolution:\n")
        print(self$pretty_solution())
    }
))


