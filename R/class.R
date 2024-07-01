
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
            sets <- dots_list(...)
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

        # Update objective function
        self$objective_fun <- c(self$objective_fun, numeric(len) |> setNames(nams))

        # Update solution
        self$solution <- c(self$solution, numeric(len) |> setNames(nams))
        if (lower_bound > 0 || upper_bound < 0)
            self$reset_solution()

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

        if (self$direction == "min")
            set.objfn(prob, +c(self$objective_fun))
        else if (self$direction == "max")
            set.objfn(prob, -c(self$objective_fun))
        else stop("Direction must be either 'min' or 'max'.")

        for (x in self$variables) {
            set.type(prob, columns = x$ind, type = x$type)
            set.bounds(prob, columns = x$ind,
                       lower = rep(x$bound[1L], length(x$ind)),
                       upper = rep(x$bound[2L], length(x$ind)))
        }

        with(self$constraint, for (i in seq_along(rhs)) {
            d <- if (dir[i] == "==") "="  else dir[i]
            add.constraint(prob, mat[i, ], d, rhs[i])
        })

        status <- solve(prob)
        objval <- get.objective(prob) |> large_to_infinity()
        if (self$direction == "max") objval <- -objval
        self$objective_value <- objval + self$objective_add
        self$solution[] <- get.variables(prob) |> large_to_infinity()
        self$status <- switch(
            as.character(status),
            "0" = "optimal",
            "1" = "sub-optimal",
            "2" = "unfeasable",
            "3" = "unbounded",
            "4" = "degenerate model",
            "5" = "numerical failure encountered",
            "6" = "process aborted",
            "7" = "timeout",
            "9" = "the model was solved by presolve",
            "10" = "the branch and bound routine failed",
            "11" = "the branch and bound was stopped because of a break-at-first or break-at-value",
            "12" = "a feasible branch and bound solution was found",
            "13" = "no feasible branch and bound solution was found",
            "undocumented status"
        )

        self$pointer <- prob
        self
    },

    sensitivity_objective = function() {
        self$check_solved()
        if (self$any_integer())
            stop("Sensitivity unavailable for problems with integer/binary variables")
        objective <- array(
            dim = c(length(self$objective_fun), 2L),
            dimnames = list(Variable = names(self$solution),
                            Bound = c("Upper", "Lower"))
        )
        sens <- get.sensitivity.obj(self$pointer)
        objective[, "Lower"] <- large_to_infinity(sens$objfrom)
        objective[, "Upper"] <- large_to_infinity(sens$objtill)
        return(objective)
    },
    sensitivity_rhs = function() {
        self$check_solved()
        if (self$any_integer())
            stop("Sensitivity unavailable for problems with integer/binary variables")
        rhs <- array(
            dim = c(nrow(self$constraint$mat), 2L),
            dimnames = list(Constraint = rownames(self$constraint$mat),
                            Bound = c("Upper", "Lower"))
        )
        sens <- get.sensitivity.rhs(self$pointer)
        rhs[, "Lower"] <- large_to_infinity(sens$dualsfrom)
        rhs[, "Upper"] <- large_to_infinity(sens$dualstill)
        return(rhs)
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
        message("Current solution has become unfeasable. Use easylp$solve() to find a new one.")
        self$reset_solution()
        return(self)
    },
    check_optimal = function() {
        self$check_solved()
    },
    check_solved = function() {
        if (self$status == "unsolved")
            stop("Linear Problem has not been solved. Use easylp$solve().")
    },
    any_integer = function() {
        for (v in self$variables)
            if (v$integer || v$binary) return(TRUE)
        return(FALSE)
    },
    reset_solution = function() {
        self$status <- "unsolved"
        self$solution[] <- 0
        self$objective_value <- NA_real_
        invisible(self)
    },

    .obj = function(expr) {
        envir <- as_environment(self$variables, parent = caller_env(2))
        joint_var <- sum(eval(expr, envir))
        self$objective_fun[] <- joint_var$coef
        self$objective_add <- joint_var$add
        self$reset_solution()
        invisible(self)
    },
    .test = function(expr) {
        expr <- enexpr(expr)
        envir <- as_environment(self$variables, parent = caller_env())
        eval(expr, envir)
    },

    pretty_solution = function() {
        self$check_solved()
        stopifnot(self$status == "optimal")
        lapply(self$variables, \(x) {
            if (length(x$ind) == 1L)
                return(self$solution[x$ind] |> unname())
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


