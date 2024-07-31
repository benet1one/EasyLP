
library(R6)
library(lpSolveAPI)
library(rlang)

#' Easy Linear Problem
#' @description
#' Object containing all the information about a linear problem, as well
#' as functions to define, modify, and solve it.
#'
#' @field solution Optimal values for each variable, with
#' the sets they were defined with.
#' @field sensitivity_objective Sensitivity for objective function coefficients.
#' Rows are variables, first column is the lower bound,
#' second column is the current value,
#' and third column is the upper bound.
#' @field sensitivity_rhs Sensitivity for constraint right-hand-side coefficients.
#' Rows are variables, first column is the lower bound,
#' second column is the current value,
#' and third column is the upper bound.
#' @field variables List of variables included.
#' @field constraint List including the constraint matrix 'mat',
#' a vector of directions 'dir', and a vector of right-hand-side values 'rhs'.
#' Printed as a single matrix.
#' @field nvar Actual number of variables, \code{sum(lengths(variables))}.
#' @field ncon Number of constraints. \code{nrow(constraint$mat)}
#' @field direction Character indicating whether to minimize 'min' or maximize
#' 'max' the objective function. Automatically changed when defining
#' objective with \code{easylp$min(), easylp$max()}
#' @field objective_fun Vector of coefficients for the objective function.
#' @field objective_add Optional value to add to the objective value.
#' Automatically set when there's an added/subtracted constant in the objective function.
#' @field objective_value Value of the objective function with the optimal solution.
#' @field status Character indicating the status of the problem. Initialized as
#' 'unsolved'. Changes when solving the problem using \code{easylp$solve()}.
#' @field pointer Pointer to an lpSolveAPI object. Do not modify it, as
#' it is redefined every time the problem is solved.
#' See more at \url{https://lpsolve.sourceforge.net/5.5/}.
#'
#' @import R6
#' @import lpSolveAPI
#' @import rlang
easylp <- R6Class("easylp",
public = {list(

    variables = list(),
    aliases = list(),
    constraint = list(
        mat = array(dim = c(0, 0)),
        dir = character(),
        rhs = numeric()
    ) |> structure(class = "lp_con"),

    objective_fun = numeric(),
    objective_add = 0,
    objective_transform = identity,

    pointer = NULL,

    #' @description
    #' Define a variable.
    #' @param name Character scalar, name of the variable. Will be used in the
    #' constraints and objective function.
    #' @param ... Optional sets used to index the variable. Can be named.
    #' Supports \code{!!!sets} for using a list of sets. See examples.
    #' @param integer Logical. Is the variable an integer?
    #' @param binary Logical. Is the variable binary/boolean/logical?
    #' @param lower Numeric scalar. Lower bound for the variable.
    #' @param upper Numeric scalar. Upper bound for the variable.
    #' @export
    #' @examples
    #' lp <- easylp$new()
    #' lp$var("x", Origin = letters[1:3], Destination = letters[1:3], lower = 0)
    #' my_sets <- list(Origin = letters[1:3], Destination = letters[1:3])
    #' lp$var("y", !!!my_sets, lower = 0)
    #' lp$variables
    var = function(name, ..., integer = FALSE, binary = FALSE,
                   lower = -Inf, upper = +Inf) {

        stopifnot(is_scalar_character(name),
                  is_scalar_logical(integer),
                  is_scalar_logical(binary),
                  length(lower) == 1L,
                  length(upper) == 1L,
                  lower < upper)

        if (is.element(name, names(self$variables)))
            stop("Variable '", name, "' already defined in this model.")

        if (binary) {
            integer <- FALSE
            if (lower != -Inf || upper != +Inf)
                warning("Ignoring bounds for binary variable ", name)
            lower <- 0
            upper <- 1
        }

        if (...length() == 0L)
            sets <- list(scalar = "")
        else
            sets <- dots_list(..., .named = TRUE)

        ind <- array(dim = lengths(sets), dimnames = sets)
        ind[] <- 1:length(ind) + private$nvar
        len <- length(ind)

        selected <- logical(ind[len])
        selected[ind] <- TRUE

        add <- numeric(len)
        coef <- cbind(matrix(0, nrow = len, ncol = private$nvar),
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
        private$sol <- c(private$sol, numeric(len) |> setNames(nams))
        if (lower > 0 || upper < 0)
            self$reset_solution()

        # Create new variable
        x <- list(
            name = name,
            sets = sets,
            ind = ind,
            type = type,
            integer = integer,
            binary = binary,
            bound = c(Lower = lower, Upper = upper),
            selected = selected,
            indexable = TRUE,
            raw = TRUE,
            coef = coef,
            add = add
        ) |> structure(class = "lp_var")

        self$variables <- append(self$variables, list(x) |> setNames(name))
        private$nvar <- private$nvar + len
        invisible(self)
    },
    #' @description
    #' Define constraints.
    #' @param ... Constraints. See \code{vignette("constraints")}. Can be named.
    #' Supports \code{!!!} (unquote-splice).
    #' @param parent Used for recursion, do not change.
    #' @export
    #' @examples
    #' # Make sure to build vignettes when installing the package.
    #' vignette("constraints")
    con = function(..., parent = 1L) {
        dots <- enexprs(...)
        for (k in seq_along(dots)) {
            expr <- inside(dots[[k]])
            if (expr[[1L]] == quote(`for`)) {
                split <- for_split(expr, evaluator = private$eval,
                                   envir = caller_env(parent))
                split <- name_for_split(split, name = names(dots)[k])
                self$con(!!!split, parent = parent + 1L)
                next
            }
            constraint <- private$eval(expr, parent = caller_env(parent))
            if (!is_lp_con(constraint))
                stop("Constraint ", k, " did not evaluate to an (in)equality.")
            if (nrow(constraint$mat) == 0L) {
                warning("Constraint ", k, " is empty.")
                next
            }
            constraint <- name_constraint(constraint, names(dots)[k])
            self$constraint$mat <- rbind(self$constraint$mat, constraint$mat)
            self$constraint$dir <- c(self$constraint$dir, unname(constraint$dir))
            self$constraint$rhs <- c(self$constraint$rhs, unname(constraint$rhs))
        }
        self$check_feasible()
        invisible(self)
    },
    #' @description
    #' Define objective function for a minimization problem.
    #' @param objective Uses the same syntax as constraints.
    #' Must be a single value, so use \code{sum()} when needed.
    min = function(objective, trans) {
        private$dir <- "min"
        private$set_objective(enexpr(objective))
    },
    #' @description
    #' Define objective function for a maximization problem.
    #' @param objective Uses the same syntax as constraints.
    #' Must be a single value, so use \code{sum()} when needed.
    max = function(objective, trans) {
        private$dir <- "max"
        private$set_objective(enexpr(objective))
    },
    #' @description
    #' Find an optimal solution.
    #' @param ... Arguments passed on to \code{lpSolveAPI::lp.control()}.
    #' See \code{\link[lpSolveAPI]{lp.control.options}}.
    solve = function(...) {

        if (private$nvar == 0L)
            stop("Problem contains no variables.")
        if (all(self$objective_fun == 0))
            stop("Must specify objective function.")
        if (!is.element(private$dir, c("min", "max")))
            stop("Direction must be either 'min' or 'max'.")

        # try(delete.lp(self$pointer), silent = TRUE)
        prob <- make.lp(nrow = 0, ncol = private$nvar)
        set.objfn(prob, self$objective_fun)
        lp.control(prob, sense = private$dir, ...)

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
        private$objval <- get.objective(prob) |> large_to_infinity()
        private$sol[] <- get.variables(prob) |> large_to_infinity()
        private$stat <- switch(
            as.character(status),
            "0" = "optimal",
            "1" = "sub-optimal",
            "2" = "unfeasible",
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
        invisible(self)
    },

    #' @description
    #' Remove constraints.
    #' @param name Character vector containing the (unindexed) name of
    #' the constraints to remove.
    uncon = function(name) {
        if (!is_character(name))
            stop("Use the name <character> of a constraint to remove it.")
        unind <- sub(rownames(self$constraint$mat),
                     pattern = "\\[.+", replacement = "")
        to_remove <- is.element(unind, name)
        self$constraint$mat <- self$constraint$mat[!to_remove, , drop=FALSE]
        self$constraint$dir <- self$constraint$dir[!to_remove]
        self$constraint$rhs <- self$constraint$rhs[!to_remove]
        invisible(self)
    },
    #' @description
    #' Associate a numeric variable with a binary variable.
    #' It's recommended to set both lower and upper bound for the
    #' numeric variable before associating it to a binary variable.
    #' @param x Numeric variable.
    #' @param binary Binary variable.
    #' @param max1 Upper bound for 'x' when 'binary == 1'
    #' @param max0 Upper bound for 'x' when 'binary == 0'
    #' @param min1 Lower bound for 'x' when 'binary == 1'
    #' @param min0 Lower bound for 'x' when 'binary == 0'
    #' @details
    #' By default, 'x' is bounded between it's lower and upper bound when
    #' 'binary == 1', and it's bounded to it's lower value when
    #' 'binary == 0'. If 'x' is integer, you usually want to set 'min0' to
    #' 1, to ensure 'x != 0' when 'binary == 1'.
    associate = function(x, binary,
                         max1 = x$bound[2L], max0 = x$bound[1L],
                         min1 = x$bound[1L], min0 = x$bound[1L]) {

        x <- private$eval(enexpr(x))
        x <- update_bounds(x, self$variables)

        stopifnot(length(max1) == 1L, length(max0) == 1L,
                  length(min1) == 1L, length(min0) == 1L,
                  is.finite(max1), is.finite(max0),
                  is.finite(min1), is.finite(min0))

        b <- private$eval(enexpr(binary))

        if (!b$binary)
            warning("Variable '", format(enexpr(binary)), "' is not binary.",
                    "Result may not be as expected.")

        if (max1 != x$bound[2L] || max0 != x$bound[2L])
            self$con(assoc_max =  !!enexpr(x) <=
                         !!max0 + !!(max1 - max0) * !!enexpr(binary))

        if (min1 != x$bound[1L] || min0 != x$bound[1L])
            self$con(assoc_min =  !!enexpr(x) >=
                         !!min0 + !!(min1 - min0) * !!enexpr(binary))

        invisible(self)
    },
    alias = function(...) {
        dots <- enexprs(...)
        if (any(names2(dots) == ""))
            stop("Aliases must be named.")
        self$aliases <- append(self$aliases, dots)
    },

    #' @description
    #' Checks if the current solution is feasible and resets it otherwise.
    #' This function should be used every time you manually change a
    #' value in the constraint matrix or right-hand-side.
    #' @param tol Tolerance used for inequalities.
    check_feasible = function(tol = 2e-8) {

        if (private$stat == "unsolved")
            return(self)

        feas <- private$feasible(tol)

        if (any(!feas)) {
            unfeas <- paste(names(feas)[!feas], collapse = ",")
            message("Constrainsts: ", unfeas, "; are unfeasible. ",
                    "Use easylp$solve() to find a new solution.")
            self$reset_solution()
        }

        invisible(self)
    },
    #' @description
    #' Returns an error if problem is unsolved. Used internally.
    check_solved = function() {
        if (private$stat == "unsolved")
            stop("Linear Problem has not been solved. Use easylp$solve().")
    },
    #' @description
    #' Does the problem contain any integer or binary variables?
    #' Used internally.
    any_integer = function() {
        for (v in self$variables)
            if (v$integer || v$binary) return(TRUE)
        return(FALSE)
    },
    #' @description
    #' Remove all solution data, including the objective value.
    #' The pointer to the lpSolveAPI model is kept. Used internally.
    reset_solution = function() {
        private$stat <- "unsolved"
        private$sol[] <- 0
        private$objval <- NA_real_
        invisible(self)
    },

    #' @description
    #' Check if an operation is valid, using the problem's variables.
    #' Supports 'for' syntax used in constraints.
    #' @param ... Expressions to evaluate. Supports !!injection.
    #' @param envir Used for recursion, do not change.
    test = function(..., parent = 1L) {
        dots <- enexprs(...)
        results <- list()
        for (k in seq_along(dots)) {
            expr <- dots[[k]]
            if (expr[[1L]] == quote(`for`)) {
                split <- for_split(expr, evaluator = private$eval,
                                   envir = caller_env(parent))
                split <- name_for_split(split, name = names(dots)[k])
                res <- self$test(!!!split, parent = parent + 1L)
                results <- append(results, list(res))
                next
            }
            res <- tryCatch(private$eval(expr, parent = caller_env(parent)), error = identity)

            if (is_lp_var(res))
                colnames(res$coef) <- colnames(self$constraint$mat)
            else if (is_lp_con(res))
                colnames(res$mat) <- colnames(self$constraint$mat)

            results <- append(results, list(res))
        }

        names(results) <- names(dots)
        results
    },

    #' @description
    #' Print relevant information about a linear problem:
    #' status, objective value, and solution.
    print = function() {
        cat("Easy Linear Problem \nStatus:", private$stat)
        if (private$stat != "optimal")
            return()

        val <- self$objective_value
        add <- self$objective_add
        cat("\nObjective Value =", val - add)
        if (add != 0)
            cat("", ifelse(add > 0, "+", "-"), abs(add), "=", val)

        cat("\n\nSolution:\n\n")
        if (length(self$solution) == 1L)
            print(self$solution[[1L]])
        else
            print(self$solution)
    },
    #' @description
    #' Delete the problem.
    finalize = function() {
        # if (!is.null(self$pointer))
        #   try(delete.lp(self$pointer), silent = TRUE)
        # self$pointer <- NULL
    }
)},
private = {list(
<<<<<<< Updated upstream
    nvar = 0L,
    sol = numeric(),
    objval = NA_real_,
    .status = "unsolved",
    set_objective = function(expr) {
        joint_var <- sum(private$eval(expr, parent = caller_env(2L)))
=======
    n_var = 0L,
    dir = "min",
    sol = numeric(),
    objval = NA_real_,
    stat = "unsolved",
    set_objective = function(expr, trans) {
        joint_var <- private$eval(expr, parent = caller_env(2L), split_for = FALSE)
>>>>>>> Stashed changes
        self$objective_fun[] <- joint_var$coef
        self$objective_add <- joint_var$add
        self$reset_solution()
        invisible(self)
    },
    feasible = function(tol = 2e-8) {
        list2env(self$constraint, environment())
        stopifnot(nrow(mat) > 0L)
        lhs <- mat %*% private$sol
        nam <- rownames(mat)
        nam[nam == ""] <- which(nam == "")
        compare_tol(lhs, rhs, dir, tol) |> setNames(nam)
    },
    eval = function(expr, parent = caller_env(2L)) {
        modified_env <- as_environment(modified, parent = parent)
        variable_env <- as_environment(self$variables, parent = modified_env)
        expr2 <- substituteDirect(expr, frame = self$aliases)
        eval(expr2, variable_env)
    }
)},
active = {list(
    # nvar = function(arg) {
    #     error_field_assign()
    #     private$nvar
    # },
    ncon = function(arg) {
        error_field_assign()
        length(self$constraint$rhs)
    },
    direction = function(arg) {
        if (missing(arg)) return(private$dir)
        if (is_character(arg, n = 1L) && tolower(arg) %in% c("min", "max"))
            private$dir <- tolower(arg)
        else stop("Direction must be either 'min' or 'max'.")
    },
    solution = function(arg) {
        error_field_assign()
        if (private$stat != "optimal")
            warning("Problem is not optimal.\n")
        lapply(self$variables, \(x) {
            if (length(x$ind) == 1L)
                return(private$sol[x$ind] |> unname())
            sol <- x$ind
            sol[] <- private$sol[x$ind]
            sol
        })
    },
    objective_value = function(arg) {
        error_field_assign()
        self$check_solved()
        private$objval + self$objective_add
    },
    status = function(arg) {
        error_field_assign()
        private$stat
    },
    sensitivity_objective = function(arg) {
        error_field_assign()
        stopifnot(private$stat == "optimal")
        if (self$any_integer())
            stop("Sensitivity unavailable for problems with integer/binary variables")
        objective <- array(
            dim = c(length(self$objective_fun), 3L),
            dimnames = list(Variable = names(private$sol),
                            Bound = c("Lower", "Current", "Upper"))
        )
        sens <- get.sensitivity.obj(self$pointer)
        objective[, "Lower"] <- large_to_infinity(sens$objfrom)
        objective[, "Upper"] <- large_to_infinity(sens$objtill)
        objective[, "Current"] <- self$objective_fun
        return(objective)
    },
    sensitivity_rhs = function(arg) {
        error_field_assign()
        stopifnot(private$stat == "optimal")
        if (self$any_integer())
            stop("Sensitivity unavailable for problems with integer/binary variables")
        rhs <- array(
            dim = c(nrow(self$constraint$mat), 3L),
            dimnames = list(Constraint = rownames(self$constraint$mat),
                            Bound = c("Lower", "Current", "Upper"))
        )
        sens <- get.sensitivity.rhs(self$pointer)
        rhs[, "Lower"] <- large_to_infinity(sens$dualsfrom[1:nrow(rhs)])
        rhs[, "Upper"] <- large_to_infinity(sens$dualstill[1:nrow(rhs)])
        rhs[, "Current"] <- self$constraint$rhs
        return(rhs)
    }
)}
)


