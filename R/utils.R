
inside <- function(expr) {
    stopifnot(is.language(expr))
    if (is_symbol(expr))
        return(expr)
    if (expr[[1L]] == quote(`{`) || expr[[1L]] == quote(`(`))
        inside(expr[[2L]])
    else
        expr
}
for_split_old <- function(expr, evaluator = eval, envir = caller_env()) {

    if (expr[[1L]] != quote(`for`))
        stop("Expression is not wrapped in a for loop")

    sequence <- expr[[3L]] |> evaluator(envir)
    interior <- expr[[4L]]

    looper_env <- list(NA)
    names(looper_env) <- expr[[2L]] |> format()
    result <- list()

    for (k in seq_along(sequence)) {
        looper_env[[1L]] <- sequence[k]
        result[[k]] <- substituteDirect(interior, frame = looper_env)
    }

    structure(result, variable = names(looper_env), sequence = sequence)
}
for_split <- function(expr, envir = caller_env(), evaluate = TRUE) {

    stopifnot(is.language(expr))
    expr <- inside(expr)

    if (expr[[1L]] != quote(`for`)) {
        if (evaluate) return(eval(expr, envir))
        else return(expr)
    }

    sequence <- expr[[3L]] |> eval(envir = envir)
    interior <- expr[[4L]]

    looper_env <- list(NA)
    names(looper_env) <- expr[[2L]] |> format()
    result <- list()

    for (k in seq_along(sequence)) {
        looper_env[[1L]] <- unname(sequence[k])
        result[[k]] <- substituteDirect(interior, frame = looper_env)
    }

    result <- lapply(result, for_split, envir, evaluate)

    structure(
        result,
        variable = names(looper_env),
        sequence = sequence,
        names = names(sequence),
        class = "ForSplit"
    )
}

flatten_for_split <- function(split, init_name = "") {

    atoms <- list()

    add <- function(x, name = paste0(init_name, "[")) {

        if (inherits(x, "ForSplit")) {
            var <- attr(x, "variable")
            seq <- attr(x, "sequence")
            name <- sub("\\]", ",", name)
            next_names <- paste0(name, var, "=", seq, "]")

            for (k in seq_along(x)) {
                add(x[[k]], next_names[k])
            }

        } else {
            atoms <<- append(atoms, list(x) |> setNames(name))
        }
    }

    add(split)
    return(atoms)
}
join_constraints <- function(constraint, ...) {
    dots <- list2(...)
    for (k in seq_along(dots)) {
        con <- dots[[k]]
        stopifnot(is_lp_con(con))
        if (is.null(rownames(con$mat)))
            con <- name_constraint(con, names2(dots)[k])
        constraint$mat <- rbind(constraint$mat, con$mat)
        constraint$dir <- c(constraint$dir, unname(con$dir))
        constraint$rhs <- c(constraint$rhs, unname(con$rhs))
    }
    constraint
}

name_variable <- function(name, sets) {
    if (length(sets) == 1L && length(sets[[1]]) == 1L)
        return(name)
    grid <- do.call(expand.grid, sets)
    index <- .mapply(dots = grid, FUN = paste, MoreArgs = list(sep = ","))
    paste0(name, "[", index, "]")
}
name_constraint <- function(constraint, name, previous = character()) {
    if (name == "")
        return(constraint)
    if (nrow(constraint$mat) > 1L)
        name <- paste0(name, "[", 1:nrow(constraint$mat), "]")
    rownames(constraint$mat) <- name
    constraint
}

compare_tol <- Vectorize(function(lhs, rhs, dir, tol) {
    if (dir == "==")
        return(abs(lhs - rhs) <= tol)
    match.fun(dir)(lhs + c(tol,-tol), rhs) |> any()
})
large_to_infinity <- function(x, threshold = 1e30) {
    x[x >= +threshold] <- +Inf
    x[x <= -threshold] <- -Inf
    return(x)
}
update_bounds <- function(x, varlist) {

    bounds <- lapply(varlist, \(v) matrix(v$bound, nrow = 2L, ncol = length(v)))
    bounds <- do.call(cbind, bounds)
    rownames(bounds) <- c("Lower", "Upper")

    upper <- numeric(nrow(x$coef))
    lower <- numeric(nrow(x$coef))

    for (k in 1:nrow(x$coef)) {
        pos <- x$coef[k, ] > 0
        neg <- !pos
        upper[k] <-
            sum(x$coef[k, pos] * bounds["Upper", pos]) +
            sum(x$coef[k, neg] * bounds["Lower", neg]) +
            x$add[k]
        lower[k] <-
            sum(x$coef[k, pos] * bounds["Lower", pos]) +
            sum(x$coef[k, neg] * bounds["Upper", neg]) +
            x$add[k]
    }

    x$bound["Upper"] <- max(upper)
    x$bound["Lower"] <- min(lower)
    return(x)
}

warn_changed_args <- function(..., .suffix = ".", envir = caller_env()) {
    dots <- enexprs(...)
    for (k in names(dots))
        if (!identical(envir[[k]], dots[[k]]))
            warning("'", k, "' is ignored", .suffix)
}
error_field_assign <- function(message = "Cannot modify this field.") {
    env <- caller_env() |> as.list()
    if (!is_missing(env[[1L]]))
        stop(message)
}

modified <- within(list(), {
    diag <- function(x = 1, nrow, ncol, names = TRUE) {
        if (!is_lp_var(x))
            return(base::diag(x, nrow, ncol, names))
        warn_changed_args(nrow = , ncol = , names = TRUE)

        x <- x[base::diag(x$ind)]
        x$raw <- FALSE
        # x$indexable <- FALSE
        return(x)
    }
    apply <- function(X, MARGIN, FUN, ..., simplify = TRUE) {

        if (!is_lp_var(X))
            return(base::apply(X, MARGIN, FUN, ..., simplify))

        warn_changed_args(simplify = TRUE)

        if (is.character(MARGIN)) {
            if (is.null(dimnames(X)))
                stop("'X' must have named dimnames.")
            MARGIN <- match(MARGIN, dimnames(X))
            if (anyNA(MARGIN))
                stop("Not all elements of 'MARGIN' are names of dimensions.")

        } else if (is.numeric(MARGIN)) {
            if (any(MARGIN < 1) || any(MARGIN > length(dim(X))))
                stop("'MARGIN' does not match dim(X).")
        }

        grid_cols <- lapply(dim(X)[MARGIN], seq_len)
        grid <- expand.grid(grid_cols)

        coef <- matrix(ncol = ncol(X$coef), nrow = nrow(grid))
        add <- numeric(nrow(grid))

        for (k in 1:nrow(grid)) {
            ind <- lapply(dim(X), seq_len)
            ind[MARGIN] <- grid[k, ]
            y <- do.call(`[.lp_var`, c(list(X), ind))
            z <- FUN(y, ...)
            coef[k, ] <- z$coef
            add[k] <- z$add
        }

        X$ind <- array(1:nrow(grid), dim(X)[MARGIN], dimnames(X)[MARGIN])
        X$coef <- coef
        X$add <- add
        X$raw <- FALSE
        # X$indexable <- FALSE
        return(X)
    }
    rowSums  <- function(x, na.rm = FALSE, dims = 1) {
        if (!is_lp_var(x))
            return(base::rowSums(x, na.rm, dims))
        warn_changed_args(
            na.rm = FALSE,
            dims = 1,
            .suffix = " for linear variables."
        )
        apply(x, 1L, sum)
    }
    rowMeans <- function(x, na.rm = FALSE, dims = 1) {
        if (!is_lp_var(x))
            return(base::rowMeans(x, na.rm, dims))
        warn_changed_args(
            na.rm = FALSE,
            dims = 1,
            .suffix = " for linear variables."
        )
        apply(x, 1L, mean)
    }
    colSums  <- function(x, na.rm = FALSE, dims = 1) {
        if (!is_lp_var(x))
            return(base::colSums(x, na.rm, dims))
        warn_changed_args(
            na.rm = FALSE,
            dims = 1,
            .suffix = " for linear variables."
        )
        apply(x, 2L, sum)
    }
    colMeans <- function(x, na.rm = FALSE, dims = 1) {
        if (!is_lp_var(x))
            return(base::colMeans(x, na.rm, dims))
        warn_changed_args(
            na.rm = FALSE,
            dims = 1,
            .suffix = " for linear variables."
        )
        apply(x, 2L, mean)
    }
})


#' Define a parameter for a linear problem.
#' @description
#' Automatically set the dimensions and names of a parameter, based on
#' sets. Similar to \code{number} in the SAS/OR proc optmodel.
#'
#' @param x Coefficients for the parameter.
#' @param ... Sets to index the parameter.
#' @param byrow If there are 2 sets in \code{...}, whether to fill
#' the matrix by rows. Otherwise filled by columns
#'
#' @return Named array.
#' @export
#'
#' @examples
#' factory <- c("A", "B")
#' market <- c(1:3)
#' transport_cost <- parameter(c(
#'     3, 4, 2,
#'     6, 2, 5
#' ), factory, market)
parameter <- function(x, ..., byrow = FALSE) {
    if (...length() == 0L)
        stop("Parameter does not have any sets.")
    sets <- dots_list(..., .named = TRUE)
    if (length(x) == 1L)
        x <- rep(x, prod(lengths(sets)))
    else if (length(x) != prod(lengths(sets)))
        stop("Dimensions of the parameter don't match dimensions of the sets.")

    if (byrow) {
        if (length(sets) != 2L)
            stop("Use 'byrow = TRUE' only with 2-dimensional arrays.")
        matrix(x, nrow = lengths(sets)[1L], dimnames = sets, byrow = TRUE)
    } else {
        array(x, dim = lengths(sets), dimnames = sets)
    }
}

#' Sum with indexing variables.
#' @description
#' Calculates results independently and adds them.
#' @param ... Named arguments are interpreted as indexing variables. One
#' unnamed argument should the expression.
#' @param .env Environment where the expressions should be evaluated.
#' @returns The sum of all evaluated expressions.
#' @export
#' @examples
#' mat <- matrix(rpois(12, 2), ncol = 4L)
#' vec <- rpois(4, 5)
#' sum_for(i=1:3, mat[i, ] * vec[i])
#' sum(mat %*% vec)
sum_for <- function(..., .env = caller_env()) {
    dots <- enexprs(...)
    unnamed <- names(dots) == ""
    named <- !unnamed
    if (sum(unnamed) != 1L)
        stop("No unnamed expression.")
    if (!any(named))
        stop("No named indexing variables.")

    expr <- dots[[which(unnamed)]]
    ind <- dots[named] |> lapply(eval, envir = .env)
    grid <- do.call(expand.grid, ind)
    result <- vector("list", length = nrow(grid))

    for (i in 1:nrow(grid)) {
        frame <- as_environment(grid[i, , drop=FALSE], parent = .env)
        result[[i]] <- eval(expr, frame)
    }

    do.call(sum, result)
}

#' Randomize values for a variable.
#' @description
#' You can use this function to test if the variable is correctly defined,
#' or to check how you may index it. The values aren't necessarily feasable,
#' but they are bounded. If the variable is integer or binary, the values will too.
#'
#' @param x Linear problem variable. Get it from 'easylp$variables'.
#' @param max_value Numeric, maximum absolute value of the values, when
#' the variable is unbounded.
#'
#' @return An array of the same dimensions as the variable.
#' @export
#'
#' @examples
#' lp <- easylp$new()
#' lp$var("x", letters[1:2], 1:3, lower=1, integer=TRUE)
#' example_values(lp$variables$x)
example_values <- function(x, max_value = 100) {
    stopifnot(is_lp_var(x), length(max_value) == 1L)
    max_value <- abs(max_value)
    lower <- max(x$bound[1L], -max_value)
    upper <- min(x$bound[2L], +max_value)
    values <- runif(length(x), min = lower, max = upper)
    if (x$integer || x$binary)
        values <- round(values)

    arr <- x$ind
    arr[] <- values
    return(arr)
}
