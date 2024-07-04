
inside <- function(expr) {
    if (is_symbol(expr))
        return(expr)
    stopifnot(is_expression(expr) || is_call(expr))
    if (expr[[1L]] == quote(`{`) || expr[[1L]] == quote(`(`))
        inside(expr[[2L]])
    else
        expr
}
for_split <- function(expr, envir = parent.frame()) {

    if (expr[[1L]] != quote(`for`))
        stop("Expression is not wrapped in a for loop")

    sequence <- expr[[3L]] |> eval(envir = envir)
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

name_variable <- function(name, sets) {
    if (length(sets) == 1L && length(sets[[1]]) == 1L)
        return(name)
    grid <- do.call(expand.grid, sets)
    index <- .mapply(dots = grid, FUN = paste, MoreArgs = list(sep = ", "))
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
name_for_split <- function(fsplit, name) {
    names(fsplit) <- paste0(
        name, "[", attr(fsplit, "variable"), "=", attr(fsplit, "sequence"), "]"
    )
    fsplit
}

compare_tol <- function(lhs, rhs, dir, tol) {
    if (dir == "==")
        return(abs(lhs - rhs) <= tol)
    match.fun(dir)(lhs + c(tol,-tol), rhs) |> any()
}
large_to_infinity <- function(x, threshold = 1e30) {
    x[x >= +threshold] <- +Inf
    x[x <= -threshold] <- -Inf
    return(x)
}


#' Define a parameter for a linear problem.
#' @description
#' Automatically set the dimensions and names of a parameter, based on
#' sets. Similar to \code{number} in the SAS/OR proc optmodel.
#'
#' @param x Coefficients for the parameter.
#' @param ... Sets to index the parameter.
#'
#' @return Named array.
#' @export
#'
#' @examples
#'
parameter <- function(x, ..., byrow = FALSE) {
    if (...length() == 0L)
        stop("Parameter does not have any sets.")
    sets <- dots_list(..., .named = TRUE)
    if (length(x) != prod(lengths(sets)))
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
