
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
    if (length(sets) == 0L)
        return(name)
    grid <- do.call(expand.grid, sets)
    index <- .mapply(dots = grid, FUN = paste, MoreArgs = list(sep = ", "))
    paste0(name, "[", index, "]")
}
name_constraint <- function(constraint, name) {
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
