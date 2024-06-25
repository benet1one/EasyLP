#' @export
print.lp_var <- function(x) {
    cat("Linear Programming Variable '", x$name, "'", sep = "")
    if (x$binary)
        cat(" <binary>")
    else if (x$integer)
        cat(" <integer>")
    if (length(x$ind) > 1L)
        cat("\nWith sets: [", paste(names(x$sets), collapse = ", "), "]")
    if (!is.null(x$bound))
        cat("\nBounded: .x", x$bound$dir, x$bound$rhs)
}
#' @export
dim.lp_var <- function(x) dim(x$ind)
#' @export
`[.lp_var` <- function(x, ...) {
    ind <- `[`(x$ind, ...) + x$previous_vars
    x$selected[] <- FALSE
    x$selected[ind] <- TRUE
    x$coef[!x$selected] <- 0
    x
}


horizontal_multiply_add <- function(x, mult = 1, add = 0) {

    if (length(mult) == 1L)
        mult <- rep(mult, nrow(x))
    if (length(add) == 1L)
        add <- rep(add, nrow(x))
    stopifnot(nrow(x) == length(mult) && nrow(x) == length(add))

    for (i in seq_len(nrow(x)))
        x[i, ] <- x[i, ] * mult[i] + add[i]
    return(x)
}
horizontal_mat_sum <- function(x, y) {
    stopifnot(identical(dim(x), dim(y)))
    for (i in seq_len(nrow(x)))
        x[i, ] <- x[i, ] + y[i, ]
    return(x)
}
