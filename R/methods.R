
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

    # dots <- dots_list(...)
    # if (any(lengths(dots) != 1L))
    #     stop("You can only index one value of an lp_var at a time")

    ind <- `[`(x$ind, ...) + x$previous_vars
    x$selected[] <- FALSE
    x$selected[ind] <- TRUE
    x$coef[!x$selected] <- 0
    x
}
#' @export
Ops.lp_var <- function(e1, e2) {

    fun <- match.fun(.Generic)

    if (.Generic %in% c("^", "%%", "%/%"))
        stop("Can't use operations '^', '%%', '%/%' in a linear problem")

    if ("lp_var" %in% class(e1)) {
        x <- e1
        x$coef[x$selected] <- fun(x$coef[x$selected], e2)
        x

    } else if (.Generic == "/") {
        stop("Can't divide by a variable in a linear problem")
    } else {
        x <- e2
        x$coef[x$selected] <- fun(e1, x$coef[x$selected])
        x
    }
}

#' @export
print.lp_con <- function(x) {

    cat("Linear Programming Constraint", if (length(x) > 1) "s", ":", sep = "")
    deparsed <- map_chr(x, deparse_constraint)

    for (k in seq_along(deparsed)) {
        cat("\n  ", deparsed[k], sep = "")
    }
}

