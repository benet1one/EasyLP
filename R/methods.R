
is_lp_var <- function(x) {
    "lp_var" %in% class(x)
}
is_lp_con <- function(x) {
    "lp_con" %in% class(x)
}

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
length.lp_var <- function(x) length(x$ind)
#' @export
dim.lp_var <- function(x) dim(x$ind)
#' @export
`[.lp_var` <- function(x, ...) {

    # dots <- dots_list(...)
    # if (any(lengths(dots) != 1L))
    #     stop("You can only index one value of an lp_var at a time")

    old_ind <- x$ind
    x$ind <- `[`(x$ind, ..., drop = FALSE)
    x$selected[] <- FALSE
    x$selected[x$ind] <- TRUE

    rows <- is.element(old_ind, x$ind)
    x$coef <- x$coef[rows, ]
    x$add  <- x$add[rows]
    return(x)
}

# questioning
horizontal_multiply <- function(x, mult) {

    if (length(mult) == 1L)
        mult <- rep(mult, nrow(x))

    stopifnot(nrow(x) == length(mult))

    for (i in seq_len(nrow(x)))
        x[i, ] <- x[i, ] * mult[i]
    return(x)
}
horizontal_mat_sum <- function(x, y) {

    if (nrow(x) == 1L)
        x <- x[rep(1L, nrow(y)), , drop = FALSE]
    if (nrow(y) == 1L)
        y <- y[rep(1L, nrow(x)), , drop = FALSE]

    stopifnot(identical(dim(x), dim(y)))
    return(x + y)
}

#' @export
Ops.lp_var <- function(e1, e2) {
    arith <- c("+", "-", "*", "/", "^", "%%", "%/%")
    logic <- c("&", "|", "!")
    compare <- c("==", "!=", "<", "<=", ">=", ">")

    if (is.element(.Generic, logic))
        Logic_lp_var(e1, maybe_missing(e2), .Generic)
    else if (is.element(.Generic, arith))
        Arith_lp_var(e1, maybe_missing(e2), .Generic)
    else if (is.element(.Generic, compare))
        Compare_lp_var(e1, maybe_missing(e2), .Generic)
    else stop("huh?")
}
Logic_lp_var <- function(e1, e2, .Generic) {
    if (.Generic != "!")
        stop("Operations '&', '|' not supported.")
    x <- e1
    if (!x$binary)
        stop("Logical negation '!' only supported on binary variables.")
    return(-x + 1)
}
Arith_lp_var <- function(e1, e2, .Generic) {

    if (.Generic %in% c("^", "%*%", "%%", "%/%"))
        stop("Can't use operations '^', '%%', '%/%' in a linear problem")

    if (missing(e2)) {
        # +x or -x
        if (.Generic == "-") e1$coef <- -e1$coef
        return(e1)

    } else if (is_lp_var(e1) && !is_lp_var(e2)) {
        if (.Generic == "*") {
            e1$coef <- horizontal_multiply(e1$coef, e2)
            e1$add  <- e1$add * e2
        } else if (.Generic == "/") {
            e1$coef <- horizontal_multiply(e1$coef, 1/e2)
            e1$add  <- e1$add / e2
        } else if (.Generic == "+") {
            e1$add <- e1$add + e2
        } else if (.Generic == "-") {
            e1$add <- e1$add - e2
        } else stop("Operation not allowed")

        return(e1)

    } else if (is_lp_var(e1) && is_lp_var(e2)) {
        if (.Generic == "+") {
            e1$coef <- horizontal_mat_sum(e1$coef, e2$coef)
            e1$add  <- e1$add + e2$add
        } else if (.Generic == "-") {
            e1$coef <- horizontal_mat_sum(e1$coef, -e2$coef)
            e1$add  <- e1$add - e2$add
        } else stop("Can't multiply or divide variables in a linear problem")

        return(e1)

    } else if (.Generic == "/") {
        stop("Can't divide by a variable in a linear problem")

    } else if (!is_lp_var(e1) && is_lp_var(e2)) {
        if (.Generic == "*") {
            return(e2 * e1)
        } else if (.Generic == "+") {
            return(e2 + e1)
        } else if (.Generic == "-") {
            e2$coef <- -e2$coef
            return(e2 + e1)
        }
    }

    stop("what?")
}
Compare_lp_var <- function(e1, e2, .Generic) {

    if (is_lp_var(e2)) {
        x <- e1 - e2
        rhs <- 0
    } else {
        x <- e1
        rhs <- e2
    }

    if (length(rhs) == 1L)
        rhs <- rep(rhs, nrow(x$coef))
    stopifnot(length(rhs) == nrow(x$coef))

    rhs <- rhs - x$add
    dir <- .Generic
    if (dir == "!=")
        stop("Inequality '!=' not allowed in linear problems")

    dir <- rep(dir, length(rhs))
    list(
        mat = x$coef,
        dir = dir,
        rhs = rhs
    ) |> structure(class = "lp_con")
}
#' @export
sum.lp_var <- function(x, ..., na.rm = FALSE) {
    check_dots_empty(error = "Function 'sum' only supports one variable.")
    x$coef <- matrix(colSums(x$coef), nrow = 1L)
    x$add <- sum(x$add)
    return(x)
}

# #' @export
# print.lp_con <- function(x) {
#
#     cat("Linear Programming Constraint", if (length(x) > 1) "s", ":", sep = "")
#     deparsed <- map_chr(x, deparse_constraint)
#
#     for (k in seq_along(deparsed)) {
#         cat("\n  ", deparsed[k], sep = "")
#     }
# }

