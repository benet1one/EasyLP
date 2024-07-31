
is_lp_var <- function(x) {
    inherits(x, "lp_var")
}
is_lp_con <- function(x) {
    inherits(x, "lp_con")
}
is_for_split <- function(x) {
    inherits(x, "ForSplit")
}

#' @export
print.lp_var <- function(x, ...) {

    if (!x$raw) {
        print(x$coef)
        return(x)
    }

    cat("Linear Programming Variable '", x$name, "'", sep = "")

    if (x$binary)
        cat(" <binary>")
    else if (x$integer)
        cat(" <integer>")

    if (length(x$ind) > 1L)
        cat("\nWith sets: [", paste(names(x$sets), collapse = ", "), "]")

    if (all(x$bound != c(-Inf, +Inf))) {
        cat("\n")
        cat(x$bound[1L], "<=", x$name, "<=", x$bound[2L])
    } else if (x$bound[1L] != -Inf) {
        cat("\n", x$name, " >= ", x$bound[1L], sep = "")
    } else if (x$bound[2L] != +Inf) {
        cat("\n", x$name, " <= ", x$bound[2L], sep = "")
    }
    cat("\n")
    return(x)
}
#' @export
length.lp_var <- function(x) length(x$ind)
#' @export
dim.lp_var <- function(x) dim(x$ind)
#' @export
dimnames.lp_var <- function(x) dimnames(x$ind)
#' @export
`[.lp_var` <- function(x, ...) {

    if (!x$indexable)
        stop("Cannot index this result.")

    old_ind <- x$ind
    x$ind <- `[`(x$ind, ..., drop=FALSE)

    if (anyNA(x$ind))
        stop("Variable ", format(enexpr(x)), " was wrongly indexed.")

    x$selected[] <- FALSE
    x$selected[x$ind] <- TRUE
    x$raw <- FALSE

    rows <- is.element(old_ind, x$ind)
    x$coef <- x$coef[rows, , drop = FALSE]
    x$add  <- x$add[rows]
    return(x)
}

#' @export
print.lp_con <- function(x, ...) {
    mat <- with(x, cbind(mat, dir=dir, rhs=rhs))
    print(mat, quote = FALSE)
}

horizontal_multiply <- function(x, mult) {

    if (nrow(x) == 1L)
        x <- x[rep(1L, length(mult)), , drop=FALSE]
    if (length(mult) == 1L)
        mult <- rep(mult, nrow(x))

    if (nrow(x) != length(mult))
        stop("Linear variable must have the same length as multiplier ",
             "Only values of size one are recycled.")


    for (i in seq_len(nrow(x)))
        x[i, ] <- x[i, ] * mult[i]
    return(x)
}
horizontal_mat_sum <- function(x, y) {

    if (nrow(x) == 1L)
        x <- x[rep(1L, nrow(y)), , drop = FALSE]
    if (nrow(y) == 1L)
        y <- y[rep(1L, nrow(x)), , drop = FALSE]

    if (nrow(x) != nrow(y))
        stop("Linear variables must have the same length. ",
             "Only values of size one are recycled.")

    stopifnot(identical(dim(x), dim(y)))
    return(x + y)
}

#' @export
Ops.lp_var <- function(e1, e2) {
    arith <- c("+", "-", "*", "/", "^", "%%", "%/%")
    logic <- c("&", "|", "!")
    compare <- c("==", "!=", "<", "<=", ">=", ">")

    if (is.element(.Generic, logic)) {
        Logic_lp_var(e1, maybe_missing(e2), .Generic)

    } else if (is.element(.Generic, arith)) {
        x <- Arith_lp_var(e1, maybe_missing(e2), .Generic)
        check_na <- c(x$coef, x$add, x$selected)
        if (anyNA(check_na)) {
            e1 <- enexpr(e1) |> format()
            e2 <- enexpr(e2) |> format()
            stop("Operation '", e1, " ", .Generic, " ", e2,
                 "' resulted in NA values")
        }

        x$raw <- FALSE
        return(x)

    } else if (is.element(.Generic, compare)) {
        Compare_lp_var(e1, e2, .Generic)

    } else stop("huh?")
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
        } else stop("Operation not allowed.")

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
Math.lp_var <- function(x, ...) {
    if (.Generic == "abs")
        stop("Function 'abs' is not linear. ",
             "See how to use absolute values in linear programming here:\n",
             "https://optimization.cbe.cornell.edu/index.php?title=Optimization_with_absolute_values")
    if (.Generic != "cumsum")
        stop("Function '", .Generic, "' is not linear.")

    x$add <- cumsum(x$add)
    if (nrow(x$coef) >= 2L)  for (i in 2:nrow(x$coef))
        x$coef[i, ] <- x$coef[i, ] + x$coef[i-1L, ]

    x$raw <- FALSE
    return(x)
}
#' @export
sum.lp_var <- function(x, ..., na.rm = FALSE) {
    warn_changed_args(na.rm = FALSE, .suffix = " for linear variables.")
    dots <- dots_list(...)
    if (length(dots) != 0L) {
        x_summed <- sum(x)
        dots_summed <- lapply(dots, sum)
        return(Reduce(`+`, dots_summed, x_summed))
    }
    x$coef <- matrix(colSums(x$coef), nrow = 1L)
    x$add <- sum(x$add)
    x$indexable <- FALSE
    x$raw <- FALSE
    return(x)
}
#' @export
mean.lp_var <- function(x, ...) {
    check_dots_empty()
    sum(x) / length(x)
}
#' @export
weighted.mean.lp_var <- function(x, w, ...) {
    check_dots_empty()
    if (length(w) != length(x)) stop("'x' and 'w' must have the same length")
    sum((x * w)) / sum(w)
}

