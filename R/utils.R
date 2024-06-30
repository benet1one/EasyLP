
.parse_bound <- function(bound) {

    bound <- inside(bound)
    inequality <- switch(
        bound[[1L]] |> format(),
        ">=" = +1,
        ">"  = +1,
        "<=" = -1,
        "<"  = -1,
        stop("bound must be an inequality of shape '.x >= 0' or '.x <= 2'")
    )

    lhs_x <- bound[[2L]] == quote(.x)
    if (!lhs_x) {
        if (bound[[3L]] != quote(.x))
            stop("bound must be an inequality of shape '.x >= 0' or '.x <= 2'")
        inequality = -inequality
    }

    val <- if (lhs_x) 3L  else 2L

    list(
        rhs = eval.parent(bound[[val]], n = 2L),
        dir = if (inequality == 1L) ">="  else "<="
    )
}
.bound_to_constraints <- function(x) {
    ind <- (x$previous_vars + 1) : length(x$ind)
    mat <- matrix(0L, nrow = length(ind), ncol = x$n_vars)

    for (i in seq_along(ind))
        mat[i, ind[i]] <- 1L

    list(
        mat = mat,
        dir = rep(x$bound$dir, nrow(mat)),
        rhs = rep(x$bound$rhs, nrow(mat))
    )
}

.parse_constraint <- function(constraint, ...) {

    constraint <- inside(constraint)
    dir <- constraint[[1L]] |> format()

    if (constraint[[1L]] == quote(`for`)) {

        multiple_con <- map(for_split(constraint), parse_constraint, ...)

        if (!is_constraint_atomic(multiple_con[[1L]]))
            multiple_con <- unlist(multiple_con, recursive = FALSE)

        return(multiple_con)
    }

    if (!is.element(dir, c("<=", "<", ">=", ">", "==")))
        stop("Constraint does not contain inequality.")

    env <- as_environment(list(...), parent = parent.frame())

    list(
        lhs = substituteDirect(constraint[[2L]], env),
        dir = dir,
        rhs = substituteDirect(constraint[[3L]], env)
    ) %>% structure(class = "atomic_lp_con")
}
.deparse_constraint <- function(constraint) {
    map(constraint, format) |> do.call(what = paste)
}
.is_constraint_atomic <- function(constraint) {
    is.element("atomic_lp_con", class(constraint))
}

.join_lp_vars <- function(varlist) {

    nams <- map_chr(varlist, ~.x$name)
    if (anyDuplicated(nams))
        stop("Variables must have unique names")

    pv <- 0L

    for (k in seq_along(varlist)) {
        varlist [[k]] $ previous_vars <- pv
        pv <- pv + length(varlist[[k]])
    }

    total <- pv

    for (k in seq_along(varlist)) {
        x <- varlist [[k]]
        pv <- x$previous_vars
        varlist [[k]] $ ind <- x$ind + pv
        varlist [[k]] $ selected [] <- FALSE
        varlist [[k]] $ selected [x$ind] <- TRUE
        varlist [[k]] $ coef <- cbind(
            matrix(0, nrow = nrow(x$coef), ncol = pv),
            x$coef,
            matrix(0, nrow = nrow(x$coef), ncol = total - (pv + length(x)))
        )
        # varlist [[k]] $ add <-
    }

    names(varlist) <- nams
    varlist
}

.some_combinations <- function (x, m, max_combn = 1000L) {
    stopifnot(length(m) == 1L, is.numeric(m))
    if (m < 0)
        stop("m < 0", domain = NA)
    if (is.numeric(x) && length(x) == 1L && x > 0 && trunc(x) == x)
        x <- seq_len(x)
    n <- length(x)
    if (n < m)
        stop("n < m", domain = NA)
    x0 <- x
    m <- as.integer(m)
    e <- 0
    h <- m
    a <- seq_len(m)

    r <- x[a]
    len.r <- length(r)
    count <- min(as.integer(round(choose(n, m))), max_combn)

    dim.use <- c(m, count)
    out <- matrix(r, nrow = len.r, ncol = count)
    i <- 2L
    nmmp1 <- n - m + 1L

    while (a[1L] != nmmp1  &&  i <= count) {
        if (e < n - h) {
            h <- 1L
            e <- a[m]
            j <- 1L
        }
        else {
            e <- a[m - h]
            h <- h + 1L
            j <- 1L:h
        }
        a[m - h + j] <- e + j
        r <- x[a]
        out[, i] <- r
        i <- i + 1L
    }

    dim(out) <- dim.use
    out
}

inside <- function(expr) {
    if (is_symbol(expr))
        return(expr)
    stopifnot(is_expression(expr) || is_call(expr))
    if (expr[[1L]] == quote(`{`) || expr[[1L]] == quote(`(`))
        inside(expr[[2L]])
    else
        expr
}
for_split <- function(expr) {

    if (expr[[1L]] != quote(`for`))
        stop("Expression is not wrapped in a for loop")

    sequence <- expr[[3L]] |> eval()
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

parse_variable_definition <- function(expr) {

    expr <- inside(expr)

    if (is_character(expr))
        return(list(name = expr, bound = c(-Inf, +Inf), type = "real"))
    if (is_symbol(expr))
        return(list(name = format(expr), bound = c(-Inf, +Inf), type = "real"))
    stopifnot(is_call(expr))

    bound <- c(-Inf, +Inf)

    if (is_call(expr)) {
        lesser <- c("<", "<=")
        greater <- c(">", ">=")
        wrapper <- format(expr[[1L]])

        if (wrapper %in% lesser) {
            rhs <- eval.parent(expr[[3L]])
            stopifnot(is.numeric(rhs))
            bound[2L] <- rhs
            expr <- expr[[2L]]

        } else if (wrapper %in% greater) {
            rhs <- eval.parent(expr[[3L]])
            stopifnot(is.numeric(rhs))
            bound[1L] <- rhs
            expr <- expr[[2L]]
        }
    }

    type <- "real"

    if (is_call(expr)) {
        int <- c("int", "integer")
        bin <- c("bin", "binary", "logical")
        wrapper <- format(expr[[1L]])

        if (wrapper %in% int) {
            type <- "integer"
            expr <- expr[[2L]]

        } else if (wrapper %in% bin) {
            if (bound[1L] != -Inf || bound[2L] != +Inf) {
                warning("Ignoring bound for binary variable")
                bound <- c(-Inf, +Inf)
            }
            type <- "binary"
            expr <- expr[[2L]]
        }
    }

    if (is_symbol(expr)) {
        name <- format(expr)
    } else {
        name <- eval.parent(expr)
    }

    list(name = name, type = type, bound = bound)
}

name_variable <- function(name, sets) {
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
