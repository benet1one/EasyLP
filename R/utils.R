
inside <- function(expr) {
    stopifnot(is_expression(expr) || is_call(expr))
    if (expr[[1L]] == quote(`{`) || expr[[1L]] == quote(`(`))
        inside(expr[[2L]])
    else
        expr
}
parse_bound <- function(bound) {

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
bound_to_constraints <- function(x) {
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

parse_constraint <- function(constraint, ...) {

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
deparse_constraint <- function(constraint) {
    map(constraint, format) |> do.call(what = paste)
}
is_constraint_atomic <- function(constraint) {
    is.element("atomic_lp_con", class(constraint))
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

    result
}

join_lp_vars <- function(varlist) {

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

name_constraint <- function(constraint, name) {
    if (name == "")
        return(constraint)
    if (nrow(constraint$mat) > 1L)
        name <- paste0(name, " [", 1:nrow(constraint$mat), "]")
    rownames(constraint$mat) <- name
    constraint
}
