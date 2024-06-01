
require(rlang)
require(purrr)

# questioning
lp_set <- function(...) {
    structure(c(...), class = "lp_set")
}

#' Define a variable for a linear problem.
#' @description
#' Creates an object of class 'lp_var' to be used in lp_solve.
#'
#' @param ... Sets where the variables lives. Must be \link{lp_set} objects.
#' The order is important, as it defines the shape of the array:
#' \code{dim(x) = c(length(set_1), length(set_2), ...)}
#'
#' @param bound Inequality of the shape \code{.x >= 0, .x <= 2},
#' or \code{NULL} if it's free.
#'
#' @param integer Is the variable an integer?
#' @param binary Is the variable binary {0, 1}?
#'
#' @return Object of class 'lp_var'
#' @export
#'
#' @examples
#' # To do
lp_var <- function(name, ..., bound = NULL, integer = FALSE, binary = FALSE) {

    stopifnot(is_scalar_character(name))
    stopifnot(is_scalar_logical(integer))
    stopifnot(is_scalar_logical(binary))

    if (integer && binary)
        message("If binary = TRUE, you don't need to specify integer = TRUE")
    if (binary)
        integer <- TRUE

    if (...length() == 0L) {
        sets <- list(scalar = 1)
    } else {
        sets <- list(...)
        if (any(names(sets) == ""))
            stop("All sets in ... must be named")
    }

    # grid <- do.call(expand.grid, sets)
    # x_name <- pmap(grid, paste, sep = "-")
    ind <- array(dim = lengths(sets), dimnames = sets)
    ind[] <- 1:length(ind)

    bound <- enexpr(bound)
    if (!is_syntactic_literal(bound)) {
        bound <- parse_bound(bound)
        if (binary) {
            warning("Should not specify bound for binary variables.")
            bound <- NULL
        }
    } else {
        bound <- NULL
    }

    list(
        name = name,
        sets = sets,
        ind = ind,
        bound = bound,
        integer = integer,
        binary = binary,
        previous_vars = 0L,
        n_vars = length(ind),
        selected = rep(TRUE, length(ind)),
        coef = rep(1, length(ind))
    ) |> structure(class = "lp_var")
}
lp_variable <- lp_var

#' Title
#'
#' @param objective_fun
#'
#' @return
#' @export
#'
#' @examples
lp_obj <- function(objective_fun) {
    enexpr(objective_fun)
}

#' Define a constraint for a linear problem.
#' @description
#' Creates one or more linear constraints
#'
#' @param constraint In the form of \code{2*x - y <= b}. The variables
#' must always be on the left hand side. The inequality can be one of:
#' \code{<=, <, ==, >, >=}
#' @param ... Abbreviations for variables. See examples.
#'
#' @details
#' If the left hand side of the constraint is not atomic, the values are summed.
#' For example, \code{lp_con(x[1,] <= 10)} means 'the sum of the first row
#' of x is less than 10.'
#'
#' If you want the constraint to be 'all values in the first row of x must
#' be less than 10' you need to use this syntax:
#' \code{lp_con( for (j in 1:ncol(x)) x[1, j] <= 10)}.
#'
#' @return Object of class 'lp_con'
#' @export
#'
#' @examples
#' product <- c("apple", "orange", "lemon")
#' stock <- c(apple = 150, orange = 120, lemon = 60)
#' x <- lp_var("sales", product = product, integer = TRUE)
#'
#' # Max stock for each product
#' max_stock <- lp_con( for (p in product[1:2]) x[p] <= stock[p])
#' max_stock
#'
#' # 'apple' + 'orange' >= 240
#' min_total <- lp_con(x[p] >= 240, p = product[1:2])
#' min_total
#'
#' # Notice how 'p' serves very different purposes in the two cases.
lp_con <- function(constraint, ...) {

    constraint <- enexpr(constraint)
    constraint <- inside(constraint)

    con <- if (constraint[[1L]] == quote(`for`)) {
        browser()
        for_split(constraint) %>% map(~parse_constraint(.x, ...))
    } else {
        list(parse_constraint(constraint, ...))
    }

    structure(con, class = "lp_con")
}
lp_constraint <- lp_con

