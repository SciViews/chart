#' Create aes()thetics from formula for ggplot2
#'
#' This function allows to use a formula interface directly with **ggplot2**, or
#' **chart**.
#'
#' @param formula A formula.
#' @param ... Further aesthetics to set (like `size`, `colour`, et.)
#' @param with.facets Do we create special (non-ggplot2) aesthetics for facets
#' (no by default)?
#'
#' @return An aesthetic object of class `uneval`, as those obtained with [aes()].
#' @export
#'
#' @examples
#' # TODO...
f_aes <- function(formula, ..., with.facets = FALSE) {
  args <- as_list(match.call())[-1]
  args$formula <- NULL
  .rename_aes(.f_to_aes(formula, args, with.facets = with.facets))
}

.f_get_args <- function(expr, args = list(x = expr), replace = FALSE) {
  if (!is_call(expr) || length(expr) < 2)
    return(args)

  # Check if operator is like `%name=%` or `%name =%`
  if (grepl("^%[^=]+=%$", op <- expr[[1]])) {# This is an argument
    # Record argument
    name <- trimws(substr(op, 2, nchar(op) - 2))
    if (replace || all(names(args) != name))
      args[[name]] <- expr[[3]]
    # Update expression
    args$x <- expr <- expr[[2]]
    return(.f_get_args(expr, args, replace = replace))
  }

  # Also look for last item in the expression
  l <- length(expr)
  subexpr <- expr[[l]]
  # Check if operator is like `%name=%` or `%name =%`
  if (length(subexpr) == 3 && grepl("^%[^=]+=%$", op <- subexpr[[1]])) {
    # Record argument
    name <- trimws(substr(op, 2, nchar(op) - 2))
    if (replace || all(names(args) != name))
      args[[name]] <- subexpr[[3]]
    # Update expression
    expr[[l]] <- subexpr[[2]]
    args$x <- expr
    args <- .f_get_args(expr, args, replace = replace)
  }
  args
}

.f_to_aes <- function(formula, args = list(), with.facets = FALSE) {
  if (missing(formula))
    abort("'formula' must be provided")
  # Convert formula into x and y aes() arguments
  x <- f_rhs(formula)
  y <- f_lhs(formula)
  # Possibly get facets from y ~ x | facets
  if (is_call(x) && x[[1]] == '|') {# Extract facets as a formula
    # If facets already exists, do not replace it (provided in the args)
    if (all(names(args) != "facets")) {
      facets <- x[[3]]
      # Could be either w, or w * z
      if (is_name(facets)) {
        facets <- as_call(list(quote(`~`), facets))
      } else if (is_call(facets) && facets[[1]] == '*') {
        facets[[1]] <- quote(`~`)
      }
      args$facets <- as_formula(facets, env = f_env(formula))
    }
    x <- x[[2]]
  }
  # Do we set x?
  if (all(names(args) != "x")) args$x <- x
  # Also get y
  if (all(names(args) != "y")) args$y <- f_lhs(formula)
  # Further decompose 'x' to get col, size, fill, alpha, ... from formula
  args <- .f_get_args(args$x, args, replace = FALSE)
  # Are we autorized to use facetting in the formula?
  if (!is_true(with.facets) && !is_null(args$facets))
    abort("Facets are specified but are not autorized in this context (use + facet_grid() or + facet_wrap() instead, or force it using with.facets = TRUE)")
  # The result of aes() in an 'uneval' object
  class(args) <- "uneval"
  args
}
