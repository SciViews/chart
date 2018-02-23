#' Create charts
#'
#' `chart()` provides a unified interface for base plots, lattice and ggplot2.
#'
#' @param data The dataset (a `data.frame` or `tibble`, usually).
#' @param specif Specification, being either `aes()`, or a formula.
#' @param formula A formula.
#' @param mapping An `aes()` object, as for [ggplot()].
#' @param ... Further arguments.
#' @param type The type of plot to produce.
#' @param env The environment where to evaluated the formula.
#'
#'
#' @details ....
#' @export
#' @name chart
#' @seealso [f_aes()], [ggplot()]
#' @keywords hplot
#' @concept R plots with graphics, lattice or ggplot2
#' @examples
#' # TODO..
chart <- function(data, ..., type = NULL, env = parent.frame()) {
  UseMethod("chart")
}

#' @export
#' @rdname chart
chart.default <- function(data, specif = NULL, formula = NULL, mapping = NULL, ...,
type = NULL, env = parent.frame()) {
  if (!missing(specif)) {
    if (specif %is% 'formula') {
      formula <- specif
    } else if (specif %is% 'uneval') {
      mapping <- specif
    } else abort("'specif' must be either a formula or aes()/f_aes()")
  }
  # Resolve formula first, if specified
  if (!is_null(formula)) {
    args <- as_list(match.call())[-1]
    args$data <- NULL
    args$specif <- NULL
    args$formula <- NULL
    args$mapping <- NULL
    args$env <- NULL
    mapping <- .rename_aes(.f_to_aes(formula, args, with.facets = TRUE))
    # If mapping is provided, use it to append (and possibly replace) formula items
    #    if (!is.null(mapping))
  }

  # Extract facets
  facets <- mapping$facets
  mapping$facets <- NULL
  # Create ggplot object
  p <- ggplot(data = data, mapping = mapping, environment = env)
  # Add facets, if provided
  if (!is_null(facets)) {
    if (is_null(f_lhs(facets))) {# facets like ~var
      p <- p + facet_wrap(facets)
    } else {# facets like var1 ~ var2
      p <- p + facet_grid(facets)
    }
  }
  # If type =="auto", automatically add a layer, like qplot() does
  if (!is_null(type)) {
    if (type == "auto") {
      mapping_names <- names(mapping)
      if ("sample" %in% mapping_names) {
        p <- p + geom_qq()
      } else if (!'y' %in% mapping_names) {
        x <- eval(mapping$x, p$data, env)
        if (.is_discrete(x)) {
          p <- p + geom_bar()
        } else {
          p <- p + geom_histogram() # TODO: select adequate bins!
        }
        #if (missing(ylab)) ylab <- "count"
      } else if (!'x' %in% mapping_names) {
        p <- p + geom_point(aes(x = bquote(seq_along(.(y)))))
      } else {
        p <- p + geom_point()
      }
    } else warn("Only type = NULL or type = 'auto' are recognized. Argument ignored")
    # TODO: use geom_<type>() instead
  }
  p
  #  } else stop("Objects of type ", paste(class(object), collapse = "/"),
  #    " not supported by chart().", call. = FALSE)
}
