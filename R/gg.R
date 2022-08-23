#' Make a ggplot function pipeable
#'
#' @description The function [gg()] should be used like this: `gg$geom_point()`.
#' It transforms on the fly an original {ggplot2} function supposed to be used
#' with the `+` operator (like `p + geom_point()`) into a pipeable version
#' (like `p %>% gg$geom_point()`).
#'
#' @param ggplot An object of class "ggplot" (or "theme").
#' @param ... Further arguments passed to the the ggplot function (see Details).
#' @param x The `gg()`function.
#' @param name The name of the ggplot function to make pipeable.
#' @param pattern A regular expression to list matching names.
#'
#' @return The `gg()` function just returns an error message. When subsetted
#' with the name of a {ggplot2} function (e.g., `gg$geom_point()`), it
#' returns a modified version of that function in such a way that it can be
#' used with a pipe operator.
#'
#' @details The function returned by `gg$fun` is a modified version of the
#' function `fun` where a first argument `ggplot =` is added, and the
#' instruction `ggplot + ...` is added in its body. A message
#' is also added in the body to explicitly warn about these changes. All the
#' other arguments of `fun` remain valid and should keep their original meaning.
#'
#' The changes are done on the fly, and the original function `fun` is **not**
#' altered anywhere else (and in particular, no alteration is done in a package
#' or a namespace). When using this construct, make sure that: (1) you
#' understand what is done, (2) you are aware that you use an altered version of
#' the original function, (3) a bug or strange behavior may occur due to the
#' patch and the original author of the function is not responsible in this case
#' (the problem must be reported to the author of [gg()] and the maintainer of
#' the present package instead), and (4) the patched function exhibits an
#' additional argument and behaves differently to what is described in the help
#' page of the original, non-patched, function!
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' data(iris)
#' ggplot(aes(x = Petal.Length, y = Sepal.Length, col = Species) , data = iris) |>
#'   gg$geom_point() |>
#'   gg$labs(x = "Sepal length (mm)", y = "Petal length (mm)")
#' # Also try completion with gg$<tab>
gg <- structure(function(ggplot, ...) {
  stop("You must indicate gg$<something>(), like gg$geom_point() or gg$labs()")
}, class = c("subsettable_gg", "function"))

.make_pipeable_gg <- function(fun) {
  if (is.function(fun)) {
    f <- fun
  } else {
    fun_name <- as.character(fun)
    f <- get0(fun_name, envir = parent.frame(2), mode = "function",
      inherits = TRUE)
    if (is.null(f))
      stop("Cannot found function '", fun_name, "'")
  }
  # Change arguments to prepend ggplot =
  formals(f) <- c(pairlist(ggplot = NULL), formals(f))
  # Change body: prepend ggplot + {...}
  patched_body <- body(function(ggplot, ...) {
    "Warning: this is a patched version of a ggplot function to make it pipeable. Please, read ?gg first!"
    ggplot + 1
  })
  patched_body[[3]][[3]] <- body(f)
  body(f) <- patched_body
  f
}

#' @export
#' @rdname gg
#' @method $ subsettable_gg
`$.subsettable_gg` <- function(x, name) {
  if (name == "title")
    name <- "ggtitle"
  .make_pipeable_gg(name)
}

#' @export
#' @rdname gg
#' @method .DollarNames subsettable_gg
.DollarNames.subsettable_gg <- function(x, pattern = "") {
  l <- list("annotate", "expand_limits", "guides", "labs", "lims", "theme",
    "title", # Instead of ggtitle (special case!)
    "xlab", "xlim", "ylab", "ylim")
  l <- grep(pattern = pattern, l, value = TRUE)
  l <- c(l, apropos(paste0("^geom_", pattern)))
  l <- c(l, apropos(paste0("^annotation_", pattern)))
  l <- c(l, apropos(paste0("^coord_", pattern)))
  l <- c(l, apropos(paste0("^facet_", pattern)))
  l <- c(l, apropos(paste0("^scale_", pattern)))
  l <- c(l, apropos(paste0("^stat_", pattern)))
  l <- c(l, apropos(paste0("^theme_", pattern)))
  sort(l)
}

# TODO: allow custom extensions to this list
# (trials that do not work for now)
# gg_help_handler <- function(type = c("completion", "parameter", "url"),
#   topic, source, ...) {
#   type <- match.arg(type)
#   if (type == "completion") {
#     # Just a trial
#     list(title = "title", signature = "function(x, y)", returns = "1",
#       description = "desc", details = "details", sections = "sections")
#   } else if (type == "parameter") {
#     list(args = c("x", "y"), arg_descriptions = c("x param", "y param"))
#   } else if (type == "url") {
#     "https://wp.sciviews.org"
#   }
# }
#
# .DollarNames.subsettable_gg <- function(x, pattern = "") {
#   l <- list("annotate", "expand_limits", "guides", "labs", "lims", "theme",
#     "title", # Instead of ggtitle (special case!)
#     "xlab", "xlim", "ylab", "ylim")
#   l <- grep(pattern = pattern, l, value = TRUE)
#   l <- c(l, apropos(paste0("^geom_", pattern)))
#   l <- c(l, apropos(paste0("^annotation_", pattern)))
#   l <- c(l, apropos(paste0("^coord_", pattern)))
#   l <- c(l, apropos(paste0("^facet_", pattern)))
#   l <- c(l, apropos(paste0("^scale_", pattern)))
#   l <- c(l, apropos(paste0("^stat_", pattern)))
#   l <- c(l, apropos(paste0("^theme_", pattern)))
#   # This works and my object is recognized as a function, but with only ... arg
#   #l <- paste0(sort(l), "(")
#   # This does not work!
#   #attr(l, "types") <- rep(6, length.out = length(l))
#   # This does not work!
#   #attr(l, "helpHandler") <- gg_help_handler
#   l
# }
