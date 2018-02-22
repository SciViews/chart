#' Chart, unified interface for R plots
#'
#' Unification of base plots, lattice and ggplot2, providing a formula interface
#' for all three plot engines.
#'
#' @section Important functions:
#'
#'- [chart()] constructs a `Chart` object.
#'
#' @docType package
#' @name chart-package
#'
#' @import ggplot2
#' @importFrom rlang abort warn f_env f_lhs f_rhs is_true
#' @importFrom stats as.formula
#' @importFrom plyr rename
NULL


# Non-exported functions --------------------------------------------------

`%is%` <- function(x, what) # This is more expressive!
  inherits(x, what)

is_call <- is.call

as_call <- as.call

is_name <- is.name

is_null <- is.null # rlang::is_null is much slower!

is_factor <- is.factor

is_character <- is.character

is_logical <- is.logical

as_list <- as.list

as_formula <- as.formula

# ggplot2:::is.discrete is not exported. So, I have to clone it here
.is_discrete <- function(x) {
  is_factor(x) || is_character(x) || is_logical(x)
}

# ggplot2:::rename_aes() is unfortunately not exported...
# This is a copy from ggplot2 2.2.1
.rename_aes <- function(x) {
  full <- match(names(x), .all_aesthetics)
  names(x)[!is.na(full)] <- .all_aesthetics[full[!is.na(full)]]
  plyr::rename(x, .base_to_ggplot, warn_missing = FALSE)
}

.all_aesthetics <- c("adj", "alpha", "angle", "bg", "cex", "col", "color",
  "colour", "fg", "fill", "group", "hjust", "label", "linetype", "lower", "lty",
  "lwd", "max", "middle", "min", "pch", "radius", "sample", "shape", "size",
  "srt", "upper", "vjust", "weight", "width", "x", "xend", "xmax", "xmin",
  "xintercept", "y", "yend", "ymax", "ymin", "yintercept", "z")

.base_to_ggplot <- c(col = "colour", color = "colour", pch = "shape",
  cex = "size", lty = "linetype", lwd = "size", srt = "angle", adj = "hjust",
  bg = "fill", fg = "colour", min = "ymin", max = "ymax")
