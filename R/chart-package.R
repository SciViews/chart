#' Unified Interface (with Formula) for R Plots
#'
#' Unification of base plots, lattice and ggplot2, providing a single interface
#' for all three plot engines.
#'
#' @section Important functions:
#'
#'- [chart()] constructs a **Chart** object.
#'
#'- [combine_charts()] combines multiple **Chart** objects into a single plot.
#'
#'- [f_aes()] creates a formula for aesthetics mapping (use it instead of [ggplot2::aes()].
#'
#' @docType package
#' @name chart-package

## usethis namespace: start
#' @import grDevices
# @importFrom grDevices hcl colorRampPalette dev.off palette
#' @import graphics
# @importFrom graphics, par
#' @import lattice
#' @importFrom latticeExtra custom.theme ggplot2like ggplot2like.opts
#' @import ggplot2
#' @importFrom viridis viridis_pal
#' @importFrom scales hue_pal gradient_n_pal
#' @importFrom cowplot theme_cowplot
#' @importFrom rlang abort warn env_label f_env f_lhs f_rhs is_true is_quosure
#' @importFrom stats as.formula asOneSidedFormula
#' @importFrom utils .DollarNames apropos modifyList
#' @importFrom pryr modify_lang
#' @importFrom data.io label
#' @importFrom ggplotify as.ggplot
#' @importFrom ggpubr ggarrange
#' @importFrom svMisc aka section
## usethis namespace: end
NULL

# Non-exported functions --------------------------------------------------

.onAttach <- function(libname, pkgname) {
  # Don't load themes automatically, but use them plot by plot instead
  #ggplot2::theme_set(theme_sciviews())
  #theme_sciviews_lattice()
  #theme_sciviews_graphics()
  # Use viridis continuous palettes by default with ggplot2 charts
  if (is.null(getOption("ggplot2.continuous.fill")))
    options(ggplot2.continuous.fill = "viridis")
  if (is.null(getOption("ggplot2.continuous.colour")))
    options(ggplot2.continuous.colour = "viridis")
}

`%is%` <- function(x, what) # This is more expressive!
  inherits(x, what)

is_call <- is.call

as_call <- as.call

is_name <- is.name

is_null <- is.null # rlang::is_null is much slower!

is_factor <- is.factor

is_character <- is.character

as_character <- as.character

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
  .rename(x, .base_to_ggplot)
}

# This is plyr::rename(), but since plyr seems deprecated in favor of dplyr and
# purrr, we don't want to depend on it... So, this is our own version
.rename <- function(x, replace, warn_duplicated = TRUE) {
  names <- names(x)
  new_names <- as_character(replace[names])
  not_replaced <- is.na(new_names)
  new_names[not_replaced] <- names[not_replaced]
  duplicated_names <- new_names[duplicated(new_names)]
  if (warn_duplicated && length(duplicated_names)) {
    if (length(duplicated_names) == 1) {
      duplicated_names_message <- paste0(
        "Found duplicate for the name `",
        duplicated_names, "`")
    } else {
      duplicated_names_message <- paste0("`", duplicated_names,
        "`", collapse = ", ")
      duplicated_names_message <- paste0(
        "Found duplicates for the following names: (",
      duplicated_names_message, ")")
    }
    warn(duplicated_names_message)
  }
  names(x) <- new_names
  x
}

.all_aesthetics <- c("adj", "alpha", "angle", "bg", "cex", "col", "color",
  "colour", "fg", "fill", "group", "hjust", "label", "linetype", "lower", "lty",
  "lwd", "max", "middle", "min", "pch", "radius", "sample", "shape", "size",
  "srt", "upper", "vjust", "weight", "width", "x", "xend", "xmax", "xmin",
  "xintercept", "y", "yend", "ymax", "ymin", "yintercept", "z")

.base_to_ggplot <- c(col = "colour", color = "colour", pch = "shape",
  cex = "size", lty = "linetype", lwd = "size", srt = "angle", adj = "hjust",
  bg = "fill", fg = "colour", min = "ymin", max = "ymax")
