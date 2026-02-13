#' 'SciViews::R' - Unified Interface (with Formula) for R Plots
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

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
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
#' @importFrom ggplotify as.ggplot
#' @importFrom ggpubr ggarrange
#' @importFrom svBase aka section label
## usethis namespace: end
"_PACKAGE"

