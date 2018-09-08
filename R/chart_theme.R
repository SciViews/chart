#' Select chart theme
#'
#' Lattice and ggplot2 provide themes for their plots, while with base R plots,
#' one specifies appearance through [par()]. `chart_theme()` tries to get the
#' plot appearance as uniform as possible between the three plot engines. So,
#' setting themes this way change the appearance of all three kinds of plots.
#'
#' @param theme The theme to apply (character string).
#' @param font_size The default size font for this theme.
#' @param font_family The default font family in this theme.
#' @param line_size The default line size in this theme.
#' @param ... Arguments passed to [ggplot2like()], the most used being `n=` for
#' the number of colors to generate in the palette.
#'
#' @export
#' @seealso [chart()], [theme()], [trellis.par.set()]
#' @keywords color
#' @name chart_theme
#' @concept Themes for R plots with graphics, lattice or ggplot2
#' @examples
#' # TODO..
chart_theme <- function(theme) {
  # TODO... select theme + custom themes + set also lattice and graphics themes
  ggplot2::theme_set(theme_sciviews())
  theme_sciviews_lattice()
  # In latticeExtra, there is ggplot2like theme (theme_gray()):
  # library(latticeExtra)
  # library(grid)
  # opar <- trellis.par.get()
  # trellis.par.set(latticeExtra::ggplot2like(n = 4, h.start = 180))
  # oopt <- lattice.options(latticeExtra::ggplot2like.opts())
  #
  # + show.settings() to be used as show method for a chart_theme(), may be...
}

#' @export
#' @rdname chart_theme
theme_sciviews <- function(font_size = 12, font_family = "", line_size = .5) {
  cowplot::theme_cowplot(font_size = font_size, font_family = font_family,
    line_size = line_size) %+replace%
    theme(
      legend.box.background = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white"),
      panel.border = element_rect(colour = "black", size = 0.5,
        linetype = "solid"),
      panel.grid.major  = element_line(colour = "grey90", size = 0.2),
      panel.grid.minor  = element_line(colour = "grey98", size = 0.5),
      plot.background   = element_rect(fill = "transparent", colour = NA),
      strip.background  = element_rect(fill = "grey80", colour = "black",
        size = 0.5, linetype = "solid"),
      strip.text.x      = element_text(margin =
          margin(0.15 ,0.1, 0.15 ,0.1, "cm")),
      axis.line = element_line(colour = "black", size = 0.5),
      complete = TRUE
    )
}

# Inspired from latticeExtra::ggplot2like() function, but adapted to the
# sciviews theme
.sciviewslike <- function(..., n = 6, h = c(0, 360) + 15, l = 65, c = 100,
h.start = 0, direction = 1, low = "#3B4FB8", high = "#B71B1A", space = "rgb") {

  rotate <- function(x) (x + h.start) %% 360 * direction

  if ((diff(h) %% 360) < 1) {
    h[2] <- h[2] - 360 / n
  }

  colseq <- grDevices::hcl(h = rotate(seq(h[1], h[2], length = n)),
    c = c, l = l)
  ramp <- grDevices::colorRampPalette(c(low, high), space = space,
    interpolate = "linear")(100)

  theme <- latticeExtra::custom.theme(symbol = colseq, fill = colseq,
    region = ramp)
  theme <- modifyList(theme, list(
    scales            = list(alternating = 1),
    layout.heights    = list(bottom.padding = 0.2, top.padding = 0.2,
      strip = 1.2),
    layout.widths     = list(left.padding = 0.4, right.padding = 0.2),
    axis.components   = list(left   = list(pad1 = 0.8, pad2 = 0.8),
                             top    = list(pad1 = 0.8, pad2 = 0.8),
                             bottom = list(pad1 = 0.8, pad2 = 0.8),
                             right  = list(pad1 = 0.8, pad2 = 0.8)),
    axis.line         = list(col = "black", lwd = 1),
    axis.text         = list(cex = 0.85, lineheight = 0.85, col = "black"),
    panel.background  = list(col = "white"),
    reference.line    = list(col = "gray90"),
    strip.background  = list(col = c("grey80", "grey70", "grey60")),
    strip.shingle     = list(col = c("grey60", "grey50", "grey40")),
    strip.border      = list(col = "black", lwd = 0.8),
    add.text          = list(cex = 0.85)
  ))

  theme <- modifyList(theme, list(
    plot.symbol       = list(col = "black", pch = 19, cex = 0.6),
    superpose.symbol  = list(pch = 19, cex = 0.6),
    plot.line         = list(col = "black"),
    plot.polygon      = list(col = "grey20", border = "transparent"),
    superpose.polygon = list(border = "transparent"),
    box.dot           = list(col = "grey20", pch = "|"),
    box.rectangle     = list(fill = "white", col = "grey20"),
    box.umbrella      = list(col = "grey20", lty = 1),
    dot.line          = list(col = "white"),
    dot.symbol        = list(col = "black", pch = 19)
  ))

  modifyList(theme, simpleTheme(...))
}

#' @export
#' @rdname chart_theme
theme_sciviews_lattice <- function(...) {
  opar <- trellis.par.get()
  trellis.par.set(.sciviewslike(...))
  oopt <- lattice.options(.sciviewslike_opts())
  invisible(list(lattice_par = opar, lattice_opt = oopt))
}

#' @export
#' @rdname chart_theme
theme_sciviews_graphics <- function(...) {
  # Set the palette to colors similar to those in ggplot2 and lattice
  # but we keep first (usually 'black'), and last (usually 'gray') colors
  palette(c("black", hue_pal()(6), "gray"))
}

#' @export
#' @rdname chart_theme
theme_svgray <- function() NULL

#' @export
#' @rdname chart_theme
theme_svgray_lattice <- function(...) {
  opar <- trellis.par.get()
  trellis.par.set(latticeExtra::ggplot2like(...))
  oopt <- lattice.options(latticeExtra::ggplot2like.opts())
  invisible(list(lattice_par = opar, lattice_opt = oopt))
}

#' @export
#' @rdname chart_theme
theme_svgray_graphics <- function() NULL

#' @export
#' @rdname chart_theme
theme_svmap <- function() NULL

#' @export
#' @rdname chart_theme
theme_svmap_lattice <- function() {
  opar <- trellis.par.get()
  trellis.par.set(latticeExtra::ggplot2like(n = 4, h.start = 180))
  oopt <- lattice.options(latticeExtra::ggplot2like.opts())
  invisible(list(lattice_par = opar, lattice_opt = oopt))
}

#' @export
#' @rdname chart_theme
theme_svmap_graphics <- function() NULL

#' Viridis color scales
#'
#' ggplot2 version 3 add these function to include viridis color scales. These
#' functions are cloned from ggplot2 v.3.0.0 in order to use the same color
#' scheme with earlier versions of ggplot2. Note, however, that the options
#' `ggplot2.continuous.fill` and `gplot2.continuous.colour` are set to
#' `"viridis"` if not already specified. This changes the default for continuous
#' colors in favor of viridis for both `ggplot()` and `chart()`.
#'
#' @param ... Other arguments passed on to [discrete_scale()] or
#' [continuous_scale()] to control name, limits, breaks, labels and so forth.
#' @param alpha The alpha transparency, a number in `[0,1]`, see argument alpha
#' in hsv.
#' @param begin The (corrected) hue in `[0,1]` at which the viridis colormap
#' begins.
#' @param end The (corrected) hue in `[0,1]` at which the viridis colormap ends.
#' @param direction Sets the order of colors in the scale. If `1`, the default,
#' colors are ordered from darkest to lightest. If `-1`, the order of colors is
#' reversed.
#' @param option A character string indicating the colormap option to use. Four
#' options are available: `"magma"` (or `"A"`), `"inferno"` (or `"B"`),
#' `"plasma"` (or `"C"`), `"viridis"` (or `"D"`, the default option) and
#' `"cividis"` (or `"E"`).
#' @param aesthetics Character string or vector of character strings listing the
#' name(s) of the aesthetic(s) that this scale works with. This can be useful,
#' for example, to apply colour settings to the colour and fill aesthetics at
#' the same time, via `aesthetics = c("colour", "fill")`.
#' @param values If colours should not be evenly positioned along the gradient
#' this vector gives the position (between 0 and 1) for each colour in the
#' colours vector. See rescale() in the scales package for a convience function
#' to map an arbitrary range to between 0 and 1.
#' @param space Colour space in which to calculate gradient. Must be "Lab" -
#' other values are deprecated.
#' @param na.value Missing values will be replaced with this value.
#' @param guide A function used to create a guide or its name. See [guides()]
#' for more info.
#' @param type One of "gradient" or "viridis" indicating the colour scale to
#' use. Can be changed with [options()], and the chart package set it to
#' `"viridis"` in case it is not set yet when the package is attached.
#'
#' @export
#' @seealso [chart()], [chart_theme()]
#' @keywords color
#' @name viridis
#' @concept Viridis colors
#' @examples
#' geyser <- data.io::read("geyser", package = "MASS")
#' chart(data = geyser, waiting ~ duration) +
#'   xlim(0.5, 6) + ylim(40, 110) +
#'   stat_density2d(aes(fill = ..level..), geom = "polygon") +
#'   theme(panel.grid = element_blank()) -> gg
#'
#' combine_charts(
#'   list(
#'     gg + scale_fill_viridis_c(option = "A") + labs(x = "A = magma", y = NULL),
#'     gg + scale_fill_viridis_c(option = "B") + labs(x = "B = inferno", y = NULL),
#'     gg + scale_fill_viridis_c(option = "C") + labs(x = "C = plasma", y = NULL),
#'     gg + scale_fill_viridis_c(option = "D") + labs(x = "D  = viridis", y = NULL),
#'     gg + scale_fill_viridis_c(option = "E") + labs(x = "E = cividis", y = NULL)
#'   ), ncol = 3, nrow = 2
#' )
scale_colour_viridis_c <- function(..., alpha = 1, begin = 0, end = 1,
direction = 1, option = "D", values = NULL, space = "Lab", na.value = "grey50",
guide = "colourbar", aesthetics = "colour") {
  continuous_scale(aesthetics, "viridis_c", scales::gradient_n_pal(
    viridis::viridis_pal(alpha, begin, end, direction, option)(6),
    values, space), na.value = na.value, guide = guide, ...)
}

#' @export
#' @rdname viridis
scale_color_viridis_c <- scale_colour_viridis_c

#' @export
#' @rdname viridis
scale_colour_viridis_d <- function(..., alpha = 1, begin = 0, end = 1,
direction = 1, option = "D", aesthetics = "colour") {
  discrete_scale(aesthetics, "viridis_d", viridis_pal(alpha,
    begin, end, direction, option), ...)
}

#' @export
#' @rdname viridis
scale_color_viridis_d <- scale_colour_viridis_d

#' @export
#' @rdname viridis
scale_fill_viridis_c <- function(..., alpha = 1, begin = 0, end = 1,
direction = 1, option = "D", values = NULL, space = "Lab", na.value = "grey50",
guide = "colourbar", aesthetics = "fill") {
  continuous_scale(aesthetics, "viridis_c", gradient_n_pal(
    viridis_pal(alpha, begin, end, direction, option)(6),
    values, space), na.value = na.value, guide = guide, ...)
}

#' @export
#' @rdname viridis
scale_fill_viridis_d <- function(..., alpha = 1, begin = 0, end = 1,
direction = 1, option = "D", aesthetics = "fill") {
  discrete_scale(aesthetics, "viridis_d", viridis_pal(alpha,
    begin, end, direction, option), ...)
}

#' @export
#' @rdname viridis
scale_colour_continuous <- function(...,
type = getOption("ggplot2.continuous.colour", default = "gradient")) {
  switch(type,
    gradient = scale_colour_gradient(...),
    viridis = scale_colour_viridis_c(...),
    stop("Unknown scale type", call. = FALSE))
}

#' @export
#' @rdname viridis
scale_color_continuous <- scale_colour_continuous

#' @export
#' @rdname viridis
scale_fill_continuous <- function(...,
type = getOption("ggplot2.continuous.fill", default = "gradient")) {
  switch(type,
    gradient = scale_fill_gradient(...),
    viridis = scale_fill_viridis_c(...),
    stop("Unknown scale type", call. = FALSE))
}

.sciviewslike_opts <- function() {
  list(default.args = list(axis = .axis_grid,
    xscale.components = .xscale_components_subticks,
    yscale.components = .yscale_components_subticks,
    between = list(x = 0.4, y = 0.4)))
}

.xscale_components_subticks <- function(lim, ..., n = 5, n2 = n * 2,
min.n2 = n + 5) {
  ans <- xscale.components.default(lim = lim, ..., n = n)
  ans2 <- xscale.components.default(lim = lim, ..., n = n2, min.n = min.n2)
  ticks <- ans$bottom$ticks$at
  ticks2 <- ans2$bottom$ticks$at
  ticks2 <- ticks2[!(ticks2 %in% ticks)]
  ans$bottom$ticks$at <- c(ticks, ticks2)
  ans$bottom$ticks$tck <- c(rep(0.5, length(ticks)), rep(0, length(ticks2)))
  ans$bottom$labels$at <- ans$bottom$ticks$at
  ans$bottom$labels$labels <- c(ans$bottom$labels$labels,
    rep(" ", length(ticks2)))
  ans$bottom$labels$check.overlap <- FALSE
  ans
}

.yscale_components_subticks <- function(lim, ..., n = 5, n2 = n * 2,
  min.n2 = n + 5) {
  ans <- yscale.components.default(lim = lim, ..., n = n)
  ans2 <- yscale.components.default(lim = lim, ..., n = n2, min.n = min.n2)
  ticks <- ans$left$ticks$at
  ticks2 <- ans2$left$ticks$at
  ticks2 <- ticks2[!(ticks2 %in% ticks)]
  ans$left$ticks$at <- c(ticks, ticks2)
  ans$left$ticks$tck <- c(rep(0.5, length(ticks)), rep(0, length(ticks2)))
  ans$left$labels$at <- ans$left$ticks$at
  ans$left$labels$labels <- c(ans$left$labels$labels,
    rep(" ", length(ticks2)))
  ans$left$labels$check.overlap <- FALSE
  ans
}

.axis_grid <- function(side = c("top", "bottom", "left", "right"), ...,
ticks = c("default", "yes", "no"), scales, components, line.col) {
  side <- match.arg(side)
  ticks <- match.arg(ticks)
  scales.tck <- switch(side, left = , bottom = scales$tck[1],
    right = , top = scales$tck[2])
  comps.major <- components
  mycomps <- components[[side]]
  if (is.list(mycomps)) {
    lab <- as.character(mycomps$labels$labels)
    if (any(lab != "")) {
      tck <- mycomps$ticks$tck
      if (any(tck * scales.tck != 0)) {
        tck <- rep(tck, length = length(lab))
        comps.major[[side]]$ticks$tck <- ifelse(lab == "", NA, tck)
      }
    }
  } else {
    ticks <- "no"
  }
  axis.text <- trellis.par.get("axis.text")
  axis.default(side, scales = scales, ticks = ticks, components = comps.major,
    ..., line.col = axis.text$col)
  if (side %in% c("top", "left"))
    return()
  if (scales$draw == FALSE)
    return()
  ref.line <- trellis.par.get("reference.line")
  if (side == "bottom") {
    tck <- abs(mycomps$ticks$tck) + 0.5
    panel.refline(v = mycomps$ticks$at, lwd = ref.line$lwd * tck,
      alpha = ref.line$alpha * tck/max(tck, na.rm = TRUE))
  }
  if (side == "right") {
    if (!is.list(mycomps))
      mycomps <- components[["left"]]
    tck <- abs(mycomps$ticks$tck) + 0.5
    panel.refline(h = mycomps$ticks$at, lwd = ref.line$lwd * tck,
      alpha = ref.line$alpha * tck/max(tck, na.rm = TRUE))
  }
}
