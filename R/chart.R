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
#' @param auto.labs Are labels (and units) automatically used for axes?
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
#' urchin <- data.io::read("urchin_bio", package = "data.io", lang = "en")
#'
#' # base R graphics
#' hist(urchin$height)
#' # ... translates to:
#' chart(function() hist(urchin$height))
#' # ... or if the expression is provided directly, specify it is a base chart
#' chart(hist(urchin$height), type = "base")
#' # ... or more concisely:
#' chart$base(hist(urchin$height))
#'
#' # A lattice plot
#' histogram(~ height, data = urchin)
#' chart$histogram(urchin, ~ height)
#'
#' # ggplot2 histogram
#' ggplot(urchin, aes(height)) + geom_histogram()
#' #... or with chart (notice similarities with lattice version)
#' chart(urchin,  ~ height) + geom_histogram()
#' #chart$geom_histogram(urchin,  ~ height)
chart <- structure(function(data, ..., type = NULL, env = parent.frame()) {
  UseMethod("chart")
}, class = c("function", "subsettable_type"))

#' @export
#' @rdname chart
#' @method chart default
chart.default <- function(data, specif = NULL, formula = NULL, mapping = NULL,
..., type = NULL, auto.labs = TRUE, env = parent.frame()) {
  # TODO: add lattice, autoplot and plot methods too!
  force(env)
  if (!is.null(type) && type != "auto") {
    if (type == "base") { # Try using the expression in 'data' for making a plot
      if (is.function(data)) {
        plot_base <- data
      } else {
        plot_base <- function() NULL
        body(plot_base, envir = env) <- substitute(data)
      }
      fun <- function() {
        par(pch = 16, las = 1, cex.axis = 0.8)
        plot_base()
        rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4],
          col = "white") # A trick to get white plot area only (with transp. bg)
        grid(lty = "solid")
        par(new = TRUE)
        plot_base()
      }

      # Customize theme
      opal <- palette()
      opar <- par(no.readonly = TRUE)
      on.exit({palette(opal); par(opar)})
      theme_sciviews_graphics()

      res <- try(as.ggplot(suppressWarnings(fun)), silent = TRUE)
      if (inherits(res, "try-error")) {
        dev.off()
        stop("It seems no plot was generated with this code")
      }
      class(res) <- unique(c("Chart", class(res)))
      return(res)

    } else if (type == "plot") {
      # Use the plot generic function, which is supposed to give a base graphic
      # TODO: change theme, and perhaps, add labels
      fun <- function() {
        par(pch = 16, las = 1, cex.axis = 0.8)
        plot(data, ...)
        rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4],
          col = "white") # A trick to get white plot area only (with transp. bg)
        grid(lty = "solid")
        par(new = TRUE)
        plot(data, ...)
      }
      res <- try(suppressWarnings(as.ggplot(fun)), silent = TRUE)
      if (inherits(res, "try-error")) {
        dev.off()
        stop("It seems no plot was generated with this code")
      }
      class(res) <- unique(c("Chart", class(res)))
      return(res)

    } else if (substring(type, 1, 5) != "geom_") {
      # TODO: theme and labels!

      # Temporary change the theme
      opar <- trellis.par.get()
      theme_sciviews_lattice()
      on.exit(trellis.par.set(theme = opar, strict = 2))

      fun <- get0(type, envir = env, mode = "function")
      if (is.null(fun))
        stop("function '", type, "' not found")
      if (!missing(specif))
        formula <- specif
      # TODO: possibly use formula= instead + specif and data mix
      # TODO: lattice funs are fun(x, data, ...)
      # TODO: apply theme locally only + allow selecting the theme
      theme_sciviews_lattice()
      res <- try(as.ggplot(fun(formula, data = data, ...)), silent = TRUE)
      if (inherits(res, "try-error"))
        stop("no plot was obtained")
      class(res) <- unique(c("Chart", class(res)))
      return(res)
    }
  }

  # This should be a ggplot2 chart
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
    args$type <- NULL
    args$auto.labs <- NULL
    args$env <- NULL
    mapping <- .rename_aes(.f_to_aes(formula, args, with.facets = TRUE))
    # If mapping is provided, use it to append (or replace) formula items
    #    if (!is.null(mapping))
  }

  # Extract facets
  facets <- mapping$facets
  mapping$facets <- NULL
  # Create ggplot object
  # TODO: for the moment, the theme is hardcoded!
  # TODO: for lattice it would be something like:
  # xyplot(..., par.settings = .sciviewslike(n = 6),
  #   lattice.options = .sciviewslike_opts())
  p <- ggplot(data = data, mapping = mapping, environment = env) +
    theme_sciviews()
  # Add facets, if provided
  if (!is_null(facets)) {
    if (is_null(f_lhs(facets))) {# facets like ~var
      p <- p + facet_wrap(facets)
    } else {# facets like var1 ~ var2
      p <- p + facet_grid(facets)
    }
  }
  # If type == "auto", automatically add a layer, like qplot() does
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
      # PhG: got an R CMD Check note that y is not defined!
      #} else if (!'x' %in% mapping_names) {
      #  p <- p + geom_point(aes(x = bquote(seq_along(.(y)))))
      } else {

        # If x is discrete, make a parallel boxplot if enough points or jitter?
        # TODO ...

        p <- p + geom_point()
      }
    } else {# This should be geom_xxx
      fun <- get0(type, envir = env, mode = "function")
      if (is.null(fun))
        stop("function '", type, "' not found")
      # With ggplot2 3.0.0, this does not work any more, due to tidyeval
      p <- p + fun()
      # This does not work too!
      #eval(parse(text = paste0("p <- p + ", type, "()")))
    }
  }

  if (isTRUE(auto.labs)) {
    data <- p$data
    labels <- dnames <- names(data)
    names(labels) <- dnames
    if (!is.null(dnames)) {
      for (dname in dnames)
        labels[[dname]] <- svBase::label(data[[dname]], units = TRUE)
      labels <- labels[labels != ""]
      if (length(labels)) {
        dnames <- names(labels)
        maps <- p$mapping
        if (!is.null(maps)) {
          mnames <- names(maps)
          for (mname in mnames) {
            expr <- maps[[mname]]
            if (is_quosure(expr)) {
              form <- unclass(expr)
            } else {
              form <- asOneSidedFormula(as.character(expr))
            }
            vars <- all.vars(form)
            vars <- vars[vars %in% dnames]
            for (var in vars) {
              expr <- pryr::modify_lang(expr, function(x)
                if (is.name(x) && identical(x, as.name(var)))
                  as.name(labels[[var]]) else x)
            }
            p$labels[[mname]] <- sub("^~", "", gsub("`", "", deparse(expr)))
          }
        }
      }
    }
  }

  if (inherits(p, "ggplot"))
    class(p) <- unique(c("Chart", class(p)))
  p
  #  } else stop("Objects of type ", paste(class(object), collapse = "/"),
  #    " not supported by chart().", call. = FALSE)
}

#' @export
#' @rdname chart
#' @method chart function
chart.function <- function(data, ..., type = NULL, auto.labs = TRUE,
env = parent.frame()) {
  # TODO: inject code for "theming" the base plot here
  # TODO: add labels...
  res <- try(as.ggplot(data), silent = TRUE)
  if (inherits(res, "try-error")) {
    dev.off()
    stop("It seems no plot was generated with this code")
  }
  class(res) <- unique(c("Chart", class(res)))
  res
}

#' Combine charts
#'
#' Assemble multiple charts on the same page. Wrapper around [ggarrange()] with
#' different defaults.
#'
#' @param chartlist List of charts to combine.
#' @param ncol (optional) number of columns in the plot grid.
#' @param nrow (optional) number of rows in the plot grid.
#' @param labels (optional) labels to use for each individual plot. `"AUTO"`
#' @param ... further arguments passed to [ggarrange()].
#' (default value) auto-generates uppercase labels, and `"auto"` does the same
#' for lowercase labels.
#' @return An object of class `ggarrange` containing a list of `ggplot`s.
#' @export
#' @seealso [chart()], [ggarrange()]
#' @keywords hplot
#' @concept Combine plots on the same page
#' @examples
#' # TODO...
combine_charts <- function(chartlist, ncol = NULL, nrow = NULL, labels = "AUTO",
  ...)
  ggpubr::ggarrange(plotlist = chartlist, ncol = ncol, nrow = nrow,
    labels = labels, ...)

#' @export
ggpubr::ggarrange
