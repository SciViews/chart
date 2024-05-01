#' Make a ggplot function pipeable
#'
#' @description The set `Sgg` should be used like this: `Sgg$geom_point()`.
#' It transforms on the fly an original \{ggplot2\} function supposed to be used
#' with the `+` operator (like `p + geom_point()`) into a pipeable version
#' (like `p %>% Sgg$geom_point()`).
#'
#' @param ggplot An object of class "ggplot" (or "theme").
#' @param ... Further arguments passed to the the ggplot function (see Details).
#' @param x The `Sgg()`function.
#' @param name The name of the ggplot function to make pipeable.
#' @param pattern A regular expression to list matching names.
#'
#' @return The `Sgg()` function just returns an error message. When subsetted
#' with the name of a \{ggplot2\} function (e.g., `Sgg$geom_point()`), it
#' returns a modified version of that function in such a way that it can be
#' used with a pipe operator.
#'
#' @details The function returned by `Sgg$fun` is a modified version of the
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
#' (the problem must be reported to the author of `Sgg` and the maintainer of
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
#'   Sgg$geom_point() |>
#'   Sgg$labs(x = "Sepal length (mm)", y = "Petal length (mm)")
#' # Also try completion with Sgg$<tab>
Sgg <- structure(function(ggplot, ...) {
  stop("You must indicate Sgg$<something>(), like Sgg$geom_point() or Sgg$labs()")
}, class = c("subsettable_Sgg", "function"))

.make_pipeable_Sgg <- function(fun) {
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
    "Warning: this is a patched version of a ggplot function to make it pipeable. Please, read ?Sgg first!"
    ggplot + 1
  })
  patched_body[[3]][[3]] <- body(f)
  body(f) <- patched_body
  f
}

#' @export
#' @rdname Sgg
#' @method $ subsettable_Sgg
`$.subsettable_Sgg` <- function(x, name) {
  # We use ggtitle(), but also allow title()
  if (name == "title")
    name <- "ggtitle"
  if (grepl("o__", name)) {
    make_aka <- function(name) {
      fun <- get(name)
      env_label <- rlang::env_label(environment(fun))
      if (grepl("^namespace:", env_label)) {
        alias <- paste(substring(env_label, 11L), name, sep = "::")
      } else {
        alias <- name
      }
      aka(fun, alias = alias)
    }

    construct_section <- function(name, obj, list,
    section = sub("^o__(.+)__$", "\\1", name)) {
      sec <- as.list(c(name = section(obj, section), list))
      names(sec) <- c(names(sec)[1], as.character(sec[-1]))
      for (i in 2:length(sec)) {
        sec[[i]] <- make_aka(sec[[i]])
      }
      sec
    }

    obj <- switch(name,
      o__GEOMETRIES__ = construct_section("o__GEOMETRIES__", obj,
        apropos("^geom_")),
      o__AXES_LABELS__ = construct_section("o__AXES_LABELS__", obj,
        c("expand_limits", "guides", "labs", "lims", "ggtitle",
        "xlab", "xlim", "ylab", "ylim")),
      o__STATS__ = construct_section("o__STATS__", obj,
        apropos("^stat_")),
      o__ANNOTATIONS__ = construct_section("o__ANNOTATIONS__", obj,
        c("annotate", apropos("^annotation_"))),
      o__COORDINATES__ = construct_section("o__COORDINATES__", obj,
        apropos("^coord_")),
      o__FACETS__ = construct_section("o__FACETS__", obj,
        apropos("^facet_")),
      o__SCALES__ = construct_section("o__SCALES__", obj,
        apropos("^scale_")),
      o__THEMES__ = construct_section("o__THEMES__", obj,
        .get_gg_themes()),
      stop("Unknown section ", name)
    )
    obj
  } else {
    .make_pipeable_Sgg(name)
  }
}

.get_gg_themes <- function(pattern = "") {
  themes <- apropos(paste0("^theme_", pattern))
  # Not all themes relate to ggplot2 (e.g., there are also flextable themes)
  check_theme <- function(name) {
    if (name %in% c("theme_get", "theme_set", "theme_update", "theme_replace")) {
      TRUE
    } else {# Check if the theme returns a 'gg' object
      theme_fun <- get(name)
      theme_class <- try(theme_fun(), silent = TRUE)
      inherits(theme_class, "gg")
    }
  }
  is_gg_themes <- logical(length(themes))
  for (i in 1:length(themes)) {
    is_gg_themes[i] <- check_theme(themes[i])
  }
  c("theme", themes[is_gg_themes])
}

#' @export
#' @rdname Sgg
#' @method .DollarNames subsettable_Sgg
.DollarNames.subsettable_Sgg <- function(x, pattern = "") {
  l <- list("o__GEOMETRIES__")
  l <- c(l, apropos(paste0("^geom_", pattern)))
  l <- c(l, "o__AXES_LABELS__")
  l <- c(l, "expand_limits", "guides", "labs", "lims",
    "ggtitle", # No, not any more! Instead of ggtitle (special case!)
    "xlab", "xlim", "ylab", "ylim")
  l <- c(l, "o__STATS__")
  l <- c(l, apropos(paste0("^stat_", pattern)))
  l <- c(l, "o__ANNOTATIONS__")
  l <- c(l, c("annotate", apropos(paste0("^annotation_", pattern))))
  l <- c(l, "o__COORDINATES__")
  l <- c(l, apropos(paste0("^coord_", pattern)))
  l <- c(l, "o__FACETS__")
  l <- c(l, apropos(paste0("^facet_", pattern)))
  l <- c(l, "o__SCALES__")
  l <- c(l, apropos(paste0("^scale_", pattern)))
  l <- c(l, "o__THEMES__")
  l <- c(l, .get_gg_themes())
  l <- grep(pattern = pattern, l, value = TRUE)
  # Add an opening parenthesis to indicate these are functions,
  # except for sections
  is_section <- grepl("^o__", l)
  l[!is_section] <- paste0(l[!is_section], "(")
  #sort(l)
  l
}

# TODO: allow custom extensions to this list
# (trials that do not work for now)
# Sgg_help_handler <- function(type = c("completion", "parameter", "url"),
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
# .DollarNames.subsettable_Sgg <- function(x, pattern = "") {
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
#   #attr(l, "helpHandler") <- Sgg_help_handler
#   l
# }
