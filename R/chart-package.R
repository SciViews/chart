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
#' @importFrom grid grid.convert
NULL


# Non-exported functions --------------------------------------------------

`%is%` <- function(x, what) # This is more expressive!
  inherits(x, what)

is_null <- is.null # rlang::is_null is much slower!

child_env <- function(.parent, ...) {
  # A faster child_env() than rlang::child_env(), but that does not convert
  # .parent and ignores ...
  new.env(parent = .parent)
}

# rlang proposes invoke() in place of do.call(), but it is 100x slower! So:
do_call <- function(what, ...)
  do.call(what, ...)

# rlang::env_parent(env, n = 1) is supposed to replace parent.env(), but it is
# 25x time slower, and we don't need to specify something else than n = 1 here.
# So, we redefine it simply for speed as:
env_parent <- function(env)
  parent.env(env)

# rlang uses ctxt_frame() and call_frame() in place of base::parent.frame() but
# it appears more complex for simple use. Hence call_frame(2)$env is the same as
# parent.frame()... But there is caller_env() as shortcut for the same purpose!

# Again, rlang::is_function is 10x slower than base::is.function(), so:
is_function <- is.function

# The as_character() and as_string() in rlang are difficult to understand. Here
# we simply want a function that (tries) to convert anything into character, as
# as.character() does. So, it is called as_chr()
as_chr <- as.character

# rlang uses env_has() and env_get() in place of exists() and get(), but with
# the environment as first argument (and also cannot specify mode). It can
# extract environments from objects like formulas or quosures, but then, they
# are more than 10x slower than exists() or get() (and get0()). So, for now, I
# stick with exists()/get() in my code...

# Further base/utils functions rename for consistent snake_case notation...
is_chr <- is.character
is_env <- is.environment
stop_if_not <- stopifnot
capture_output <- capture.output
