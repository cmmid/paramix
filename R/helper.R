
#' @title Interpolation Options
#'
#' @description
#' Creates and interpolation options object for use with [alembic()].
#'
#' @param fun a function
#'
#' @param kind a string; either "point" or "integral". How to interpret the
#' x, y values being interpolated. Either as point observations of a function OR
#' as the integral of the function over the interval.
#'
#' @param ... arbitrary other arguments, but checked against signature of `fun`
#'
#' @details
#' This method creates the interpolation object for use with [alembic()]; this
#' is a convenience method, which does basic validation on arguments and ensures
#' the information used in [alembic()] to do interpolation is available.
#'
#' The `...` arguments will be provided to `fun` when it is invoked to
#' interpolate the tabular "functional" form of arguments to [alembic()]. If
#' `fun` has an argument `kind`, that parameter will also be passed when
#' invoking the function; if not, then the input data will be transformed to
#' \eqn{\{x, z\}} pairs, such that \eqn{x_{i+1}-x_{i} * z_i = y_i} - i.e., transforming to
#' a point value and a functional form which is assumed constant until the next
#' partition.
#'
#' @return a list, with `fun` and `kind` keys, as well as whatever other valid
#' keys appear in `...`.
#'
#' @examples
#' interpolate_opts(
#'   fun = stats::splinefun, method = "natural", kind = "point"
#' )
#' interpolate_opts(
#'   fun = stats::approxfun, method = "constant", yleft = 0, yright = 0,
#'   kind = "integral"
#' )
#' @export
interpolate_opts <- function(fun, kind = c("point", "integral"), ...) {
  if (!is.function(fun)) {
    stop(sprintf("%s is not a function.", deparse(substitute(fun))))
  }

  kind <- match.arg(kind, c("point", "integral"))
  funargs <- formals(fun)
  if (is.null(funargs)) {
    stop("`fun` must have at least one argument.")
  }
  newargs <- list(...)
  # for when interpolate_opts is do.call'd on interpolate_opts output
  newargs$.usekind <- NULL

  baseret <- list(fun = fun, kind = kind, .usekind = "kind" %in% names(funargs))

  if (length(newargs)) {
    if (is.null(names(newargs)) || ("" %in% names(newargs))) {
      stop("If provided via `...` additional new args must be named.")
    }
    if (!("..." %in% names(funargs))) {
      if (length(setdiff(names(newargs), names(funargs))) != 0) {
        stop(sprintf(
          "Provided arguments not present in %s signature: %s (signature arguments are %s)",
          deparse(substitute(fun)),
          toString(setdiff(names(newargs), names(funargs))),
          toString(funargs)
        ))
      }
    }
    if (names(funargs)[1] %in% names(newargs)) {
      stop(sprintf(
        "The first argument to `fun` is where the data will be sent; cannot provide %s via `...`",
        names(funargs)[1]
      ))
    }
    return(c(baseret, newargs))
  } else {
    return(baseret)
  }

}
