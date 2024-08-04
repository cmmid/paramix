
require(ggplot2)

# the `trap` pattern here let's us source this file interactively
# without having to rm(list = ls()) to avoid leaking unwanted objects into
# our resulting file

.args <- if (interactive()) c(
  file.path("figure", "fig_utilities.rda")
) else commandArgs(trailingOnly = TRUE)

#' Generic Function Wrapper
#'
#' @description provides a convenience function for
#' producing duplicate functions with different
#' defaults
#'
#' @param FUN the function to wrap
#'
#' @param ... the new defaults
#'
#' @param .ENV the environment for the resulting
#' copy-function (i.e. where any variables will be
#' evaluated). NB, the default (`environment(FUN)`) is
#' mostly convenient, but can be dangerous e.g. by
#' replacing an important function
#'
#'
#' @return the new function
rejig <- function(FUN, ..., .ENV = environment(FUN)) {
  # initial validation
  stopifnot(
    "FUN isn't a function." = is.function(FUN),
    "FUN is a primitive function." = !is.primitive(FUN)
  )

  dots <- as.list(match.call())[-1] # get some new defaults
  dots$FUN <- dots$.ENV <- NULL # drop all the not-defaults

  if (length(dots) == 0) {
    warning("... is empty. Just returning FUN.")
    return(FUN)
  }

  .FUN <- FUN # make a duplicate of FUN
  forms <- formals(FUN) # get the original defaults

  # potentially more validation: check for ... argument
  # in FUN and try to partial match all arguments in
  # rejig
  hasdots <- "..." %in% names(forms)
  replacements <- names(forms)[pmatch(names(dots), names(forms))]

  if (any(is.na(replacements)) && !hasdots) {
    errmsg <- sprintf("
FUN does not have ... argument, and
rejig ... arguments do not match FUN arguments:
%s
", names(dots)[is.na(replacements)] |> paste(collapse = ", ")
    )
    stop(errmsg)
  }

  match.call.defaults <- function(
    definition = sys.function(sys.parent()),
    call = sys.call(sys.parent()),
    expand.dots = TRUE,
    envir = parent.frame(2L)
  ) {
    # get the call
    mc <- match.call(definition, call, expand.dots, envir)
    # get the formals, tossing any ellipsis
    fs <- formals(definition, envir)
    fs$... <- NULL

    # for any arguments set in formals & not in the call
    for(nm in setdiff(names(fs), names(mc)))
      mc[nm] <- fs[nm] # add those to the call

    return(mc)
  }

  # correct any partially matched defaults
  names(dots)[!is.na(replacements)] <- replacements[!is.na(replacements)]
  # set the new defaults
  formals(.FUN)[names(dots)] <- dots
  environment(.FUN) <- .ENV

  if (hasdots && any(is.na(replacements))) {
    # the internals of FUN may pass around the ellipsis, which now
    # excludes newly set default variables, so need to use it
    body(.FUN) <- substitute({
      mc <- match.call.defaults()
      mc[[1]] <- FUN
      eval(mc)
    })
  }

  return(.FUN)

}

trap <- function(.target) {

  #' consolidate single-scenario files into single object
  #'
  #' @param args a `character()`; all the arguments to filter down from for
  #' reading in files
  #'
  #' @param pattern a string; the pattern to match to identify files to keep
  #'
  #' @return a `data.table`
  consolidate <- function(
    args, pattern
  ) {

    # extract the relevant args
    relargs <- grep(pattern, args, value = TRUE)

    # read relevant files and bind them together
    res_dt <- relargs |> lapply(readRDS) |>
      rbindlist(idcol = "file_index")

    # scenario output files all have foo_ISO_PATHOGEN.ext pattern
    pat <- "(.*)_(.*)"
    fullpat <- paste0("^.*_", pat, "\\..*$")
    file_info <- relargs |> gsub(fullpat, "\\1_\\2", x = _)
    res_dt[, place := file_info[file_index] |> gsub(pat, "\\1", x = _)]
    res_dt[, pathogen := file_info[file_index] |> gsub(pat, "\\2", x = _)]
    res_dt$file_index <- NULL

    return(res_dt)
  }

  from_labels <- c(
    "0" = "0-4", "5" = "5-19", "20" = "20-64", "65" = "65+"
  )

  age_group_labels <- from_labels |> setNames(1:4)

  iso_labels <- c(AFG = "LMIC-like", GBR = "HIC-like")

  intervention_labels <- from_labels |>
    setNames(c("none", paste0("vax_", c("young", "working", "older"))))
  intervention_labels[1] <- "Nobody"
  # selected using colorbrewer2.org: https://colorbrewer2.org/#type=sequential&scheme=GnBu&n=5
  intervention_cols <- c("#0868ac", "#43a2ca", "#7bccc4", "#bae4bc") |>
    setNames(names(intervention_labels))

  pathogen_labels <- c(FLU = "Flu-like", SC2 = "COVID-like")

  model_assumption_labels <- c(
    f_mid = "IFR(mid(Age))", f_mean = "IFR(E[Age])", mean_f = "E[IFR(Age)]", wm_f = "paramix"
  )

  distill_assumption_labels <- c(
    "Uniform Across Partition", "@ Mean Age",
    "Prop. to Pop. Density", "paramix"
  ) |> setNames(names(model_assumption_labels))

  scale_color_intervention <- rejig(
    scale_color_manual, name = "Vaccinate...",
    breaks = names(intervention_labels), labels = intervention_labels,
    values = intervention_cols,
    aesthetics = c("color", "fill")
  )

  scale_x_simtime <- rejig(
    scale_x_continuous, name = "Simulation Time [weeks]",
    breaks = \(lims) seq(0, lims[2], by = 7), labels = \(b) b / 7,
    expand = expansion()
  )

  scale_linetype_pathogen <- rejig(
    scale_linetype_discrete,
    name = "Pathogen", labels = pathogen_labels
  )

  facet_iso <- rejig(
    facet_grid, cols = vars(iso3),
    labeller = ggplot2::labeller(iso3 = iso_labels)
  )

  save(list = ls(all.names = FALSE), file = .target)

}

trap(.args[1])
