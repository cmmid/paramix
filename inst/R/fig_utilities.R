
# the `trap` pattern here let's us source this file interactively
# without having to rm(list = ls()) to avoid leaking unwanted objects into
# our resulting file

.args <- if (interactive()) c(
  file.path("figure", "fig_utilities.rda")
) else commandArgs(trailingOnly = TRUE)

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

  age_labs <- c(
    "0" = '[0, 5)', "5" = '[5, 20)', "20" = '[20, 65)', "65" = '[65, 101)'
  )

  scale_color_agegroup <- function(
    name = "Age group", palette = "Set1", labels = age_labs, ...
  ) {
    scale_color_brewer(name = name, palette = palette, labels = labels, ...)
  }

  save(list = ls(all.names = FALSE), file = .target)

}

trap(.args[1])
