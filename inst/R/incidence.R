
require(data.table)

.args <- if (interactive()) c(
  list.files("output", "^sim_", full.names = TRUE),
  file.path("output", "incidence.rds")
) else commandArgs(trailingOnly = TRUE)

relargs <- head(.args, -1)

res_dt <- relargs |> lapply(readRDS) |> rbindlist(idcol = "file_index")
pat <- "(.*)_(.*)"
fullpat <- paste0("^.*_", pat, "\\..*$")
file_info <- gsub(fullpat, "\\1_\\2", x = relargs)
res_dt[, `:=`(place, gsub(pat, "\\1", x = file_info[file_index]))]
res_dt[, `:=`(pathogen, gsub(pat, "\\2", x = file_info[file_index]))]
res_dt$file_index <- NULL

res_dt |> saveRDS(tail(.args, 1))
