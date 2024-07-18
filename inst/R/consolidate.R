
require(data.table)

.args <- if (interactive()) c(
  list.files("output", "^yll", full.names = TRUE),
  file.path("output", "consolidated.rds")
) else commandArgs(trailingOnly = TRUE)

yll_dt <- head(.args, -1) |> lapply(readRDS) |> rbindlist(idcol = TRUE)
scenario <- head(.args, -1) |> gsub(".*_(.*)_(.*)\\.rds$", "\\1 \\2", x = _)
place <- gsub("^(.*) .*$", "\\1", scenario)
pathogen <- gsub("^.* (.*)$", "\\1", scenario)
yll_dt[, place := place[.id]]
yll_dt[, pathogen := pathogen[.id]]

yll_dt |> saveRDS(tail(.args, 1))
