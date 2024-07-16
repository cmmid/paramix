
require(data.table)
require(wpp2022)

.args <- if (interactive()) c(
  "AFG", "GBR",
  file.path("input", "lex.rds")
) else commandArgs(trailingOnly = TRUE)

isos <- head(.args, -1)

dt <- data.table()

dt |> saveRDS(tail(.args, 1))
