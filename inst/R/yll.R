
require(data.table)

.args <- if (interactive()) c(
  file.path("input", "lex.rds"),
  file.path("output", "distill_GBR_SC2.json"),
  file.path("output", "yll_GBR_SC2.rds")
) else commandArgs(trailingOnly = TRUE)


dt <- data.table()

dt |> saveRDS(tail(.args, 1))
