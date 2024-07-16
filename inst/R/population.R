
require(data.table)
require(wpp2022)

.args <- if (interactive()) c(
  file.path("input", "population.rds")
) else commandArgs(trailingOnly = TRUE)

dt <- data.table()

dt |> saveRDS(tail(.args, 1))
