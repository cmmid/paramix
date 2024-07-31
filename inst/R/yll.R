
require(data.table)

.args <- if (interactive()) c(
  file.path("input", "lex.rds"),
  file.path("output", "distill_GBR_SC2.rds"),
  "GBR",
  file.path("output", "yll_GBR_SC2.rds")
) else commandArgs(trailingOnly = TRUE)

lex_dt <- readRDS(.args[1])
lex_dt <- lex_dt[
  iso3 == lex_dt[, match.arg(.args[3], unique(iso3))]
]

distill_dt <- readRDS(.args[2])

halfages <- distill_dt[, unique(partition[partition != round(partition)])]

expander <- data.table(lower = floor(halfages), upper = ceiling(halfages))

lower_dt <- lex_dt[expander, on = .(age = lower), .(index = seq_len(.N), age, ex)]
upper_dt <- lex_dt[expander, on = .(age = upper), .(index = seq_len(.N), age, ex)]

expand_dt <- lower_dt[
  upper_dt, on = .(index), .(age = halfages, ex = (ex + i.ex)/2)
]

ex_dt <- rbind(lex_dt[, .(age, ex)], expand_dt)

yll_dt <- distill_dt[
  ex_dt, on = .(partition = age)
][, .(YLL = sum(value*ex)), keyby = .(sim_method, method, intervention, capita)]

yll_dt |> saveRDS(tail(.args, 1))
