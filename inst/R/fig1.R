
require(data.table)
require(ggplot2)

.args <- if (interactive()) c(
  file.path("output", "consolidated.rds"),
  file.path("figure", "fig1.png")
) else commandArgs(trailingOnly = TRUE)

yll_dt <- readRDS(.args[1])

ref_dt <- yll_dt[intervention == "none", .SD, .SDcols = -c("intervention")]
int_dt <- yll_dt[intervention != "none"][ref_dt, on = .(method, place, pathogen)]
int_dt[, delYLL := i.YLL - YLL]

p <- ggplot(int_dt) + aes(x = method, y = delYLL, color = intervention) +
  facet_grid(place ~ pathogen, scales = "free_y") +
  geom_point(position = position_dodge(width = 0.1)) +
  scale_y_log10("Averted YLL (log10 scale)") +
  theme()

ggsave(tail(.args, 1), p, bg = "white", width = 8, height = 6, units = "in")
