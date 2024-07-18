
require(data.table)
require(ggplot2)

.args <- if (interactive()) c(
  file.path("output", "consolidated.rds"),
  file.path("figure", "fig1.png")
) else commandArgs(trailingOnly = TRUE)

yll_dt <- readRDS(.args[1])

p <- ggplot(yll_dt) + aes(x = intervention, y = YLL, color = method) +
  facet_grid(place ~ pathogen) +
  geom_point(position = position_dodge(width = 0.1)) + theme()

ggsave(tail(.args, 1), p, bg = "white", width = 8, height = 6, units = "in")
