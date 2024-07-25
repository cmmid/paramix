
library(data.table)
library(ggplot2)

.args <- if (interactive()) c(
  file.path("figure", "fig_utilities.rda"),
  list.files("output", "^sim_.*\\.rds$", full.names = TRUE),
  file.path("figure", "incidence.png")
) else commandArgs(trailingOnly = TRUE)

load(.args[1])

ts_dt <- consolidate(.args, "sim_")

# TODO adjust sim output to be time series
ts_dt[, time := 100]

p <- ggplot(ts_dt[intervention == "none"]) +
  aes(x = time, y = deaths / 1e5, color = factor(age_group)) +
  facet_grid(place ~ pathogen, scales = "free_y") +
  geom_point() + # TODO adjust sim output to be a timeseries
  theme_bw() + theme(element_text(size = 16)) +
  scale_x_continuous("Simulation Time [days]") +
  scale_y_continuous("Infection Incidence [100000 count]") +
  scale_color_agegroup()

ggsave(tail(.args, 1), p, width = 25, height = 14, units = "cm", bg = "white")
