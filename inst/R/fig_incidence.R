
library(data.table)
library(ggplot2)
library(ggh4x)

.args <- if (interactive()) c(
  file.path("figure", "fig_utilities.rda"),
  file.path("output", "incidence.rds"),
  file.path("figure", "incidence.png")
) else commandArgs(trailingOnly = TRUE)

load(.args[1])
ts_dt <- readRDS(.args[2])

# in this model, deaths do not affect dynamics, so the method for aggregating
# death parameter (`method` field) is irrelevant
p <- ggplot(ts_dt[method == "f_mean"][between(time, 0, 15*7)]) + aes(
    x = time, y = value/capita,
    color = intervention, linetype = pathogen
  ) +
  facet_nested("Age Group" + age_group ~ "Setting" + place, labeller = labeller(
    age_group = age_group_labels, place = iso_labels
  )) +
  geom_line() +
  theme_minimal() + theme(
    element_text(size = 16), legend.position = "bottom",
    panel.spacing.x = unit(1.5, "line")
  ) +
  scale_x_simtime() +
  scale_y_continuous("Infections\n[incidence per capita]") +
  scale_color_intervention() +
  scale_linetype_pathogen()

ggsave(tail(.args, 1), p, width = 25, height = 14, units = "cm", bg = "white")
