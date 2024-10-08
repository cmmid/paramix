
library(data.table)
library(ggplot2)
library(ggh4x)

.args <- if (interactive()) c(
  file.path("figure", "fig_utilities.rda"),
  file.path("output", "consolidated.rds"),
  file.path("figure", "yll.png")
) else commandArgs(trailingOnly = TRUE)

load(.args[1])
ylls_dt <- readRDS(.args[2])

base_dt <- ylls_dt[
  intervention == "none", .SD, .SDcols = -c("intervention")
]

int_dt <- ylls_dt[intervention != "none"][
  base_dt, on = setdiff(names(base_dt), "YLL")
]

int_dt[, averted_yll := i.YLL - YLL]

int_dt[, sim_method := factor(sim_method, levels = names(model_assumption_labels), ordered = TRUE)]
int_dt[, method := factor(method, levels = names(model_assumption_labels), ordered = TRUE)]

# in this model, deaths do not affect dynamics, so the method for aggregating
# death parameter (`method` field) is irrelevant
p <- ggplot(int_dt[sim_method == method]) + aes(
  x = method, group = intervention,
  y = averted_yll/1000, fill = intervention, shape = sim_method
) +
  facet_nested(place ~ pathogen, scale = "free_y", labeller = labeller(
    pathogen = pathogen_labels, place = iso_labels
  )) +
  geom_bar(position = 'dodge', stat = 'identity') +
  # geom_point(position = position_dodge(width = 0.3)) +
  theme_bw() + theme(
    element_text(size = 16), legend.position = "right",
    panel.spacing.x = unit(1.5, "line"),
    axis.text.x = element_text(angle = 0)
  ) +
  scale_x_discrete("Distillation Assumption", labels = distill_assumption_labels) +
  # scale_y_log10("Years of life saved (thousands)") +
  scale_y_continuous("Years of life saved (thousands)") +
  scale_color_intervention(
    breaks = rev(names(intervention_labels)) # order by ranking
  ) + scale_shape_discrete("Simulation\nRate Assumption", labels = model_assumption_labels)

ggsave(tail(.args, 1), p, width = 25, height = 14, units = "cm", bg = "white")

