
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

int_dt[, sim_method := factor(sim_method, levels = c("wm_f", "f_mean", "mean_f"), ordered = TRUE)]
int_dt[, method := factor(method, levels = c("mean_partition", "uniform_model", "alembic_weighted", "proportional_density"), ordered = TRUE)]


# in this model, deaths do not affect dynamics, so the method for aggregating
# death parameter (`method` field) is irrelevant
p <- ggplot(int_dt) + aes(
  x = method, group = sim_method,
  y = averted_yll/capita, color = intervention, shape = sim_method
) +
  facet_nested(place ~ pathogen, scale = "free_y", labeller = labeller(
    pathogen = pathogen_labels, place = iso_labels
  )) +
  geom_point(position = position_dodge(width = 0.3)) +
  theme_bw() + theme(
    element_text(size = 16), legend.position = "right",
    panel.spacing.x = unit(1.5, "line"),
    axis.text.x = element_text(angle = 45)
  ) +
  scale_x_discrete("Distillation Assumption", labels = distill_assumption_labels) +
  scale_y_log10("Years Life Saved\n[person life years]") +
  scale_color_intervention(
    breaks = rev(names(intervention_labels)) # order by ranking
  ) + scale_shape_discrete("Simulation\nRate Assumption", labels = model_assumption_labels)

ggsave(tail(.args, 1), p, width = 25, height = 14, units = "cm", bg = "white")
