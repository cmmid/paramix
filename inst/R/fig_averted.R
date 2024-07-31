
library(data.table)
library(ggplot2)
library(ggh4x)

.args <- if (interactive()) c(
  file.path("figure", "fig_utilities.rda"),
  file.path("output", "incidence.rds"),
  file.path("figure", "averted.png")
) else commandArgs(trailingOnly = TRUE)

load(.args[1])
ts_dt <- readRDS(.args[2])

base_dt <- ts_dt[
  intervention == "none", .SD, .SDcols = -c("intervention", "capita")
]
joinvars <- c("method", "age_group", "time", "model_from", "place", "pathogen")

int_dt <- ts_dt[intervention != "none"][
  base_dt, on = c(joinvars)
]

int_dt[, averted_inf := i.value - value][, averted_death := i.deaths - deaths]

plot_dt <- melt.data.table(
  int_dt, id.vars = c("intervention", "capita", joinvars), measure.vars = c("averted_inf", "averted_death")
)[variable == "averted_death" | method == "f_mean"]

plot_dt[variable == "averted_inf", method := NA]

plot_dt[
  order(time), cvalue:= cumsum(value),
  by = c(setdiff(joinvars, "time"), "variable")
]

tot_dt <- plot_dt[, .(tot = sum(unique(capita))), by = place]

plot_dt <- plot_dt[tot_dt, on = .(place), cpc := cvalue / tot][time == max(time)]

focus_dt <- plot_dt[,
  .(cpc = sum(cpc)), by = .(intervention, place, pathogen, variable, method)
]

# in this model, deaths do not affect dynamics, so the method for aggregating
# death parameter (`method` field) is irrelevant
p <- ggplot(focus_dt[variable != "averted_inf"]) + aes(
  x = method, y = cpc, color = intervention,
  group = method
) +
  facet_nested(place ~ pathogen, scale = "free_y", labeller = labeller(
    pathogen = pathogen_labels, place = iso_labels
  )) +
  geom_point(stat = "identity", position = "stack") +
  theme_bw() + theme(
    element_text(size = 16), legend.position = "right",
    panel.spacing.x = unit(1.5, "line")
  ) +
  scale_x_discrete("Model Assumption", labels = model_assumption_labels) +
  scale_y_continuous("Cumulative Averted\n[incidence per capita]") +
  scale_color_intervention(
    breaks = rev(names(intervention_labels)) # order by ranking
  )

ggsave(tail(.args, 1), p, width = 25, height = 14, units = "cm", bg = "white")
