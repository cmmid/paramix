
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

joinvars <- c("method", "place", "pathogen")

base_dt <- ts_dt[
  intervention == "none", .SD, .SDcols = -c("intervention", "capita")
][,.(deaths = sum(deaths)), by = c(joinvars)]

int_dt <- ts_dt[intervention != "none", .(
  deaths = sum(deaths), capita = sum(unique(capita))
), by = c(joinvars, "intervention")][
  base_dt, on = c(joinvars)
]

int_dt[, averted_death := i.deaths - deaths]

int_dt$method <- factor(int_dt$method, levels=unique(int_dt$method))
int_dt$intervention <- factor(int_dt$intervention, levels=unique(int_dt$intervention))

# in this model, deaths do not affect dynamics, so the method for aggregating
# death parameter (`method` field) is irrelevant
p <- ggplot(int_dt[!method=='mean_f']) + aes(
  x = intervention, y = 1000*averted_death/capita, fill = method
) +
  facet_nested(place ~ pathogen, scale = "free_y", labeller = labeller(
    pathogen = pathogen_labels, place = iso_labels
  )) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw() + theme(
    element_text(size = 16), legend.position = "right",
    panel.spacing.x = unit(1.5, "line")
  ) +
  scale_x_discrete("Vaccination age group", labels = intervention_labels) +
  scale_y_continuous("Deaths averted (per 1000 total population)") +
  scale_color_model()

ggsave(tail(.args, 1), p, width = 25, height = 14, units = "cm", bg = "white")
