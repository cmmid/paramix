
library(data.table)
library(ggplot2)

.args <- if (interactive()) c(
  file.path("figure", "fig_utilities.rda"),
  list.files("output", "^sim_.*\\.rds$", full.names = TRUE),
  file.path("figure", "incidence.png")
) else commandArgs(trailingOnly = TRUE)

load(.args[1])

ts_dt <- consolidate(.args, "sim_")

age_lowers <- ts_dt[, unique(model_from)]
age_uppers <- age_lowers[-1] - 1L

age_facet_labels <- paste0(age_lowers, c(paste0("-", age_uppers),"+"))
names(age_facet_labels) <- seq_along(age_facet_labels)
iso_labels <- c(AFG = "Afghanistan", GBR = "United Kingdom")

intervention_labels <- unname(age_facet_labels)
intervention_labels[1] <- "Nobody"
names(intervention_labels) <- c("none", paste0("vax_", c("young", "working", "older")))

# in this model, deaths do not affect dynamics, so the method for aggregating
# death parameter (`method` field) is irrelevant
p <- ggplot(ts_dt[method == "f_mean"][between(time, 0, 70)]) + aes(
    x = time, y = value/capita,
    color = intervention, linetype = pathogen
  ) +
  facet_grid(age_group ~ place, labeller = labeller(
    age_group = age_facet_labels, place = iso_labels
  )) +
  geom_line() +
  theme_bw() + theme(element_text(size = 16)) +
  scale_x_continuous(
    "Simulation Time [weeks]", breaks = seq(0, 70, by = 7), labels = \(b) b / 7
  ) +
  scale_y_continuous("Infections\n[incidence per capita]") +
  scale_color_discrete(
    "Vaccinate ...", labels = intervention_labels,
    breaks = names(intervention_labels)
  ) +
  scale_linetype_discrete("Pathogen", labels = c(FLU = "Influenza", SC2 = "SARS-CoV-2"))

ggsave(tail(.args, 1), p, width = 25, height = 14, units = "cm", bg = "white")
